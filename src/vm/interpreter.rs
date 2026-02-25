use crate::backend::JitSession;
#[cfg(test)]
use crate::ir::bytecode::Opcode;
use crate::ir::bytecode::{BytecodeChunk, ConstEntry};
use crate::runtime::builtins;
use crate::runtime::{Heap, Value};
use std::sync::atomic::{AtomicBool, Ordering};

use super::calls::{execute_builtin, pop_args, read_i16, read_u8, setup_callee_locals};
use super::ops::{
    add_values, div_values, gt_values, gte_values, instanceof_check, is_nullish, is_truthy,
    lt_values, lte_values, mod_values, mul_values, pow_values, strict_eq, sub_values,
    value_to_prop_key,
};
use super::props::{resolve_get_prop, GetPropCache};
use super::types::{BuiltinResult, Completion, Program, VmError, MAX_CALL_DEPTH};

struct Frame {
    chunk_index: usize,
    is_dynamic: bool,
    pc: usize,
    locals: Vec<Value>,
    this_value: Value,
    rethrow_after_finally: bool,
    new_object: Option<usize>,
}

/// Mutable execution state for one run. Shared boundary for interpreter and (future) JIT:
/// same heap + program + state so JIT can run a chunk and return into this state.
struct RunState {
    stack: Vec<Value>,
    frames: Vec<Frame>,
    dynamic_chunks: Vec<BytecodeChunk>,
    dynamic_captures: Vec<Vec<(u32, Value)>>,
    chunks_stack: Vec<BytecodeChunk>,
    getprop_cache: GetPropCache,
    jit: Option<JitSession>,
    call_hot_counts: Vec<u32>,
}

const JIT_HOT_CALL_THRESHOLD: u32 = 32;
const CHECK_INTERVAL_MASK: u32 = CHECK_INTERVAL - 1;

pub fn interpret(chunk: &BytecodeChunk) -> Result<Completion, VmError> {
    let program = Program {
        chunks: vec![chunk.clone()],
        entry: 0,
        init_entry: None,
        global_funcs: Vec::new(),
    };
    interpret_program(&program)
}

pub fn interpret_program(program: &Program) -> Result<Completion, VmError> {
    interpret_program_with_trace(program, false)
}

pub fn interpret_program_with_limit(
    program: &Program,
    trace: bool,
    step_limit: Option<u64>,
) -> Result<Completion, VmError> {
    let (result, _) =
        interpret_program_with_trace_and_limit(program, trace, step_limit, None, false);
    result
}

pub fn interpret_program_with_limit_and_cancel(
    program: &Program,
    trace: bool,
    step_limit: Option<u64>,
    cancel: Option<&AtomicBool>,
    test262_mode: bool,
) -> (Result<Completion, VmError>, Heap) {
    interpret_program_with_trace_and_limit(program, trace, step_limit, cancel, test262_mode)
}

pub fn interpret_program_with_trace(program: &Program, trace: bool) -> Result<Completion, VmError> {
    let (result, _) = interpret_program_with_trace_and_limit(program, trace, None, None, false);
    result
}

#[cold]
fn trace_op(pc: usize, op: u8) {
    let opname = crate::ir::disasm::opcode_name(op);
    eprintln!("  {:04}  {}", pc, opname);
}

#[inline(always)]
fn jit_i64_to_value(result: i64) -> Value {
    Value::Int(result.clamp(i32::MIN as i64, i32::MAX as i64) as i32)
}

fn interpret_program_with_trace_and_limit(
    program: &Program,
    trace: bool,
    step_limit: Option<u64>,
    cancel: Option<&AtomicBool>,
    test262_mode: bool,
) -> (Result<Completion, VmError>, Heap) {
    let mut heap = Heap::new();
    if test262_mode {
        heap.init_test262_globals();
    }
    let global_id = heap.global_object();
    for (name, chunk_idx) in &program.global_funcs {
        if *chunk_idx < program.chunks.len() {
            heap.set_prop(global_id, name, Value::Function(*chunk_idx));
        }
    }
    let result = interpret_program_with_heap(program, &mut heap, trace, step_limit, cancel);
    (result, heap)
}

pub fn interpret_program_with_heap(
    program: &Program,
    heap: &mut Heap,
    trace: bool,
    step_limit: Option<u64>,
    cancel: Option<&AtomicBool>,
) -> Result<Completion, VmError> {
    if let Some(init_idx) = program.init_entry {
        interpret_program_with_heap_and_entry(program, heap, init_idx, trace, step_limit, cancel)?;
    }
    interpret_program_with_heap_and_entry(program, heap, program.entry, trace, step_limit, cancel)
}

/// Throttle (cancel/step check) runs only when timeout or step limit is in use.
/// Unthrottled runs have no extra branches in the hot path for low latency and JIT-friendly execution.
const CHECK_INTERVAL: u32 = 256;

pub fn interpret_program_with_heap_and_entry(
    program: &Program,
    heap: &mut Heap,
    entry: usize,
    trace: bool,
    step_limit: Option<u64>,
    cancel: Option<&AtomicBool>,
) -> Result<Completion, VmError> {
    let needs_throttle = cancel.is_some() || step_limit.is_some();
    let mut steps_remaining = step_limit;
    let entry_chunk = program
        .chunks
        .get(entry)
        .ok_or(VmError::InvalidConstIndex(entry))?;
    let mut state = RunState {
        stack: Vec::with_capacity(256),
        frames: vec![Frame {
            chunk_index: entry,
            is_dynamic: false,
            pc: 0,
            locals: vec![Value::Undefined; entry_chunk.num_locals as usize],
            this_value: Value::Undefined,
            rethrow_after_finally: false,
            new_object: None,
        }],
        dynamic_chunks: Vec::new(),
        dynamic_captures: Vec::new(),
        chunks_stack: Vec::new(),
        getprop_cache: GetPropCache::new(),
        jit: if trace || step_limit.is_some() || cancel.is_some() {
            None
        } else {
            Some(JitSession::new())
        },
        call_hot_counts: vec![0; program.chunks.len()],
    };

    let mut loop_counter: u32 = 0;

    loop {
        if needs_throttle {
            loop_counter = loop_counter.wrapping_add(1);
            if (loop_counter & CHECK_INTERVAL_MASK) == 0 {
                if let Some(c) = cancel {
                    if c.load(Ordering::Relaxed) {
                        return Err(VmError::Cancelled);
                    }
                }
            }
            if let Some(ref mut n) = steps_remaining {
                if *n == 0 {
                    return Err(VmError::StepLimitExceeded);
                }
                *n -= 1;
            }
        }

        let frame = state.frames.last_mut().ok_or(VmError::StackUnderflow)?;
        let chunk = if frame.is_dynamic {
            state
                .chunks_stack
                .get(frame.chunk_index)
                .ok_or(VmError::InvalidConstIndex(frame.chunk_index))?
        } else {
            program
                .chunks
                .get(frame.chunk_index)
                .ok_or(VmError::InvalidConstIndex(frame.chunk_index))?
        };
        let code = &chunk.code;
        let constants = &chunk.constants;
        let locals = &mut frame.locals;
        let pc = &mut frame.pc;

        if *pc >= code.len() {
            break;
        }

        let op = read_u8(code, *pc);
        let trace_pc = *pc;
        *pc += 1;

        if trace {
            trace_op(trace_pc, op);
        }

        match op {
            // ---- Stack / Locals ----
            0x01 => {
                let idx = read_u8(code, *pc) as usize;
                *pc += 1;
                let val = match constants.get(idx).ok_or(VmError::InvalidConstIndex(idx))? {
                    ConstEntry::Global(name) => heap.get_global(name),
                    ConstEntry::Function(func_idx) => {
                        let callee_chunk = program
                            .chunks
                            .get(*func_idx)
                            .ok_or(VmError::InvalidConstIndex(*func_idx))?;
                        if callee_chunk.captured_names.is_empty() {
                            Value::Function(*func_idx)
                        } else {
                            let mut captured_slots: Vec<(u32, Value)> = Vec::new();
                            for capture_name in &callee_chunk.captured_names {
                                let outer_slot = chunk
                                    .named_locals
                                    .iter()
                                    .find_map(|(name, slot)| {
                                        (name == capture_name).then_some(*slot)
                                    })
                                    .map(|slot| slot as usize);
                                let inner_slot =
                                    callee_chunk.named_locals.iter().find_map(|(name, slot)| {
                                        (name == capture_name).then_some(*slot)
                                    });
                                if let Some(inner_slot) = inner_slot {
                                    let captured_value = outer_slot
                                        .and_then(|slot| locals.get(slot))
                                        .cloned()
                                        .unwrap_or(Value::Undefined);
                                    captured_slots.push((inner_slot, captured_value));
                                }
                            }
                            let dynamic_index = state.dynamic_chunks.len();
                            state.dynamic_chunks.push(callee_chunk.clone());
                            if state.dynamic_captures.len() <= dynamic_index {
                                state.dynamic_captures.resize(dynamic_index + 1, Vec::new());
                            }
                            state.dynamic_captures[dynamic_index] = captured_slots;
                            Value::DynamicFunction(dynamic_index)
                        }
                    }
                    c => c.to_value(),
                };
                state.stack.push(val);
            }
            0x02 => {
                state.stack.pop().ok_or(VmError::StackUnderflow)?;
            }
            0x03 => {
                let slot = read_u8(code, *pc) as usize;
                *pc += 1;
                let val = locals.get(slot).cloned().unwrap_or(Value::Undefined);
                state.stack.push(val);
            }
            0x04 => {
                let slot = read_u8(code, *pc) as usize;
                *pc += 1;
                let val = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                if slot < locals.len() {
                    locals[slot] = val;
                }
            }
            0x05 => {
                state.stack.push(frame.this_value.clone());
            }
            0x06 => {
                let top = state.stack.last().cloned().ok_or(VmError::StackUnderflow)?;
                state.stack.push(top);
            }
            0x07 => {
                let b = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let a = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(b);
                state.stack.push(a);
            }

            // ---- Arithmetic ----
            0x10 => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(match (&lhs, &rhs) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a.saturating_add(*b)),
                    _ => add_values(&lhs, &rhs),
                });
            }
            0x11 => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(match (&lhs, &rhs) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a.saturating_sub(*b)),
                    _ => sub_values(&lhs, &rhs),
                });
            }
            0x12 => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(match (&lhs, &rhs) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a.saturating_mul(*b)),
                    _ => mul_values(&lhs, &rhs),
                });
            }
            0x13 => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(div_values(&lhs, &rhs));
            }
            0x14 => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(match (&lhs, &rhs) {
                    (Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
                    _ => lt_values(&lhs, &rhs),
                });
            }
            0x15 => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(mod_values(&lhs, &rhs));
            }
            0x16 => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(pow_values(&lhs, &rhs));
            }

            // ---- Comparison / Equality ----
            0x17 => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(Value::Bool(strict_eq(&lhs, &rhs)));
            }
            0x18 => {
                let val = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(Value::Bool(!is_truthy(&val)));
            }
            0x19 => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(lte_values(&lhs, &rhs));
            }
            0x1a => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(gt_values(&lhs, &rhs));
            }
            0x1b => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(gte_values(&lhs, &rhs));
            }
            0x1c => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(Value::Bool(!strict_eq(&lhs, &rhs)));
            }
            0x1d => {
                let val = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let s = match &val {
                    Value::Undefined => "undefined",
                    Value::Null => "object",
                    Value::Bool(_) => "boolean",
                    Value::Int(_) | Value::Number(_) => "number",
                    Value::String(_) => "string",
                    Value::Symbol(_) => "symbol",
                    Value::Object(_)
                    | Value::Array(_)
                    | Value::Map(_)
                    | Value::Set(_)
                    | Value::Date(_) => "object",
                    Value::Function(_) | Value::DynamicFunction(_) | Value::Builtin(_) => {
                        "function"
                    }
                };
                state.stack.push(Value::String(s.to_string()));
            }

            // ---- Bitwise ----
            0x1e => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(Value::Int(lhs.to_i32() << rhs.to_i32()));
            }
            0x1f => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(Value::Int(lhs.to_i32() >> rhs.to_i32()));
            }
            0x23 => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(Value::Int(
                    (lhs.to_i32() as u32 >> rhs.to_i32() as u32) as i32,
                ));
            }
            0x24 => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(Value::Int(lhs.to_i32() & rhs.to_i32()));
            }
            0x25 => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(Value::Int(lhs.to_i32() | rhs.to_i32()));
            }
            0x26 => {
                let rhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(Value::Int(lhs.to_i32() ^ rhs.to_i32()));
            }
            0x27 => {
                let val = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state.stack.push(Value::Int(!val.to_i32()));
            }
            0x28 => {
                let constructor = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let value = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                state
                    .stack
                    .push(Value::Bool(instanceof_check(&value, &constructor, heap)));
            }
            0x29 => {
                let key = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let obj_val = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let key_str = value_to_prop_key(&key);
                let result = match &obj_val {
                    Value::Object(id) => {
                        heap.delete_prop(*id, &key_str);
                        true
                    }
                    Value::Array(id) => {
                        if key_str == "length" {
                            false
                        } else {
                            heap.delete_array_prop(*id, &key_str);
                            true
                        }
                    }
                    Value::Function(function_index) => {
                        heap.delete_function_prop(*function_index, &key_str);
                        true
                    }
                    _ => true,
                };
                state.stack.push(Value::Bool(result));
            }

            // ---- Control flow: Return / Throw / Finally ----
            0x20 => {
                let val = state.stack.pop().unwrap_or(Value::Undefined);
                let popped = state.frames.pop();
                if let Some(ref f) = popped {
                    if f.is_dynamic {
                        state.chunks_stack.pop();
                    }
                }
                let result = if let Some(ref f) = popped {
                    if let Some(obj_id) = f.new_object {
                        if matches!(val, Value::Object(_)) {
                            val
                        } else {
                            Value::Object(obj_id)
                        }
                    } else {
                        val
                    }
                } else {
                    val
                };
                if state.frames.is_empty() {
                    return Ok(Completion::Return(result));
                }
                state.stack.push(result);
            }
            0x21 => {
                let val = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let throw_pc = *pc - 1;
                if let Some((hpc, slot, is_fin)) = find_handler(chunk, throw_pc) {
                    if slot < locals.len() {
                        locals[slot] = val.clone();
                    }
                    frame.rethrow_after_finally = is_fin;
                    *pc = hpc;
                } else {
                    return Ok(Completion::Throw(val));
                }
            }
            0x22 => {
                let slot = read_u8(code, *pc) as usize;
                *pc += 1;
                if frame.rethrow_after_finally {
                    frame.rethrow_after_finally = false;
                    let val = locals.get(slot).cloned().unwrap_or(Value::Undefined);
                    return Ok(Completion::Throw(val));
                }
            }

            // ---- Jumps ----
            0x30 => {
                let offset = read_i16(code, *pc) as isize;
                *pc += 2;
                let val = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                if !is_truthy(&val) {
                    *pc = (*pc as isize + offset) as usize;
                }
            }
            0x31 => {
                let offset = read_i16(code, *pc) as isize;
                *pc += 2;
                *pc = (*pc as isize + offset) as usize;
            }
            0x32 => {
                let offset = read_i16(code, *pc) as isize;
                *pc += 2;
                let val = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                if is_nullish(&val) {
                    *pc = (*pc as isize + offset) as usize;
                }
            }

            // ---- Call (static target) ----
            0x40 => {
                let func_idx = read_u8(code, *pc) as usize;
                let argc = read_u8(code, *pc + 1) as usize;
                *pc += 2;
                let callee = program
                    .chunks
                    .get(func_idx)
                    .ok_or(VmError::InvalidConstIndex(func_idx))?;
                let args = pop_args(&mut state.stack, argc)?;

                if let Some(jit) = state.jit.as_ref()
                    && let Some(result) = jit.try_invoke_compiled(func_idx)
                {
                    state.stack.push(jit_i64_to_value(result));
                    continue;
                }

                if let Some(hot_count) = state.call_hot_counts.get_mut(func_idx) {
                    *hot_count = hot_count.saturating_add(1);
                    if *hot_count >= JIT_HOT_CALL_THRESHOLD {
                        if let Some(jit) = state.jit.as_mut() {
                            match jit.try_compile(func_idx, callee) {
                                Ok(Some(result)) => {
                                    state.stack.push(jit_i64_to_value(result));
                                    continue;
                                }
                                Ok(None) | Err(_) => {}
                            }
                        }
                    }
                }

                let callee_locals = setup_callee_locals(callee, &args, heap);
                if state.frames.len() >= MAX_CALL_DEPTH {
                    return Err(VmError::CallDepthExceeded);
                }
                state.frames.push(Frame {
                    chunk_index: func_idx,
                    is_dynamic: false,
                    pc: 0,
                    locals: callee_locals,
                    this_value: Value::Undefined,
                    rethrow_after_finally: false,
                    new_object: None,
                });
            }

            // ---- CallBuiltin ----
            0x41 => {
                let builtin_id = read_u8(code, *pc);
                let argc = read_u8(code, *pc + 1) as usize;
                *pc += 2;
                let call_pc = *pc - 3;
                let mut ctx = builtins::BuiltinContext {
                    heap,
                    dynamic_chunks: &mut state.dynamic_chunks,
                };
                match execute_builtin(builtin_id, argc, &mut state.stack, &mut ctx) {
                    Ok(BuiltinResult::Push(v)) => {
                        state.getprop_cache.invalidate_all();
                        state.stack.push(v);
                    }
                    Ok(BuiltinResult::Throw(v)) => {
                        if let Some((hpc, slot, is_fin)) = find_handler(chunk, call_pc) {
                            if slot < locals.len() {
                                locals[slot] = v.clone();
                            }
                            frame.rethrow_after_finally = is_fin;
                            *pc = hpc;
                        } else {
                            return Ok(Completion::Throw(v));
                        }
                    }
                    Err(e) => return Err(e),
                }
            }

            // ---- CallMethod (dynamic target) ----
            0x42 => {
                let argc = read_u8(code, *pc) as usize;
                *pc += 1;
                let call_pc = *pc - 2;
                let args = pop_args(&mut state.stack, argc)?;
                let callee = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let receiver = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                match callee {
                    Value::Builtin(builtin_id) => {
                        for a in &args {
                            state.stack.push(a.clone());
                        }
                        state.stack.push(receiver);
                        let mut ctx = builtins::BuiltinContext {
                            heap,
                            dynamic_chunks: &mut state.dynamic_chunks,
                        };
                        match execute_builtin(builtin_id, argc + 1, &mut state.stack, &mut ctx) {
                            Ok(BuiltinResult::Push(v)) => {
                                state.getprop_cache.invalidate_all();
                                state.stack.push(v);
                            }
                            Ok(BuiltinResult::Throw(v)) => {
                                if let Some((hpc, slot, is_fin)) = find_handler(chunk, call_pc) {
                                    if slot < locals.len() {
                                        locals[slot] = v.clone();
                                    }
                                    frame.rethrow_after_finally = is_fin;
                                    *pc = hpc;
                                } else {
                                    return Ok(Completion::Throw(v));
                                }
                            }
                            Err(e) => return Err(e),
                        }
                    }
                    Value::DynamicFunction(heap_idx) => {
                        let callee_chunk = state
                            .dynamic_chunks
                            .get(heap_idx)
                            .ok_or(VmError::InvalidConstIndex(heap_idx))?
                            .clone();
                        let mut callee_locals = setup_callee_locals(&callee_chunk, &args, heap);
                        if let Some(captured) = state.dynamic_captures.get(heap_idx) {
                            for (slot, value) in captured {
                                let slot = *slot as usize;
                                if slot < callee_locals.len() {
                                    callee_locals[slot] = value.clone();
                                }
                            }
                        }
                        if state.frames.len() >= MAX_CALL_DEPTH {
                            return Err(VmError::CallDepthExceeded);
                        }
                        state.chunks_stack.push(callee_chunk);
                        state.frames.push(Frame {
                            chunk_index: state.chunks_stack.len() - 1,
                            is_dynamic: true,
                            pc: 0,
                            locals: callee_locals,
                            this_value: receiver,
                            rethrow_after_finally: false,
                            new_object: None,
                        });
                    }
                    Value::Function(func_idx) => {
                        let callee_chunk = program
                            .chunks
                            .get(func_idx)
                            .ok_or(VmError::InvalidConstIndex(func_idx))?;

                        if let Some(jit) = state.jit.as_ref()
                            && let Some(result) = jit.try_invoke_compiled(func_idx)
                        {
                            state.stack.push(jit_i64_to_value(result));
                            continue;
                        }

                        if let Some(hot_count) = state.call_hot_counts.get_mut(func_idx) {
                            *hot_count = hot_count.saturating_add(1);
                            if *hot_count >= JIT_HOT_CALL_THRESHOLD {
                                if let Some(jit) = state.jit.as_mut() {
                                    match jit.try_compile(func_idx, callee_chunk) {
                                        Ok(Some(result)) => {
                                            state.stack.push(jit_i64_to_value(result));
                                            continue;
                                        }
                                        Ok(None) | Err(_) => {}
                                    }
                                }
                            }
                        }

                        let callee_locals = setup_callee_locals(callee_chunk, &args, heap);
                        if state.frames.len() >= MAX_CALL_DEPTH {
                            return Err(VmError::CallDepthExceeded);
                        }
                        state.frames.push(Frame {
                            chunk_index: func_idx,
                            is_dynamic: false,
                            pc: 0,
                            locals: callee_locals,
                            this_value: receiver,
                            rethrow_after_finally: false,
                            new_object: None,
                        });
                    }
                    _ => {
                        let msg = format!(
                            "TypeError: callee is not a function (got {})",
                            callee.type_name_for_error(),
                        );
                        return Ok(Completion::Throw(Value::String(msg)));
                    }
                }
            }

            // ---- NewCall (static target) ----
            0x43 => {
                let func_idx = read_u8(code, *pc) as usize;
                let argc = read_u8(code, *pc + 1) as usize;
                *pc += 2;
                let callee = program
                    .chunks
                    .get(func_idx)
                    .ok_or(VmError::InvalidConstIndex(func_idx))?;
                let obj_id = heap.alloc_object();
                let args = pop_args(&mut state.stack, argc)?;
                let callee_locals = setup_callee_locals(callee, &args, heap);
                if state.frames.len() >= MAX_CALL_DEPTH {
                    return Err(VmError::CallDepthExceeded);
                }
                state.frames.push(Frame {
                    chunk_index: func_idx,
                    is_dynamic: false,
                    pc: 0,
                    locals: callee_locals,
                    this_value: Value::Object(obj_id),
                    rethrow_after_finally: false,
                    new_object: Some(obj_id),
                });
            }

            // ---- NewMethod (dynamic target) ----
            0x44 => {
                let argc = read_u8(code, *pc) as usize;
                *pc += 1;
                let args = pop_args(&mut state.stack, argc)?;
                let callee = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let obj_id = heap.alloc_object();
                let receiver = Value::Object(obj_id);
                match callee {
                    Value::Builtin(builtin_id) => {
                        for a in &args {
                            state.stack.push(a.clone());
                        }
                        state.stack.push(receiver);
                        let mut ctx = builtins::BuiltinContext {
                            heap,
                            dynamic_chunks: &mut state.dynamic_chunks,
                        };
                        match execute_builtin(builtin_id, argc + 1, &mut state.stack, &mut ctx) {
                            Ok(BuiltinResult::Push(v)) => {
                                state.getprop_cache.invalidate_all();
                                state.stack.push(v);
                            }
                            Ok(BuiltinResult::Throw(v)) => {
                                return Ok(Completion::Throw(v));
                            }
                            Err(e) => return Err(e),
                        }
                    }
                    Value::DynamicFunction(heap_idx) => {
                        let callee_chunk = state
                            .dynamic_chunks
                            .get(heap_idx)
                            .ok_or(VmError::InvalidConstIndex(heap_idx))?
                            .clone();
                        let mut callee_locals = setup_callee_locals(&callee_chunk, &args, heap);
                        if let Some(captured) = state.dynamic_captures.get(heap_idx) {
                            for (slot, value) in captured {
                                let slot = *slot as usize;
                                if slot < callee_locals.len() {
                                    callee_locals[slot] = value.clone();
                                }
                            }
                        }
                        if state.frames.len() >= MAX_CALL_DEPTH {
                            return Err(VmError::CallDepthExceeded);
                        }
                        state.chunks_stack.push(callee_chunk);
                        state.frames.push(Frame {
                            chunk_index: state.chunks_stack.len() - 1,
                            is_dynamic: true,
                            pc: 0,
                            locals: callee_locals,
                            this_value: receiver,
                            rethrow_after_finally: false,
                            new_object: Some(obj_id),
                        });
                    }
                    Value::Function(func_idx) => {
                        let callee_chunk = program
                            .chunks
                            .get(func_idx)
                            .ok_or(VmError::InvalidConstIndex(func_idx))?;
                        let callee_locals = setup_callee_locals(callee_chunk, &args, heap);
                        if state.frames.len() >= MAX_CALL_DEPTH {
                            return Err(VmError::CallDepthExceeded);
                        }
                        state.frames.push(Frame {
                            chunk_index: func_idx,
                            is_dynamic: false,
                            pc: 0,
                            locals: callee_locals,
                            this_value: receiver,
                            rethrow_after_finally: false,
                            new_object: Some(obj_id),
                        });
                    }
                    _ => {
                        let msg = format!(
                            "TypeError: callee is not a function (got {})",
                            callee.type_name_for_error(),
                        );
                        return Ok(Completion::Throw(Value::String(msg)));
                    }
                }
            }

            // ---- Objects / Arrays / Properties ----
            0x50 => {
                state.stack.push(Value::Object(heap.alloc_object()));
            }
            0x51 => {
                state.stack.push(Value::Array(heap.alloc_array()));
            }
            0x52 => {
                let key_idx = read_u8(code, *pc) as usize;
                *pc += 1;
                let obj = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let key_str = match constants
                    .get(key_idx)
                    .ok_or(VmError::InvalidConstIndex(key_idx))?
                {
                    ConstEntry::String(s) => s.clone(),
                    ConstEntry::Int(n) => n.to_string(),
                    _ => return Err(VmError::InvalidConstIndex(key_idx)),
                };
                let result = resolve_get_prop(&obj, &key_str, Some(&mut state.getprop_cache), heap);
                state.stack.push(result);
            }
            0x53 => {
                let key_idx = read_u8(code, *pc) as usize;
                *pc += 1;
                let obj = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let value = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let key_str = match constants
                    .get(key_idx)
                    .ok_or(VmError::InvalidConstIndex(key_idx))?
                {
                    ConstEntry::String(s) => s.clone(),
                    ConstEntry::Int(n) => n.to_string(),
                    _ => return Err(VmError::InvalidConstIndex(key_idx)),
                };
                match &obj {
                    Value::Object(id) => {
                        state.getprop_cache.invalidate(*id, false, &key_str);
                        heap.set_prop(*id, &key_str, value.clone());
                    }
                    Value::Array(id) => {
                        state.getprop_cache.invalidate(*id, true, &key_str);
                        heap.set_array_prop(*id, &key_str, value.clone());
                    }
                    Value::Map(id) => heap.map_set(*id, &key_str, value.clone()),
                    Value::Function(i) => heap.set_function_prop(*i, &key_str, value.clone()),
                    _ => {}
                }
                state.stack.push(value);
            }
            0x54 => {
                let key = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let obj = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let key_str = value_to_prop_key(&key);
                let result = resolve_get_prop(&obj, &key_str, None, heap);
                state.stack.push(result);
            }
            0x55 => {
                let value = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let key = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let obj = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let key_str = value_to_prop_key(&key);
                match &obj {
                    Value::Object(id) => heap.set_prop(*id, &key_str, value.clone()),
                    Value::Array(id) => heap.set_array_prop(*id, &key_str, value.clone()),
                    Value::Map(id) => heap.map_set(*id, &key_str, value.clone()),
                    Value::Function(i) => heap.set_function_prop(*i, &key_str, value.clone()),
                    _ => {}
                }
                state.stack.push(value);
            }
            0x56 => {
                let proto = state.stack.pop().ok_or(VmError::StackUnderflow)?;
                let prototype = match &proto {
                    Value::Null | Value::Undefined => None,
                    Value::Object(id) => Some(*id),
                    _ => None,
                };
                state
                    .stack
                    .push(Value::Object(heap.alloc_object_with_prototype(prototype)));
            }

            _ => return Err(VmError::InvalidOpcode(op)),
        }
    }

    let result = state.stack.pop().unwrap_or(Value::Undefined);
    Ok(Completion::Normal(result))
}

/// Finds the innermost exception handler covering `throw_pc`.
/// Returns (handler_pc, catch_slot, is_finally).
#[inline]
fn find_handler(chunk: &BytecodeChunk, throw_pc: usize) -> Option<(usize, usize, bool)> {
    chunk
        .handlers
        .iter()
        .find(|h| (h.try_start as usize) <= throw_pc && throw_pc < (h.try_end as usize))
        .map(|h| (h.handler_pc as usize, h.catch_slot as usize, h.is_finally))
}

impl ConstEntry {
    fn to_value(&self) -> Value {
        match self {
            ConstEntry::Int(n) => Value::Int((*n).clamp(i32::MIN as i64, i32::MAX as i64) as i32),
            ConstEntry::Float(n) => Value::Number(*n),
            ConstEntry::String(s) => Value::String(s.clone()),
            ConstEntry::Null => Value::Null,
            ConstEntry::Undefined => Value::Undefined,
            ConstEntry::Function(i) => Value::Function(*i),
            ConstEntry::Global(_) => Value::Undefined,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interpret_push_return() {
        let chunk = BytecodeChunk {
            code: vec![Opcode::PushConst as u8, 0, Opcode::Return as u8],
            constants: vec![ConstEntry::Int(42)],
            num_locals: 0,
            named_locals: vec![],
            captured_names: vec![],
            rest_param_index: None,
            handlers: vec![],
        };
        let result = interpret(&chunk).expect("interpret");
        if let Completion::Return(Value::Int(42)) = result {
        } else {
            panic!("expected Return(42), got {:?}", result);
        }
    }

    #[test]
    fn interpret_add() {
        let chunk = BytecodeChunk {
            code: vec![
                Opcode::PushConst as u8,
                0,
                Opcode::PushConst as u8,
                1,
                Opcode::Add as u8,
                Opcode::Return as u8,
            ],
            constants: vec![ConstEntry::Int(1), ConstEntry::Int(2)],
            num_locals: 0,
            named_locals: vec![],
            captured_names: vec![],
            rest_param_index: None,
            handlers: vec![],
        };
        let result = interpret(&chunk).expect("interpret");
        if let Completion::Return(v) = result {
            assert_eq!(v.to_i64(), 3);
        } else {
            panic!("expected Return(3), got {:?}", result);
        }
    }

    #[test]
    fn interpret_strict_eq_int_number() {
        let result = crate::driver::Driver::run("function main() { return (1 === 1.0) ? 1 : 0; }")
            .expect("run");
        assert_eq!(result, 1, "1 === 1.0 should be true");
    }

    #[test]
    fn interpret_div_by_zero() {
        let result = crate::driver::Driver::run(
            "function main() { let a = 1/0; let b = -1/0; return (a > 1e9 && b < -1e9) ? 1 : 0; }",
        )
        .expect("run");
        assert_eq!(result, 1, "1/0=Infinity, -1/0=-Infinity");
    }

    #[test]
    fn interpret_object_prop() {
        let chunk = BytecodeChunk {
            code: vec![
                Opcode::NewObject as u8,
                Opcode::Dup as u8,
                Opcode::PushConst as u8,
                0,
                Opcode::Swap as u8,
                Opcode::SetProp as u8,
                1,
                Opcode::Pop as u8,
                Opcode::GetProp as u8,
                1,
                Opcode::Return as u8,
            ],
            constants: vec![ConstEntry::Int(42), ConstEntry::String("x".to_string())],
            num_locals: 0,
            named_locals: vec![],
            captured_names: vec![],
            rest_param_index: None,
            handlers: vec![],
        };
        let result = interpret(&chunk).expect("interpret");
        if let Completion::Return(v) = result {
            assert_eq!(v.to_i64(), 42);
        } else {
            panic!("expected Return(42), got {:?}", result);
        }
    }

    #[test]
    fn interpret_prop_assignment_via_store_load() {
        let chunk = BytecodeChunk {
            code: vec![
                Opcode::NewObject as u8,
                Opcode::StoreLocal as u8,
                0,
                Opcode::LoadLocal as u8,
                0,
                Opcode::PushConst as u8,
                0,
                Opcode::Swap as u8,
                Opcode::SetProp as u8,
                1,
                Opcode::LoadLocal as u8,
                0,
                Opcode::GetProp as u8,
                2,
                Opcode::Return as u8,
            ],
            constants: vec![
                ConstEntry::Int(42),
                ConstEntry::String("x".to_string()),
                ConstEntry::String("x".to_string()),
            ],
            num_locals: 1,
            named_locals: vec![],
            captured_names: vec![],
            rest_param_index: None,
            handlers: vec![],
        };
        let result = interpret(&chunk).expect("interpret");
        if let Completion::Return(v) = result {
            assert_eq!(
                v.to_i64(),
                42,
                "StoreLocal/LoadLocal + SetProp should mutate"
            );
        } else {
            panic!("expected Return(42), got {:?}", result);
        }
    }

    #[test]
    fn interpret_array_length() {
        let chunk = BytecodeChunk {
            code: vec![
                Opcode::NewArray as u8,
                Opcode::Dup as u8,
                Opcode::PushConst as u8,
                0,
                Opcode::Swap as u8,
                Opcode::SetProp as u8,
                1,
                Opcode::Pop as u8,
                Opcode::Dup as u8,
                Opcode::PushConst as u8,
                2,
                Opcode::Swap as u8,
                Opcode::SetProp as u8,
                3,
                Opcode::Pop as u8,
                Opcode::GetProp as u8,
                4,
                Opcode::Return as u8,
            ],
            constants: vec![
                ConstEntry::Int(10),
                ConstEntry::String("0".to_string()),
                ConstEntry::Int(20),
                ConstEntry::String("1".to_string()),
                ConstEntry::String("length".to_string()),
            ],
            num_locals: 0,
            named_locals: vec![],
            captured_names: vec![],
            rest_param_index: None,
            handlers: vec![],
        };
        let result = interpret(&chunk).expect("interpret");
        if let Completion::Return(v) = result {
            assert_eq!(v.to_i64(), 2);
        } else {
            panic!("expected Return(2) for array length, got {:?}", result);
        }
    }
}
