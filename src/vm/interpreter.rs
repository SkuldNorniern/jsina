use crate::ir::bytecode::{BytecodeChunk, ConstEntry};
#[cfg(test)]
use crate::ir::bytecode::Opcode;
use crate::runtime::builtins;
use crate::runtime::{Heap, Value};
use std::sync::atomic::{AtomicBool, Ordering};

const MAX_CALL_DEPTH: usize = 1000;

enum BuiltinResult {
    Push(Value),
    Throw(Value),
}

fn execute_builtin(
    builtin_id: u8,
    argc: usize,
    stack: &mut Vec<Value>,
    heap: &mut Heap,
) -> Result<BuiltinResult, VmError> {
    if builtin_id > builtins::MAX_BUILTIN_ID {
        return Err(VmError::InvalidOpcode(builtin_id));
    }
    let mut args: Vec<Value> = (0..argc)
        .map(|_| stack.pop().ok_or(VmError::StackUnderflow))
        .collect::<Result<Vec<_>, _>>()?;
    args.reverse();
    match builtins::dispatch(builtin_id, &args, heap) {
        Ok(v) => Ok(BuiltinResult::Push(v)),
        Err(builtins::BuiltinError::Throw(v)) => Ok(BuiltinResult::Throw(v)),
    }
}

#[inline(always)]
fn read_u8(code: &[u8], pc: usize) -> u8 {
    debug_assert!(pc < code.len());
    // SAFETY: Loop exits when pc >= code.len(); each opcode consumes correct operand bytes.
    unsafe { *code.get_unchecked(pc) }
}

#[derive(Debug, Clone)]
pub enum Completion {
    Normal(Value),
    Return(Value),
    Throw(Value),
}

#[derive(Debug)]
pub enum VmError {
    StackUnderflow,
    InvalidOpcode(u8),
    InvalidConstIndex(usize),
    CallDepthExceeded,
    StepLimitExceeded,
    Cancelled,
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VmError::StackUnderflow => write!(f, "stack underflow"),
            VmError::InvalidOpcode(b) => write!(f, "invalid opcode: 0x{:02x}", b),
            VmError::InvalidConstIndex(i) => write!(f, "invalid constant index: {}", i),
            VmError::CallDepthExceeded => write!(f, "maximum call depth exceeded"),
            VmError::StepLimitExceeded => write!(f, "step limit exceeded (infinite loop?)"),
            VmError::Cancelled => write!(f, "execution cancelled (timeout)"),
        }
    }
}

impl std::error::Error for VmError {}

#[derive(Debug, Clone)]
pub struct Program {
    pub chunks: Vec<BytecodeChunk>,
    pub entry: usize,
    pub init_entry: Option<usize>,
    pub global_funcs: Vec<(String, usize)>,
}

pub fn interpret(chunk: &BytecodeChunk) -> Result<Completion, VmError> {
    let program = Program {
        chunks: vec![chunk.clone()],
        entry: 0,
        init_entry: None,
        global_funcs: Vec::new(),
    };
    interpret_program(&program)
}

struct Frame {
    chunk_index: usize,
    pc: usize,
    locals: Vec<Value>,
    this_value: Value,
    rethrow_after_finally: bool,
    new_object: Option<usize>,
}

pub fn interpret_program(program: &Program) -> Result<Completion, VmError> {
    interpret_program_with_trace(program, false)
}

pub fn interpret_program_with_limit(
    program: &Program,
    trace: bool,
    step_limit: Option<u64>,
) -> Result<Completion, VmError> {
    let (result, _) = interpret_program_with_trace_and_limit(program, trace, step_limit, None, false);
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

#[cold]
fn trace_op(pc: usize, op: u8) {
    let opname = crate::ir::disasm::opcode_name(op);
    eprintln!("  {:04}  {}", pc, opname);
}

struct GetPropCache {
    obj_id: usize,
    is_array: bool,
    key: String,
    value: Option<Value>,
}

impl GetPropCache {
    fn get(
        &mut self,
        obj_id: usize,
        is_array: bool,
        key: &str,
        heap: &crate::runtime::Heap,
    ) -> Value {
        if self.obj_id == obj_id && self.is_array == is_array && self.key == key {
            if let Some(ref v) = self.value {
                return v.clone();
            }
        }
        let result = if is_array {
            heap.get_array_prop(obj_id, key)
        } else {
            heap.get_prop(obj_id, key)
        };
        self.obj_id = obj_id;
        self.is_array = is_array;
        self.key = key.to_string();
        self.value = Some(result.clone());
        result
    }
    fn invalidate(&mut self, obj_id: usize, is_array: bool, key: &str) {
        if self.obj_id == obj_id && self.is_array == is_array && self.key == key {
            self.value = None;
        }
    }
    fn invalidate_all(&mut self) {
        self.value = None;
    }
}

pub fn interpret_program_with_trace(program: &Program, trace: bool) -> Result<Completion, VmError> {
    let (result, _) = interpret_program_with_trace_and_limit(program, trace, None, None, false);
    result
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
        interpret_program_with_heap_and_entry(
            program, heap, init_idx, trace, step_limit, cancel,
        )?;
    }
    interpret_program_with_heap_and_entry(
        program, heap, program.entry, trace, step_limit, cancel,
    )
}

pub fn interpret_program_with_heap_and_entry(
    program: &Program,
    heap: &mut Heap,
    entry: usize,
    trace: bool,
    step_limit: Option<u64>,
    cancel: Option<&AtomicBool>,
) -> Result<Completion, VmError> {
    let mut steps_remaining = step_limit;
    let mut stack: Vec<Value> = Vec::new();
    let mut getprop_cache = GetPropCache {
        obj_id: usize::MAX,
        is_array: false,
        key: String::new(),
        value: None,
    };
    let entry_chunk = program
        .chunks
        .get(entry)
        .ok_or(VmError::InvalidConstIndex(entry))?;
    let mut frames: Vec<Frame> = vec![Frame {
        chunk_index: entry,
        pc: 0,
        locals: (0..entry_chunk.num_locals).map(|_| Value::Undefined).collect(),
        this_value: Value::Undefined,
        rethrow_after_finally: false,
        new_object: None,
    }];

    loop {
        if let Some(c) = cancel {
            if c.load(Ordering::Relaxed) {
                return Err(VmError::Cancelled);
            }
        }
        if let Some(ref mut n) = steps_remaining {
            if *n == 0 {
                return Err(VmError::StepLimitExceeded);
            }
            *n -= 1;
        }

        let frame = frames.last_mut().ok_or(VmError::StackUnderflow)?;
        let chunk = &program.chunks[frame.chunk_index];
        let code = &chunk.code;
        let constants = &chunk.constants;
        let locals = &mut frame.locals;
        let pc = &mut frame.pc;

        if *pc >= code.len() {
            break;
        }

        let op = code[*pc];
        let trace_pc = *pc;
        *pc += 1;

        if trace {
            trace_op(trace_pc, op);
        }

        match op {
            0x01 => {
                let idx = read_u8(code, *pc) as usize;
                *pc += 1;
                let val = match constants.get(idx).ok_or(VmError::InvalidConstIndex(idx))? {
                    ConstEntry::Global(name) => heap.get_global(name),
                    c => c.to_value(),
                };
                stack.push(val);
            }
            0x02 => {
                stack.pop().ok_or(VmError::StackUnderflow)?;
            }
            0x06 => {
                let top = stack.last().cloned().ok_or(VmError::StackUnderflow)?;
                stack.push(top);
            }
            0x07 => {
                let b = stack.pop().ok_or(VmError::StackUnderflow)?;
                let a = stack.pop().ok_or(VmError::StackUnderflow)?;
                stack.push(b);
                stack.push(a);
            }
            0x03 => {
                let slot = read_u8(code, *pc) as usize;
                *pc += 1;
                let val = locals.get(slot).cloned().unwrap_or(Value::Undefined);
                stack.push(val);
            }
            0x04 => {
                let slot = read_u8(code, *pc) as usize;
                *pc += 1;
                let val = stack.pop().ok_or(VmError::StackUnderflow)?;
                if slot < locals.len() {
                    locals[slot] = val;
                }
            }
            0x05 => {
                stack.push(frame.this_value.clone());
            }
            0x20 => {
                let val = stack.pop().unwrap_or(Value::Undefined);
                let popped = frames.pop();
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
                if frames.is_empty() {
                    return Ok(Completion::Return(result));
                }
                stack.push(result);
            }
            0x21 => {
                let val = stack.pop().ok_or(VmError::StackUnderflow)?;
                let throw_pc = *pc - 1;
                let handler = chunk.handlers.iter().find(|h| {
                    (h.try_start as usize) <= throw_pc && throw_pc < (h.try_end as usize)
                });
                if let Some(h) = handler {
                    if (h.catch_slot as usize) < locals.len() {
                        locals[h.catch_slot as usize] = val.clone();
                    }
                    frame.rethrow_after_finally = h.is_finally;
                    *pc = h.handler_pc as usize;
                } else {
                    return Ok(Completion::Throw(val));
                }
            }
            0x22 => {
                let slot = *code.get(*pc).ok_or(VmError::StackUnderflow)? as usize;
                *pc += 1;
                if frame.rethrow_after_finally {
                    frame.rethrow_after_finally = false;
                    let val = locals.get(slot).cloned().unwrap_or(Value::Undefined);
                    return Ok(Completion::Throw(val));
                }
            }
            0x40 => {
                let (func_idx, callee_locals) = {
                    let func_idx = *code.get(*pc).ok_or(VmError::StackUnderflow)? as usize;
                    let argc = *code.get(*pc + 1).ok_or(VmError::StackUnderflow)? as usize;
                    *pc += 2;
                    let callee = program.chunks.get(func_idx).ok_or(VmError::InvalidConstIndex(func_idx))?;
                    let mut args: Vec<Value> = (0..argc)
                        .map(|_| stack.pop().ok_or(VmError::StackUnderflow))
                        .collect::<Result<Vec<_>, _>>()?;
                    args.reverse();
                    let mut callee_locals: Vec<Value> = (0..callee.num_locals).map(|_| Value::Undefined).collect();
                    if let Some(r) = callee.rest_param_index {
                        let r = r as usize;
                        for (i, v) in args.iter().take(r).cloned().enumerate() {
                            if i < callee_locals.len() {
                                callee_locals[i] = v;
                            }
                        }
                        if r < callee_locals.len() {
                            let rest_id = heap.alloc_array();
                            if r < args.len() {
                                heap.array_push_values(rest_id, &args[r..]);
                            }
                            callee_locals[r] = Value::Array(rest_id);
                        }
                    } else {
                        for (i, v) in args.into_iter().enumerate() {
                            if i < callee_locals.len() {
                                callee_locals[i] = v;
                            }
                        }
                    }
                    (func_idx, callee_locals)
                };
                if frames.len() >= MAX_CALL_DEPTH {
                    return Err(VmError::CallDepthExceeded);
                }
                frames.push(Frame {
                    chunk_index: func_idx,
                    pc: 0,
                    locals: callee_locals,
                    this_value: Value::Undefined,
                    rethrow_after_finally: false,
                    new_object: None,
                });
            }
            0x41 => {
                let builtin_id = read_u8(code, *pc);
                let argc = read_u8(code, *pc + 1) as usize;
                *pc += 2;
                let call_pc = *pc - 3;
                match execute_builtin(builtin_id, argc, &mut stack, heap) {
                    Ok(BuiltinResult::Push(v)) => {
                        getprop_cache.invalidate_all();
                        stack.push(v);
                    }
                    Ok(BuiltinResult::Throw(v)) => {
                        let handler = chunk.handlers.iter().find(|h| {
                            (h.try_start as usize) <= call_pc && call_pc < (h.try_end as usize)
                        });
                        if let Some(h) = handler {
                            if (h.catch_slot as usize) < locals.len() {
                                locals[h.catch_slot as usize] = v.clone();
                            }
                            frame.rethrow_after_finally = h.is_finally;
                            *pc = h.handler_pc as usize;
                        } else {
                            return Ok(Completion::Throw(v));
                        }
                    }
                    Err(e) => return Err(e),
                }
            }
            0x42 => {
                let argc = *code.get(*pc).ok_or(VmError::StackUnderflow)? as usize;
                *pc += 1;
                let call_pc = *pc - 2;
                let mut args: Vec<Value> = (0..argc)
                    .map(|_| stack.pop().ok_or(VmError::StackUnderflow))
                    .collect::<Result<Vec<_>, _>>()?;
                args.reverse();
                let callee = stack.pop().ok_or(VmError::StackUnderflow)?;
                let receiver = stack.pop().ok_or(VmError::StackUnderflow)?;
                if let Value::Builtin(builtin_id) = callee {
                    for a in &args {
                        stack.push(a.clone());
                    }
                    stack.push(receiver);
                    match execute_builtin(builtin_id, argc + 1, &mut stack, heap) {
                        Ok(BuiltinResult::Push(v)) => {
                            getprop_cache.invalidate_all();
                            stack.push(v);
                        }
                        Ok(BuiltinResult::Throw(v)) => {
                            let handler = chunk.handlers.iter().find(|h| {
                                (h.try_start as usize) <= call_pc && call_pc < (h.try_end as usize)
                            });
                            if let Some(h) = handler {
                                if (h.catch_slot as usize) < locals.len() {
                                    locals[h.catch_slot as usize] = v.clone();
                                }
                                frame.rethrow_after_finally = h.is_finally;
                                *pc = h.handler_pc as usize;
                            } else {
                                return Ok(Completion::Throw(v));
                            }
                        }
                        Err(e) => return Err(e),
                    }
                } else if let Value::Function(i) = callee {
                    let func_idx = i;
                    let callee_chunk = program.chunks.get(func_idx).ok_or(VmError::InvalidConstIndex(func_idx))?;
                    let mut callee_locals: Vec<Value> = (0..callee_chunk.num_locals).map(|_| Value::Undefined).collect();
                    if let Some(r) = callee_chunk.rest_param_index {
                        let r = r as usize;
                        for (i, v) in args.iter().take(r).cloned().enumerate() {
                            if i < callee_locals.len() {
                                callee_locals[i] = v;
                            }
                        }
                        if r < callee_locals.len() {
                            let rest_id = heap.alloc_array();
                            if r < args.len() {
                                heap.array_push_values(rest_id, &args[r..]);
                            }
                            callee_locals[r] = Value::Array(rest_id);
                        }
                    } else {
                        for (i, v) in args.into_iter().enumerate() {
                            if i < callee_locals.len() {
                                callee_locals[i] = v;
                            }
                        }
                    }
                    if frames.len() >= MAX_CALL_DEPTH {
                        return Err(VmError::CallDepthExceeded);
                    }
                    frames.push(Frame {
                        chunk_index: func_idx,
                        pc: 0,
                        locals: callee_locals,
                        this_value: receiver,
                        rethrow_after_finally: false,
                        new_object: None,
                    });
                } else {
                    return Ok(Completion::Throw(Value::String(
                        "TypeError: callee is not a function".to_string(),
                    )));
                }
            }
            0x43 => {
                let (func_idx, callee_locals, obj_id) = {
                    let func_idx = *code.get(*pc).ok_or(VmError::StackUnderflow)? as usize;
                    let argc = *code.get(*pc + 1).ok_or(VmError::StackUnderflow)? as usize;
                    *pc += 2;
                    let mut args: Vec<Value> = (0..argc)
                        .map(|_| stack.pop().ok_or(VmError::StackUnderflow))
                        .collect::<Result<Vec<_>, _>>()?;
                    args.reverse();
                    let obj_id = heap.alloc_object();
                    let callee = program.chunks.get(func_idx).ok_or(VmError::InvalidConstIndex(func_idx))?;
                    let mut callee_locals: Vec<Value> = (0..callee.num_locals).map(|_| Value::Undefined).collect();
                    if let Some(r) = callee.rest_param_index {
                        let r = r as usize;
                        for (i, v) in args.iter().take(r).cloned().enumerate() {
                            if i < callee_locals.len() {
                                callee_locals[i] = v;
                            }
                        }
                        if r < callee_locals.len() {
                            let rest_id = heap.alloc_array();
                            if r < args.len() {
                                heap.array_push_values(rest_id, &args[r..]);
                            }
                            callee_locals[r] = Value::Array(rest_id);
                        }
                    } else {
                        for (i, v) in args.into_iter().enumerate() {
                            if i < callee_locals.len() {
                                callee_locals[i] = v;
                            }
                        }
                    }
                    (func_idx, callee_locals, obj_id)
                };
                if frames.len() >= MAX_CALL_DEPTH {
                    return Err(VmError::CallDepthExceeded);
                }
                frames.push(Frame {
                    chunk_index: func_idx,
                    pc: 0,
                    locals: callee_locals,
                    this_value: Value::Object(obj_id),
                    rethrow_after_finally: false,
                    new_object: Some(obj_id),
                });
            }
            0x10 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = match (&lhs, &rhs) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a.saturating_add(*b)),
                    _ => add_values(&lhs, &rhs)?,
                };
                stack.push(result);
            }
            0x11 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = match (&lhs, &rhs) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a.saturating_sub(*b)),
                    _ => sub_values(&lhs, &rhs)?,
                };
                stack.push(result);
            }
            0x12 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = match (&lhs, &rhs) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a.saturating_mul(*b)),
                    _ => mul_values(&lhs, &rhs)?,
                };
                stack.push(result);
            }
            0x13 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = div_values(&lhs, &rhs)?;
                stack.push(result);
            }
            0x15 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = mod_values(&lhs, &rhs)?;
                stack.push(result);
            }
            0x16 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = pow_values(&lhs, &rhs)?;
                stack.push(result);
            }
            0x14 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = match (&lhs, &rhs) {
                    (Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
                    _ => lt_values(&lhs, &rhs)?,
                };
                stack.push(result);
            }
            0x19 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = lte_values(&lhs, &rhs)?;
                stack.push(result);
            }
            0x1a => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = gt_values(&lhs, &rhs)?;
                stack.push(result);
            }
            0x1b => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = gte_values(&lhs, &rhs)?;
                stack.push(result);
            }
            0x17 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = strict_eq_values(&lhs, &rhs);
                stack.push(result);
            }
            0x1c => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let eq = strict_eq_values(&lhs, &rhs);
                let result = Value::Bool(!matches!(eq, Value::Bool(true)));
                stack.push(result);
            }
            0x1e => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let (a, b) = (lhs.to_i32(), rhs.to_i32());
                stack.push(Value::Int(a << b));
            }
            0x1f => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let (a, b) = (lhs.to_i32(), rhs.to_i32());
                stack.push(Value::Int(a >> b));
            }
            0x23 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let (a, b) = (lhs.to_i32() as u32, rhs.to_i32() as u32);
                stack.push(Value::Int((a >> b) as i32));
            }
            0x24 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let (a, b) = (lhs.to_i32(), rhs.to_i32());
                stack.push(Value::Int(a & b));
            }
            0x25 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let (a, b) = (lhs.to_i32(), rhs.to_i32());
                stack.push(Value::Int(a | b));
            }
            0x26 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let (a, b) = (lhs.to_i32(), rhs.to_i32());
                stack.push(Value::Int(a ^ b));
            }
            0x18 => {
                let val = stack.pop().ok_or(VmError::StackUnderflow)?;
                stack.push(Value::Bool(!is_truthy(&val)));
            }
            0x27 => {
                let val = stack.pop().ok_or(VmError::StackUnderflow)?;
                let n = val.to_i32();
                stack.push(Value::Int(!n));
            }
            0x28 => {
                let constructor = stack.pop().ok_or(VmError::StackUnderflow)?;
                let value = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = instanceof_check(&value, &constructor, &heap);
                stack.push(Value::Bool(result));
            }
            0x29 => {
                let key = stack.pop().ok_or(VmError::StackUnderflow)?;
                let obj_val = stack.pop().ok_or(VmError::StackUnderflow)?;
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
                    _ => true,
                };
                stack.push(Value::Bool(result));
            }
            0x1d => {
                let val = stack.pop().ok_or(VmError::StackUnderflow)?;
                let s = match &val {
                    Value::Undefined => "undefined",
                    Value::Null => "object",
                    Value::Bool(_) => "boolean",
                    Value::Int(_) | Value::Number(_) => "number",
                    Value::String(_) => "string",
                    Value::Symbol(_) => "symbol",
                    Value::Object(_) | Value::Array(_) | Value::Map(_) | Value::Set(_) | Value::Date(_) => "object",
                    Value::Function(_) | Value::Builtin(_) => "function",
                };
                stack.push(Value::String(s.to_string()));
            }
            0x50 => {
                let id = heap.alloc_object();
                stack.push(Value::Object(id));
            }
            0x56 => {
                let proto = stack.pop().ok_or(VmError::StackUnderflow)?;
                let prototype = match &proto {
                    Value::Null | Value::Undefined => None,
                    Value::Object(id) => Some(*id),
                    _ => None,
                };
                let id = heap.alloc_object_with_prototype(prototype);
                stack.push(Value::Object(id));
            }
            0x51 => {
                let id = heap.alloc_array();
                stack.push(Value::Array(id));
            }
            0x52 => {
                let key_idx = read_u8(code, *pc) as usize;
                *pc += 1;
                let key = constants
                    .get(key_idx)
                    .ok_or(VmError::InvalidConstIndex(key_idx))?
                    .to_value();
                let obj = stack.pop().ok_or(VmError::StackUnderflow)?;
                let key_str = match &key {
                    Value::String(s) => s.clone(),
                    Value::Int(n) => n.to_string(),
                    _ => return Err(VmError::InvalidConstIndex(key_idx)),
                };
                let result = match &obj {
                    Value::Object(id) => getprop_cache.get(*id, false, &key_str, &heap),
                    Value::Array(id) => getprop_cache.get(*id, true, &key_str, &heap),
                    Value::Map(id) if key_str == "size" => Value::Int(heap.map_size(*id) as i32),
                    Value::Map(_) => primitive_map_method(&key_str),
                    Value::Set(id) if key_str == "size" => Value::Int(heap.set_size(*id) as i32),
                    Value::Set(_) => primitive_set_method(&key_str),
                    Value::String(s) if key_str == "length" => Value::Int(s.len() as i32),
                    Value::String(s) => {
                        if let Ok(idx) = key_str.parse::<usize>() {
                            s.chars().nth(idx).map(|c| Value::String(c.to_string())).unwrap_or(Value::Undefined)
                        } else {
                            primitive_string_method(&key_str)
                        }
                    }
                    Value::Date(_) => primitive_date_method(&key_str),
                    Value::Number(_) | Value::Int(_) => primitive_number_method(&key_str),
                    Value::Bool(_) => primitive_bool_method(&key_str),
                    Value::Function(i) => heap.get_function_prop(*i, &key_str),
                    _ => Value::Undefined,
                };
                stack.push(result);
            }
            0x53 => {
                let key_idx = read_u8(code, *pc) as usize;
                *pc += 1;
                let key = constants
                    .get(key_idx)
                    .ok_or(VmError::InvalidConstIndex(key_idx))?
                    .to_value();
                let obj = stack.pop().ok_or(VmError::StackUnderflow)?;
                let value = stack.pop().ok_or(VmError::StackUnderflow)?;
                let key_str = match &key {
                    Value::String(s) => s.clone(),
                    Value::Int(n) => n.to_string(),
                    _ => return Err(VmError::InvalidConstIndex(key_idx)),
                };
                match &obj {
                    Value::Object(id) => {
                        getprop_cache.invalidate(*id, false, &key_str);
                        heap.set_prop(*id, &key_str, value.clone());
                    }
                    Value::Array(id) => {
                        getprop_cache.invalidate(*id, true, &key_str);
                        heap.set_array_prop(*id, &key_str, value.clone());
                    }
                    Value::Map(id) => heap.map_set(*id, &key_str, value.clone()),
                    Value::Function(i) => heap.set_function_prop(*i, &key_str, value.clone()),
                    _ => {}
                }
                stack.push(value);
            }
            0x54 => {
                let key = stack.pop().ok_or(VmError::StackUnderflow)?;
                let obj = stack.pop().ok_or(VmError::StackUnderflow)?;
                let key_str = value_to_prop_key(&key);
                let result = match &obj {
                    Value::Object(id) => heap.get_prop(*id, &key_str),
                    Value::Array(id) => heap.get_array_prop(*id, &key_str),
                    Value::Map(id) if key_str == "size" => Value::Int(heap.map_size(*id) as i32),
                    Value::Map(_) => primitive_map_method(&key_str),
                    Value::Set(id) if key_str == "size" => Value::Int(heap.set_size(*id) as i32),
                    Value::Set(_) => primitive_set_method(&key_str),
                    Value::String(s) if key_str == "length" => Value::Int(s.len() as i32),
                    Value::String(s) => {
                        if let Ok(idx) = key_str.parse::<usize>() {
                            s.chars().nth(idx).map(|c| Value::String(c.to_string())).unwrap_or(Value::Undefined)
                        } else {
                            primitive_string_method(&key_str)
                        }
                    }
                    Value::Date(_) => primitive_date_method(&key_str),
                    Value::Number(_) | Value::Int(_) => primitive_number_method(&key_str),
                    Value::Bool(_) => primitive_bool_method(&key_str),
                    Value::Function(i) => heap.get_function_prop(*i, &key_str),
                    _ => Value::Undefined,
                };
                stack.push(result);
            }
            0x55 => {
                let value = stack.pop().ok_or(VmError::StackUnderflow)?;
                let key = stack.pop().ok_or(VmError::StackUnderflow)?;
                let obj = stack.pop().ok_or(VmError::StackUnderflow)?;
                let key_str = value_to_prop_key(&key);
                match &obj {
                    Value::Object(id) => heap.set_prop(*id, &key_str, value.clone()),
                    Value::Array(id) => heap.set_array_prop(*id, &key_str, value.clone()),
                    Value::Map(id) => heap.map_set(*id, &key_str, value.clone()),
                    Value::Function(i) => heap.set_function_prop(*i, &key_str, value.clone()),
                    _ => {}
                }
                stack.push(value);
            }
            0x30 => {
                let offset_bytes = &code[*pc..*pc + 2];
                *pc += 2;
                let offset = i16::from_le_bytes([offset_bytes[0], offset_bytes[1]]) as isize;
                let val = stack.pop().ok_or(VmError::StackUnderflow)?;
                if !is_truthy(&val) {
                    *pc = (*pc as isize + offset) as usize;
                }
            }
            0x32 => {
                let offset_bytes = &code[*pc..*pc + 2];
                *pc += 2;
                let offset = i16::from_le_bytes([offset_bytes[0], offset_bytes[1]]) as isize;
                let val = stack.pop().ok_or(VmError::StackUnderflow)?;
                if is_nullish(&val) {
                    *pc = (*pc as isize + offset) as usize;
                }
            }
            0x31 => {
                let offset_bytes = &code[*pc..*pc + 2];
                *pc += 2;
                let offset = i16::from_le_bytes([offset_bytes[0], offset_bytes[1]]) as isize;
                *pc = (*pc as isize + offset) as usize;
            }
            _ => return Err(VmError::InvalidOpcode(op)),
        }
    }

    let result = stack.pop().unwrap_or(Value::Undefined);
    Ok(Completion::Normal(result))
}

fn add_values(a: &Value, b: &Value) -> Result<Value, VmError> {
    match (a, b) {
        (Value::String(x), Value::String(y)) => Ok(Value::String(format!("{}{}", x, y))),
        (Value::String(x), y) => Ok(Value::String(format!("{}{}", x, y))),
        (x, Value::String(y)) => Ok(Value::String(format!("{}{}", x, y))),
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x.saturating_add(*y))),
        (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
        (Value::Int(x), Value::Number(y)) => Ok(Value::Number(*x as f64 + y)),
        (Value::Number(x), Value::Int(y)) => Ok(Value::Number(x + *y as f64)),
        _ => Ok(Value::Number(f64::NAN)),
    }
}

fn is_nullish(v: &Value) -> bool {
    matches!(v, Value::Undefined | Value::Null)
}

fn instanceof_check(value: &Value, constructor: &Value, heap: &Heap) -> bool {
    let constructor_name = get_constructor_name(constructor, heap);
    match (value, constructor_name.as_deref()) {
        (Value::Array(_), Some("Array")) => true,
        (Value::Map(_), Some("Map")) => true,
        (Value::Set(_), Some("Set")) => true,
        (Value::Date(_), Some("Date")) => true,
        (Value::Object(id), Some("Error")) => heap.is_error_object(*id),
        (Value::Object(id), Some("ReferenceError")) => heap.is_error_object(*id) && matches!(heap.get_prop(*id, "name"), Value::String(s) if s == "ReferenceError"),
        (Value::Object(id), Some("TypeError")) => heap.is_error_object(*id) && matches!(heap.get_prop(*id, "name"), Value::String(s) if s == "TypeError"),
        (Value::Object(id), Some("RangeError")) => heap.is_error_object(*id) && matches!(heap.get_prop(*id, "name"), Value::String(s) if s == "RangeError"),
        (Value::Object(id), Some("SyntaxError")) => heap.is_error_object(*id) && matches!(heap.get_prop(*id, "name"), Value::String(s) if s == "SyntaxError"),
        (Value::Object(id), Some("URIError")) => heap.is_error_object(*id) && matches!(heap.get_prop(*id, "name"), Value::String(s) if s == "URIError"),
        (Value::Object(id), Some("Object")) => {
            !heap.is_error_object(*id)
        }
        (Value::Object(id), _) => {
            let constructor_proto = match constructor {
                Value::Object(cid) => heap.get_prop(*cid, "prototype"),
                _ => return false,
            };
            let proto_id = match &constructor_proto {
                Value::Object(pid) => *pid,
                _ => return false,
            };
            let mut current = Some(*id);
            let mut depth = 0;
            while let Some(obj_id) = current {
                if depth > 100 { break; }
                depth += 1;
                let obj_proto = heap.get_proto(obj_id);
                match obj_proto {
                    Some(pid) if pid == proto_id => return true,
                    Some(pid) => current = Some(pid),
                    None => break,
                }
            }
            false
        }
        _ => false,
    }
}

fn get_constructor_name(constructor: &Value, heap: &Heap) -> Option<String> {
    match constructor {
        Value::Object(id) => {
            if let Value::String(name) = heap.get_prop(*id, "name") {
                return Some(name);
            }
            let global = heap.global_object();
            for name in ["Array", "Object", "Error", "ReferenceError", "TypeError", "RangeError", "SyntaxError", "URIError", "Map", "Set", "Date"] {
                if let Value::Object(gid) = heap.get_prop(global, name) {
                    if gid == *id {
                        return Some(name.to_string());
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Undefined | Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(n) => *n != 0,
        Value::Number(n) => *n != 0.0 && !n.is_nan(),
        Value::String(_) | Value::Symbol(_) | Value::Object(_) | Value::Array(_) | Value::Map(_) | Value::Set(_) | Value::Date(_) | Value::Function(_) | Value::Builtin(_) => true,
    }
}

fn primitive_string_method(key: &str) -> Value {
    match key {
        "includes" => Value::Builtin(0x1A),
        "indexOf" => Value::Builtin(0x19),
        "split" => Value::Builtin(0x60),
        "trim" => Value::Builtin(0x61),
        "toLowerCase" => Value::Builtin(0x62),
        "toUpperCase" => Value::Builtin(0x63),
        "charAt" => Value::Builtin(0x64),
        "repeat" => Value::Builtin(0x65),
        "anchor" => Value::Builtin(0x67),
        "big" => Value::Builtin(0x68),
        "blink" => Value::Builtin(0x69),
        "bold" => Value::Builtin(0x6A),
        "fixed" => Value::Builtin(0x6B),
        "fontcolor" => Value::Builtin(0x6C),
        "fontsize" => Value::Builtin(0x6D),
        "italics" => Value::Builtin(0x6E),
        "link" => Value::Builtin(0x6F),
        "small" => Value::Builtin(0xB1),
        "strike" => Value::Builtin(0xB2),
        "sub" => Value::Builtin(0xB3),
        "sup" => Value::Builtin(0xB4),
        _ => Value::Undefined,
    }
}

fn primitive_date_method(key: &str) -> Value {
    match key {
        "getTime" => Value::Builtin(0xC2),
        "toString" => Value::Builtin(0xC3),
        "toISOString" => Value::Builtin(0xC4),
        "getYear" => Value::Builtin(0xC5),
        "setYear" => Value::Builtin(0xC6),
        "toGMTString" => Value::Builtin(0xC7),
        _ => Value::Undefined,
    }
}

fn primitive_number_method(key: &str) -> Value {
    match key {
        "toString" => Value::Builtin(0x55),
        "valueOf" => Value::Builtin(0x56),
        _ => Value::Undefined,
    }
}

fn primitive_bool_method(key: &str) -> Value {
    match key {
        "toString" => Value::Builtin(0x55),
        "valueOf" => Value::Builtin(0x56),
        _ => Value::Undefined,
    }
}

fn primitive_map_method(key: &str) -> Value {
    match key {
        "set" => Value::Builtin(0x91),
        "get" => Value::Builtin(0x92),
        "has" => Value::Builtin(0x93),
        _ => Value::Undefined,
    }
}

fn primitive_set_method(key: &str) -> Value {
    match key {
        "add" => Value::Builtin(0xA1),
        "has" => Value::Builtin(0xA2),
        _ => Value::Undefined,
    }
}

fn value_to_prop_key(v: &Value) -> String {
    match v {
        Value::String(s) => s.clone(),
        Value::Int(n) => n.to_string(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Null => "null".to_string(),
        Value::Undefined => "undefined".to_string(),
        Value::Symbol(_) => "Symbol()".to_string(),
        Value::Object(_) | Value::Array(_) | Value::Map(_) | Value::Set(_) | Value::Date(_) => "[object Object]".to_string(),
        Value::Function(_) | Value::Builtin(_) => "function".to_string(),
    }
}

fn sub_values(a: &Value, b: &Value) -> Result<Value, VmError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x.saturating_sub(*y))),
        (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x - y)),
        (Value::Int(x), Value::Number(y)) => Ok(Value::Number(*x as f64 - y)),
        (Value::Number(x), Value::Int(y)) => Ok(Value::Number(x - *y as f64)),
        _ => Ok(Value::Number(f64::NAN)),
    }
}

fn mul_values(a: &Value, b: &Value) -> Result<Value, VmError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x.saturating_mul(*y))),
        (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x * y)),
        (Value::Int(x), Value::Number(y)) => Ok(Value::Number(*x as f64 * y)),
        (Value::Number(x), Value::Int(y)) => Ok(Value::Number(x * *y as f64)),
        _ => Ok(Value::Number(f64::NAN)),
    }
}

fn div_values(a: &Value, b: &Value) -> Result<Value, VmError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => {
            if *y == 0 {
                let r = if *x == 0 {
                    f64::NAN
                } else if *x > 0 {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                };
                Ok(Value::Number(r))
            } else {
                Ok(Value::Number(*x as f64 / *y as f64))
            }
        }
        (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x / y)),
        (Value::Int(x), Value::Number(y)) => Ok(Value::Number(*x as f64 / y)),
        (Value::Number(x), Value::Int(y)) => Ok(Value::Number(x / *y as f64)),
        _ => Ok(Value::Number(f64::NAN)),
    }
}

fn mod_values(a: &Value, b: &Value) -> Result<Value, VmError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => {
            if *y == 0 {
                Ok(Value::Number(f64::NAN))
            } else {
                Ok(Value::Int(x.wrapping_rem(*y)))
            }
        }
        (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x % y)),
        (Value::Int(x), Value::Number(y)) => Ok(Value::Number(*x as f64 % y)),
        (Value::Number(x), Value::Int(y)) => Ok(Value::Number(x % *y as f64)),
        _ => Ok(Value::Number(f64::NAN)),
    }
}

fn pow_values(a: &Value, b: &Value) -> Result<Value, VmError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) if *y >= 0 && *y <= 31 => {
            Ok(Value::Int(x.saturating_pow(*y as u32)))
        }
        (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x.powf(*y))),
        (Value::Int(x), Value::Number(y)) => Ok(Value::Number((*x as f64).powf(*y))),
        (Value::Number(x), Value::Int(y)) => Ok(Value::Number(x.powi(*y))),
        _ => Ok(Value::Number(f64::NAN)),
    }
}

fn lt_values(a: &Value, b: &Value) -> Result<Value, VmError> {
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => x < y,
        (Value::Number(x), Value::Number(y)) => x < y,
        (Value::Int(x), Value::Number(y)) => (*x as f64) < *y,
        (Value::Number(x), Value::Int(y)) => *x < (*y as f64),
        _ => false,
    };
    Ok(Value::Bool(result))
}

fn lte_values(a: &Value, b: &Value) -> Result<Value, VmError> {
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => x <= y,
        (Value::Number(x), Value::Number(y)) => x <= y,
        (Value::Int(x), Value::Number(y)) => (*x as f64) <= *y,
        (Value::Number(x), Value::Int(y)) => *x <= (*y as f64),
        _ => false,
    };
    Ok(Value::Bool(result))
}

fn gt_values(a: &Value, b: &Value) -> Result<Value, VmError> {
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => x > y,
        (Value::Number(x), Value::Number(y)) => x > y,
        (Value::Int(x), Value::Number(y)) => (*x as f64) > *y,
        (Value::Number(x), Value::Int(y)) => *x > (*y as f64),
        _ => false,
    };
    Ok(Value::Bool(result))
}

fn gte_values(a: &Value, b: &Value) -> Result<Value, VmError> {
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => x >= y,
        (Value::Number(x), Value::Number(y)) => x >= y,
        (Value::Int(x), Value::Number(y)) => (*x as f64) >= *y,
        (Value::Number(x), Value::Int(y)) => *x >= (*y as f64),
        _ => false,
    };
    Ok(Value::Bool(result))
}

fn strict_eq_values(a: &Value, b: &Value) -> Value {
    let result = match (a, b) {
        (Value::Undefined, Value::Undefined) => true,
        (Value::Null, Value::Null) => true,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Int(x), Value::Number(y)) => !y.is_nan() && (*x as f64) == *y,
        (Value::Number(x), Value::Int(y)) => !x.is_nan() && *x == (*y as f64),
        (Value::Number(x), Value::Number(y)) => !x.is_nan() && !y.is_nan() && x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Object(x), Value::Object(y)) => x == y,
        (Value::Array(x), Value::Array(y)) => x == y,
        (Value::Map(x), Value::Map(y)) => x == y,
        (Value::Set(x), Value::Set(y)) => x == y,
        (Value::Date(x), Value::Date(y)) => x == y,
        (Value::Function(x), Value::Function(y)) => x == y,
        (Value::Builtin(x), Value::Builtin(y)) => x == y,
        _ => false,
    };
    Value::Bool(result)
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
                Opcode::PushConst as u8, 0,
                Opcode::PushConst as u8, 1,
                Opcode::Add as u8,
                Opcode::Return as u8,
            ],
            constants: vec![ConstEntry::Int(1), ConstEntry::Int(2)],
            num_locals: 0,
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
    fn interpret_object_prop() {
        let chunk = BytecodeChunk {
            code: vec![
                Opcode::NewObject as u8,
                Opcode::Dup as u8,
                Opcode::PushConst as u8, 0,
                Opcode::Swap as u8,
                Opcode::SetProp as u8, 1,
                Opcode::Pop as u8,
                Opcode::GetProp as u8, 1,
                Opcode::Return as u8,
            ],
            constants: vec![ConstEntry::Int(42), ConstEntry::String("x".to_string())],
            num_locals: 0,
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
                Opcode::StoreLocal as u8, 0,
                Opcode::LoadLocal as u8, 0,
                Opcode::PushConst as u8, 0,
                Opcode::Swap as u8,
                Opcode::SetProp as u8, 1,
                Opcode::LoadLocal as u8, 0,
                Opcode::GetProp as u8, 2,
                Opcode::Return as u8,
            ],
            constants: vec![
                ConstEntry::Int(42),
                ConstEntry::String("x".to_string()),
                ConstEntry::String("x".to_string()),
            ],
            num_locals: 1,
            rest_param_index: None,
            handlers: vec![],
        };
        let result = interpret(&chunk).expect("interpret");
        if let Completion::Return(v) = result {
            assert_eq!(v.to_i64(), 42, "StoreLocal/LoadLocal + SetProp should mutate");
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
                Opcode::PushConst as u8, 0,
                Opcode::Swap as u8,
                Opcode::SetProp as u8, 1,
                Opcode::Pop as u8,
                Opcode::Dup as u8,
                Opcode::PushConst as u8, 2,
                Opcode::Swap as u8,
                Opcode::SetProp as u8, 3,
                Opcode::Pop as u8,
                Opcode::GetProp as u8, 4,
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
