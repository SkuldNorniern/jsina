use crate::ir::bytecode::{BytecodeChunk, ConstEntry};
#[cfg(test)]
use crate::ir::bytecode::Opcode;
use crate::runtime::{Heap, Value};

const MAX_CALL_DEPTH: usize = 1000;

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
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VmError::StackUnderflow => write!(f, "stack underflow"),
            VmError::InvalidOpcode(b) => write!(f, "invalid opcode: 0x{:02x}", b),
            VmError::InvalidConstIndex(i) => write!(f, "invalid constant index: {}", i),
            VmError::CallDepthExceeded => write!(f, "maximum call depth exceeded"),
        }
    }
}

impl std::error::Error for VmError {}

#[derive(Debug, Clone)]
pub struct Program {
    pub chunks: Vec<BytecodeChunk>,
    pub entry: usize,
}

pub fn interpret(chunk: &BytecodeChunk) -> Result<Completion, VmError> {
    let program = Program {
        chunks: vec![chunk.clone()],
        entry: 0,
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
    let mut heap = Heap::new();
    let mut stack: Vec<Value> = Vec::new();
    let mut getprop_cache = GetPropCache {
        obj_id: usize::MAX,
        is_array: false,
        key: String::new(),
        value: None,
    };
    let entry_chunk = program
        .chunks
        .get(program.entry)
        .ok_or(VmError::InvalidConstIndex(program.entry))?;
    let mut frames: Vec<Frame> = vec![Frame {
        chunk_index: program.entry,
        pc: 0,
        locals: (0..entry_chunk.num_locals).map(|_| Value::Undefined).collect(),
        this_value: Value::Undefined,
        rethrow_after_finally: false,
        new_object: None,
    }];

    loop {
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
                let val = constants
                    .get(idx)
                    .ok_or(VmError::InvalidConstIndex(idx))?
                    .to_value();
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
                    for (i, v) in args.into_iter().enumerate() {
                        if i < callee_locals.len() {
                            callee_locals[i] = v;
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
                let builtin_id = *code.get(*pc).ok_or(VmError::StackUnderflow)?;
                let argc = *code.get(*pc + 1).ok_or(VmError::StackUnderflow)? as usize;
                *pc += 2;
                let mut args: Vec<Value> = (0..argc)
                    .map(|_| stack.pop().ok_or(VmError::StackUnderflow))
                    .collect::<Result<Vec<_>, _>>()?;
                args.reverse();
                if builtin_id > 39 {
                    return Err(VmError::InvalidOpcode(builtin_id));
                }
                match crate::runtime::builtins::dispatch(builtin_id, &args, &mut heap) {
                    Ok(v) => {
                        getprop_cache.invalidate_all();
                        stack.push(v);
                    }
                    Err(crate::runtime::builtins::BuiltinError::Throw(v)) => return Ok(Completion::Throw(v)),
                }
            }
            0x42 => {
                let argc = *code.get(*pc).ok_or(VmError::StackUnderflow)? as usize;
                *pc += 1;
                let mut args: Vec<Value> = (0..argc)
                    .map(|_| stack.pop().ok_or(VmError::StackUnderflow))
                    .collect::<Result<Vec<_>, _>>()?;
                args.reverse();
                let callee = stack.pop().ok_or(VmError::StackUnderflow)?;
                let receiver = stack.pop().ok_or(VmError::StackUnderflow)?;
                let func_idx = match &callee {
                    Value::Function(i) => *i,
                    _ => {
                        return Ok(Completion::Throw(Value::String(
                            "callee is not a function".to_string(),
                        )));
                    }
                };
                let callee_chunk = program.chunks.get(func_idx).ok_or(VmError::InvalidConstIndex(func_idx))?;
                let mut callee_locals: Vec<Value> = (0..callee_chunk.num_locals).map(|_| Value::Undefined).collect();
                for (i, v) in args.into_iter().enumerate() {
                    if i < callee_locals.len() {
                        callee_locals[i] = v;
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
                    for (i, v) in args.into_iter().enumerate() {
                        if i < callee_locals.len() {
                            callee_locals[i] = v;
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
            0x1d => {
                let val = stack.pop().ok_or(VmError::StackUnderflow)?;
                let s = match &val {
                    Value::Undefined => "undefined",
                    Value::Null => "object",
                    Value::Bool(_) => "boolean",
                    Value::Int(_) | Value::Number(_) => "number",
                    Value::String(_) => "string",
                    Value::Object(_) | Value::Array(_) => "object",
                    Value::Function(_) => "function",
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
                    Value::String(s) if key_str == "length" => Value::Int(s.len() as i32),
                    Value::String(s) => {
                        if let Ok(idx) = key_str.parse::<usize>() {
                            s.chars().nth(idx).map(|c| Value::String(c.to_string())).unwrap_or(Value::Undefined)
                        } else {
                            Value::Undefined
                        }
                    }
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
                    Value::String(s) if key_str == "length" => Value::Int(s.len() as i32),
                    Value::String(s) => {
                        if let Ok(idx) = key_str.parse::<usize>() {
                            s.chars().nth(idx).map(|c| Value::String(c.to_string())).unwrap_or(Value::Undefined)
                        } else {
                            Value::Undefined
                        }
                    }
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

fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Undefined | Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(n) => *n != 0,
        Value::Number(n) => *n != 0.0 && !n.is_nan(),
        Value::String(_) | Value::Object(_) | Value::Array(_) | Value::Function(_) => true,
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
        Value::Object(_) | Value::Array(_) => "[object Object]".to_string(),
        Value::Function(_) => "function".to_string(),
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
                Ok(Value::Number(f64::INFINITY))
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
        (Value::Number(x), Value::Number(y)) => !x.is_nan() && !y.is_nan() && x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Object(x), Value::Object(y)) => x == y,
        (Value::Array(x), Value::Array(y)) => x == y,
        (Value::Function(x), Value::Function(y)) => x == y,
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
