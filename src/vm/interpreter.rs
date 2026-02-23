use crate::ir::bytecode::{BytecodeChunk, ConstEntry, Opcode};
use crate::runtime::{Heap, Value};
use std::io::Write;

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
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VmError::StackUnderflow => write!(f, "stack underflow"),
            VmError::InvalidOpcode(b) => write!(f, "invalid opcode: 0x{:02x}", b),
            VmError::InvalidConstIndex(i) => write!(f, "invalid constant index: {}", i),
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
}

pub fn interpret_program(program: &Program) -> Result<Completion, VmError> {
    let mut heap = Heap::new();
    let mut stack: Vec<Value> = Vec::new();
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
        *pc += 1;

        match op {
            x if x == Opcode::PushConst as u8 => {
                let idx = *code.get(*pc).ok_or(VmError::StackUnderflow)? as usize;
                *pc += 1;
                let val = constants
                    .get(idx)
                    .ok_or(VmError::InvalidConstIndex(idx))?
                    .to_value();
                stack.push(val);
            }
            x if x == Opcode::Pop as u8 => {
                stack.pop().ok_or(VmError::StackUnderflow)?;
            }
            x if x == Opcode::Dup as u8 => {
                let top = stack.last().cloned().ok_or(VmError::StackUnderflow)?;
                stack.push(top);
            }
            x if x == Opcode::Swap as u8 => {
                let b = stack.pop().ok_or(VmError::StackUnderflow)?;
                let a = stack.pop().ok_or(VmError::StackUnderflow)?;
                stack.push(b);
                stack.push(a);
            }
            x if x == Opcode::LoadLocal as u8 => {
                let slot = *code.get(*pc).ok_or(VmError::StackUnderflow)? as usize;
                *pc += 1;
                let val = locals.get(slot).cloned().unwrap_or(Value::Undefined);
                stack.push(val);
            }
            x if x == Opcode::StoreLocal as u8 => {
                let slot = *code.get(*pc).ok_or(VmError::StackUnderflow)? as usize;
                *pc += 1;
                let val = stack.pop().ok_or(VmError::StackUnderflow)?;
                if slot < locals.len() {
                    locals[slot] = val;
                }
            }
            x if x == Opcode::LoadThis as u8 => {
                stack.push(frame.this_value.clone());
            }
            x if x == Opcode::Return as u8 => {
                let val = stack.pop().unwrap_or(Value::Undefined);
                frames.pop();
                if frames.is_empty() {
                    return Ok(Completion::Return(val));
                }
                stack.push(val);
            }
            x if x == Opcode::Throw as u8 => {
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
            x if x == Opcode::Rethrow as u8 => {
                let slot = *code.get(*pc).ok_or(VmError::StackUnderflow)? as usize;
                *pc += 1;
                if frame.rethrow_after_finally {
                    frame.rethrow_after_finally = false;
                    let val = locals.get(slot).cloned().unwrap_or(Value::Undefined);
                    return Ok(Completion::Throw(val));
                }
            }
            x if x == Opcode::Call as u8 => {
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
                frames.push(Frame {
                    chunk_index: func_idx,
                    pc: 0,
                    locals: callee_locals,
                    this_value: Value::Undefined,
                    rethrow_after_finally: false,
                });
            }
            x if x == Opcode::CallBuiltin as u8 => {
                let builtin_id = *code.get(*pc).ok_or(VmError::StackUnderflow)?;
                let argc = *code.get(*pc + 1).ok_or(VmError::StackUnderflow)? as usize;
                *pc += 2;
                let mut args: Vec<Value> = (0..argc)
                    .map(|_| stack.pop().ok_or(VmError::StackUnderflow))
                    .collect::<Result<Vec<_>, _>>()?;
                args.reverse();
                match builtin_id {
                    0 => {
                        let mut out = std::io::stdout();
                        for (i, v) in args.iter().enumerate() {
                            if i > 0 {
                                let _ = write!(out, " ");
                            }
                            let _ = write!(out, "{}", v);
                        }
                        let _ = writeln!(out);
                        let _ = out.flush();
                        stack.push(Value::Undefined);
                    }
                    1 => {
                        let (arr, vals) = args.split_first().ok_or(VmError::StackUnderflow)?;
                        let arr_id = match arr {
                            Value::Array(id) => *id,
                            _ => {
                                stack.push(Value::Undefined);
                                continue;
                            }
                        };
                        let new_len = heap.array_push_values(arr_id, vals);
                        stack.push(Value::Int(new_len));
                    }
                    2 => {
                        let arr = args.first().ok_or(VmError::StackUnderflow)?;
                        let arr_id = match arr {
                            Value::Array(id) => *id,
                            _ => {
                                stack.push(Value::Undefined);
                                continue;
                            }
                        };
                        let val = heap.array_pop(arr_id);
                        stack.push(val);
                    }
                    3 => {
                        let x = args.first().ok_or(VmError::StackUnderflow)?;
                        let n = match x {
                            Value::Int(i) => *i as f64,
                            Value::Number(n) => *n,
                            _ => f64::NAN,
                        };
                        let result = n.floor();
                        if result.fract() == 0.0 && result >= i32::MIN as f64 && result <= i32::MAX as f64 {
                            stack.push(Value::Int(result as i32));
                        } else {
                            stack.push(Value::Number(result));
                        }
                    }
                    4 => {
                        let x = args.first().ok_or(VmError::StackUnderflow)?;
                        let result = match x {
                            Value::Int(i) => Value::Int(if *i < 0 { -(*i) } else { *i }),
                            Value::Number(n) => Value::Number(n.abs()),
                            _ => Value::Number(f64::NAN),
                        };
                        stack.push(result);
                    }
                    5 => {
                        let nums: Vec<f64> = args.iter().map(value_to_number).collect();
                        let result = if nums.is_empty() {
                            Value::Number(f64::INFINITY)
                        } else {
                            let m = nums.iter().fold(f64::INFINITY, |a, &b| a.min(b));
                            if m.is_finite() && m.fract() == 0.0 && m >= i32::MIN as f64 && m <= i32::MAX as f64 {
                                Value::Int(m as i32)
                            } else {
                                Value::Number(m)
                            }
                        };
                        stack.push(result);
                    }
                    6 => {
                        let nums: Vec<f64> = args.iter().map(value_to_number).collect();
                        let result = if nums.is_empty() {
                            Value::Number(f64::NEG_INFINITY)
                        } else {
                            let m = nums.iter().fold(f64::NEG_INFINITY, |a, &b| a.max(b));
                            if m.is_finite() && m.fract() == 0.0 && m >= i32::MIN as f64 && m <= i32::MAX as f64 {
                                Value::Int(m as i32)
                            } else {
                                Value::Number(m)
                            }
                        };
                        stack.push(result);
                    }
                    7 => {
                        let arg = args.first().ok_or(VmError::StackUnderflow)?;
                        let s = match arg {
                            Value::String(s) => s.clone(),
                            _ => {
                                return Ok(Completion::Throw(Value::String(
                                    "JSON.parse requires a string".to_string(),
                                )));
                            }
                        };
                        match crate::runtime::json_parse(&s, &mut heap) {
                            Ok(v) => stack.push(v),
                            Err(e) => {
                                return Ok(Completion::Throw(Value::String(e.message)));
                            }
                        }
                    }
                    8 => {
                        let arg = args.first().ok_or(VmError::StackUnderflow)?;
                        match crate::runtime::json_stringify(arg, &heap) {
                            Some(s) => stack.push(Value::String(s)),
                            None => stack.push(Value::Undefined),
                        }
                    }
                    9 => {
                        let proto = args.first().ok_or(VmError::StackUnderflow)?;
                        let prototype = match proto {
                            Value::Null | Value::Undefined => None,
                            Value::Object(id) => Some(*id),
                            _ => None,
                        };
                        let id = heap.alloc_object_with_prototype(prototype);
                        stack.push(Value::Object(id));
                    }
                    10 => {
                        let arg = args.first().ok_or(VmError::StackUnderflow)?;
                        let is_arr = matches!(arg, Value::Array(_));
                        stack.push(Value::Bool(is_arr));
                    }
                    11 => {
                        let arg = args.first().ok_or(VmError::StackUnderflow)?;
                        let arr_id = heap.alloc_array();
                        if let Value::Object(obj_id) = arg {
                            for key in heap.object_keys(*obj_id) {
                                heap.array_push(arr_id, Value::String(key));
                            }
                        }
                        stack.push(Value::Array(arr_id));
                    }
                    _ => return Err(VmError::InvalidOpcode(builtin_id)),
                }
            }
            x if x == Opcode::Add as u8 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = add_values(&lhs, &rhs)?;
                stack.push(result);
            }
            x if x == Opcode::Sub as u8 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = sub_values(&lhs, &rhs)?;
                stack.push(result);
            }
            x if x == Opcode::Mul as u8 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = mul_values(&lhs, &rhs)?;
                stack.push(result);
            }
            x if x == Opcode::Div as u8 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = div_values(&lhs, &rhs)?;
                stack.push(result);
            }
            x if x == Opcode::Mod as u8 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = mod_values(&lhs, &rhs)?;
                stack.push(result);
            }
            x if x == Opcode::Pow as u8 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = pow_values(&lhs, &rhs)?;
                stack.push(result);
            }
            x if x == Opcode::Lt as u8 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = lt_values(&lhs, &rhs)?;
                stack.push(result);
            }
            x if x == Opcode::Lte as u8 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = lte_values(&lhs, &rhs)?;
                stack.push(result);
            }
            x if x == Opcode::Gt as u8 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = gt_values(&lhs, &rhs)?;
                stack.push(result);
            }
            x if x == Opcode::Gte as u8 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = gte_values(&lhs, &rhs)?;
                stack.push(result);
            }
            x if x == Opcode::StrictEq as u8 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = strict_eq_values(&lhs, &rhs);
                stack.push(result);
            }
            x if x == Opcode::StrictNotEq as u8 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let eq = strict_eq_values(&lhs, &rhs);
                let result = Value::Bool(!matches!(eq, Value::Bool(true)));
                stack.push(result);
            }
            x if x == Opcode::Not as u8 => {
                let val = stack.pop().ok_or(VmError::StackUnderflow)?;
                stack.push(Value::Bool(!is_truthy(&val)));
            }
            x if x == Opcode::Typeof as u8 => {
                let val = stack.pop().ok_or(VmError::StackUnderflow)?;
                let s = match &val {
                    Value::Undefined => "undefined",
                    Value::Null => "object",
                    Value::Bool(_) => "boolean",
                    Value::Int(_) | Value::Number(_) => "number",
                    Value::String(_) => "string",
                    Value::Object(_) | Value::Array(_) => "object",
                };
                stack.push(Value::String(s.to_string()));
            }
            x if x == Opcode::NewObject as u8 => {
                let id = heap.alloc_object();
                stack.push(Value::Object(id));
            }
            x if x == Opcode::NewArray as u8 => {
                let id = heap.alloc_array();
                stack.push(Value::Array(id));
            }
            x if x == Opcode::GetProp as u8 => {
                let key_idx = *code.get(*pc).ok_or(VmError::StackUnderflow)? as usize;
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
                    Value::Object(id) => heap.get_prop(*id, &key_str),
                    Value::Array(id) => heap.get_array_prop(*id, &key_str),
                    _ => Value::Undefined,
                };
                stack.push(result);
            }
            x if x == Opcode::SetProp as u8 => {
                let key_idx = *code.get(*pc).ok_or(VmError::StackUnderflow)? as usize;
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
                    Value::Object(id) => heap.set_prop(*id, &key_str, value.clone()),
                    Value::Array(id) => heap.set_array_prop(*id, &key_str, value.clone()),
                    _ => {}
                }
                stack.push(value);
            }
            x if x == Opcode::GetPropDyn as u8 => {
                let key = stack.pop().ok_or(VmError::StackUnderflow)?;
                let obj = stack.pop().ok_or(VmError::StackUnderflow)?;
                let key_str = value_to_prop_key(&key);
                let result = match &obj {
                    Value::Object(id) => heap.get_prop(*id, &key_str),
                    Value::Array(id) => heap.get_array_prop(*id, &key_str),
                    _ => Value::Undefined,
                };
                stack.push(result);
            }
            x if x == Opcode::SetPropDyn as u8 => {
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
            x if x == Opcode::JumpIfFalse as u8 => {
                let offset_bytes = code.get(*pc..*pc + 2).ok_or(VmError::StackUnderflow)?;
                *pc += 2;
                let offset = i16::from_le_bytes([offset_bytes[0], offset_bytes[1]]) as isize;
                let val = stack.pop().ok_or(VmError::StackUnderflow)?;
                if !is_truthy(&val) {
                    *pc = (*pc as isize + offset) as usize;
                }
            }
            x if x == Opcode::JumpIfNullish as u8 => {
                let offset_bytes = code.get(*pc..*pc + 2).ok_or(VmError::StackUnderflow)?;
                *pc += 2;
                let offset = i16::from_le_bytes([offset_bytes[0], offset_bytes[1]]) as isize;
                let val = stack.pop().ok_or(VmError::StackUnderflow)?;
                if is_nullish(&val) {
                    *pc = (*pc as isize + offset) as usize;
                }
            }
            x if x == Opcode::Jump as u8 => {
                let offset_bytes = code.get(*pc..*pc + 2).ok_or(VmError::StackUnderflow)?;
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
        Value::String(_) | Value::Object(_) | Value::Array(_) => true,
    }
}

fn value_to_number(v: &Value) -> f64 {
    match v {
        Value::Int(n) => *n as f64,
        Value::Number(n) => *n,
        Value::Bool(b) => if *b { 1.0 } else { 0.0 },
        Value::Null => 0.0,
        Value::Undefined => f64::NAN,
        Value::String(s) => s.parse().unwrap_or_else(|_| f64::NAN),
        Value::Object(_) | Value::Array(_) => f64::NAN,
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
