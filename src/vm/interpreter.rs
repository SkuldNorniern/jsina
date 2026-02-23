use crate::ir::bytecode::{BytecodeChunk, ConstEntry, Opcode};
use crate::runtime::Value;

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
}

pub fn interpret_program(program: &Program) -> Result<Completion, VmError> {
    let mut stack: Vec<Value> = Vec::new();
    let entry_chunk = program
        .chunks
        .get(program.entry)
        .ok_or(VmError::InvalidConstIndex(program.entry))?;
    let mut frames: Vec<Frame> = vec![Frame {
        chunk_index: program.entry,
        pc: 0,
        locals: (0..entry_chunk.num_locals).map(|_| Value::Undefined).collect(),
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
            x if x == Opcode::Return as u8 => {
                let val = stack.pop().unwrap_or(Value::Undefined);
                frames.pop();
                if frames.is_empty() {
                    return Ok(Completion::Return(val));
                }
                stack.push(val);
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
                });
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
            x if x == Opcode::Lt as u8 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = lt_values(&lhs, &rhs)?;
                stack.push(result);
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
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x.saturating_add(*y))),
        (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
        (Value::Int(x), Value::Number(y)) => Ok(Value::Number(*x as f64 + y)),
        (Value::Number(x), Value::Int(y)) => Ok(Value::Number(x + *y as f64)),
        _ => Ok(Value::Number(f64::NAN)),
    }
}

fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Undefined | Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(n) => *n != 0,
        Value::Number(n) => *n != 0.0 && !n.is_nan(),
        Value::String(_) => true,
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

impl ConstEntry {
    fn to_value(&self) -> Value {
        match self {
            ConstEntry::Int(n) => Value::Int((*n).clamp(i32::MIN as i64, i32::MAX as i64) as i32),
            ConstEntry::Float(n) => Value::Number(*n),
            ConstEntry::String(s) => Value::String(s.clone()),
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
        };
        let result = interpret(&chunk).expect("interpret");
        if let Completion::Return(v) = result {
            assert_eq!(v.to_i64(), 3);
        } else {
            panic!("expected Return(3), got {:?}", result);
        }
    }
}
