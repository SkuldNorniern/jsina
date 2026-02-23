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

pub fn interpret(chunk: &BytecodeChunk) -> Result<Completion, VmError> {
    let mut stack: Vec<Value> = Vec::new();
    let mut pc: usize = 0;
    let code = &chunk.code;
    let constants = &chunk.constants;

    loop {
        if pc >= code.len() {
            break;
        }

        let op = code[pc];
        pc += 1;

        match op {
            x if x == Opcode::PushConst as u8 => {
                let idx = *code.get(pc).ok_or(VmError::StackUnderflow)? as usize;
                pc += 1;
                let val = constants
                    .get(idx)
                    .ok_or(VmError::InvalidConstIndex(idx))?
                    .to_value();
                stack.push(val);
            }
            x if x == Opcode::Pop as u8 => {
                stack.pop().ok_or(VmError::StackUnderflow)?;
            }
            x if x == Opcode::Return as u8 => {
                let val = stack.pop().unwrap_or(Value::Undefined);
                return Ok(Completion::Return(val));
            }
            x if x == Opcode::Add as u8 => {
                let rhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let lhs = stack.pop().ok_or(VmError::StackUnderflow)?;
                let result = add_values(&lhs, &rhs)?;
                stack.push(result);
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
        };
        let result = interpret(&chunk).expect("interpret");
        if let Completion::Return(Value::Int(42)) = result {
        } else {
            panic!("expected Return(42), got {:?}", result);
        }
    }
}
