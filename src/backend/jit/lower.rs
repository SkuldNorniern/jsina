use crate::ir::bytecode::{BytecodeChunk, ConstEntry, Opcode};

use super::runtime::build_constant_main_module;

const OP_PUSH_CONST: u8 = Opcode::PushConst as u8;
const OP_POP: u8 = Opcode::Pop as u8;
const OP_DUP: u8 = Opcode::Dup as u8;
const OP_SWAP: u8 = Opcode::Swap as u8;
const OP_ADD: u8 = Opcode::Add as u8;
const OP_SUB: u8 = Opcode::Sub as u8;
const OP_MUL: u8 = Opcode::Mul as u8;
const OP_LT: u8 = Opcode::Lt as u8;
const OP_LTE: u8 = Opcode::Lte as u8;
const OP_GT: u8 = Opcode::Gt as u8;
const OP_GTE: u8 = Opcode::Gte as u8;
const OP_STRICT_EQ: u8 = Opcode::StrictEq as u8;
const OP_STRICT_NOT_EQ: u8 = Opcode::StrictNotEq as u8;
const OP_NOT: u8 = Opcode::Not as u8;
const OP_LEFT_SHIFT: u8 = Opcode::LeftShift as u8;
const OP_RIGHT_SHIFT: u8 = Opcode::RightShift as u8;
const OP_UNSIGNED_RIGHT_SHIFT: u8 = Opcode::UnsignedRightShift as u8;
const OP_BITWISE_AND: u8 = Opcode::BitwiseAnd as u8;
const OP_BITWISE_OR: u8 = Opcode::BitwiseOr as u8;
const OP_BITWISE_XOR: u8 = Opcode::BitwiseXor as u8;
const OP_BITWISE_NOT: u8 = Opcode::BitwiseNot as u8;
const OP_RETURN: u8 = Opcode::Return as u8;

#[derive(Clone, Copy)]
enum EvalValue {
    Int(i64),
    Bool(bool),
}

impl EvalValue {
    #[inline(always)]
    fn as_return_i64(self) -> i64 {
        match self {
            Self::Int(v) => v,
            Self::Bool(v) => i64::from(v),
        }
    }

    #[inline(always)]
    fn as_i32(self) -> i32 {
        match self {
            Self::Int(v) => v as i32,
            Self::Bool(v) => i32::from(v),
        }
    }

    #[inline(always)]
    fn is_truthy(self) -> bool {
        match self {
            Self::Int(v) => v != 0,
            Self::Bool(v) => v,
        }
    }
}

#[inline(always)]
fn pop2(stack: &mut Vec<EvalValue>) -> Option<(EvalValue, EvalValue)> {
    let rhs = stack.pop()?;
    let lhs = stack.pop()?;
    Some((lhs, rhs))
}

#[inline(always)]
fn strict_eq(lhs: EvalValue, rhs: EvalValue) -> bool {
    match (lhs, rhs) {
        (EvalValue::Int(a), EvalValue::Int(b)) => a == b,
        (EvalValue::Bool(a), EvalValue::Bool(b)) => a == b,
        _ => false,
    }
}

pub fn bytecode_to_lamina_trivial(chunk: &BytecodeChunk) -> Option<lamina::ir::Module<'static>> {
    let code = &chunk.code;
    let constants = &chunk.constants;

    if chunk.num_locals != 0 || !chunk.handlers.is_empty() || chunk.rest_param_index.is_some() {
        return None;
    }

    let mut pc = 0usize;
    let mut stack: Vec<EvalValue> = Vec::with_capacity(8);

    while pc < code.len() {
        let op = *code.get(pc)?;
        pc += 1;

        match op {
            OP_PUSH_CONST => {
                let idx = *code.get(pc)? as usize;
                pc += 1;
                let value = match constants.get(idx)? {
                    ConstEntry::Int(n) => EvalValue::Int(*n),
                    _ => return None,
                };
                stack.push(value);
            }
            OP_POP => {
                stack.pop()?;
            }
            OP_DUP => {
                let top = *stack.last()?;
                stack.push(top);
            }
            OP_SWAP => {
                let len = stack.len();
                if len < 2 {
                    return None;
                }
                stack.swap(len - 1, len - 2);
            }
            OP_ADD => {
                let (lhs, rhs) = pop2(&mut stack)?;
                let (EvalValue::Int(lhs), EvalValue::Int(rhs)) = (lhs, rhs) else {
                    return None;
                };
                stack.push(EvalValue::Int(lhs.saturating_add(rhs)));
            }
            OP_SUB => {
                let (lhs, rhs) = pop2(&mut stack)?;
                let (EvalValue::Int(lhs), EvalValue::Int(rhs)) = (lhs, rhs) else {
                    return None;
                };
                stack.push(EvalValue::Int(lhs.saturating_sub(rhs)));
            }
            OP_MUL => {
                let (lhs, rhs) = pop2(&mut stack)?;
                let (EvalValue::Int(lhs), EvalValue::Int(rhs)) = (lhs, rhs) else {
                    return None;
                };
                stack.push(EvalValue::Int(lhs.saturating_mul(rhs)));
            }
            OP_LT => {
                let (lhs, rhs) = pop2(&mut stack)?;
                let (EvalValue::Int(lhs), EvalValue::Int(rhs)) = (lhs, rhs) else {
                    return None;
                };
                stack.push(EvalValue::Bool(lhs < rhs));
            }
            OP_LTE => {
                let (lhs, rhs) = pop2(&mut stack)?;
                let (EvalValue::Int(lhs), EvalValue::Int(rhs)) = (lhs, rhs) else {
                    return None;
                };
                stack.push(EvalValue::Bool(lhs <= rhs));
            }
            OP_GT => {
                let (lhs, rhs) = pop2(&mut stack)?;
                let (EvalValue::Int(lhs), EvalValue::Int(rhs)) = (lhs, rhs) else {
                    return None;
                };
                stack.push(EvalValue::Bool(lhs > rhs));
            }
            OP_GTE => {
                let (lhs, rhs) = pop2(&mut stack)?;
                let (EvalValue::Int(lhs), EvalValue::Int(rhs)) = (lhs, rhs) else {
                    return None;
                };
                stack.push(EvalValue::Bool(lhs >= rhs));
            }
            OP_STRICT_EQ => {
                let (lhs, rhs) = pop2(&mut stack)?;
                stack.push(EvalValue::Bool(strict_eq(lhs, rhs)));
            }
            OP_STRICT_NOT_EQ => {
                let (lhs, rhs) = pop2(&mut stack)?;
                stack.push(EvalValue::Bool(!strict_eq(lhs, rhs)));
            }
            OP_NOT => {
                let value = stack.pop()?;
                stack.push(EvalValue::Bool(!value.is_truthy()));
            }
            OP_LEFT_SHIFT => {
                let (lhs, rhs) = pop2(&mut stack)?;
                let value = lhs.as_i32().wrapping_shl(rhs.as_i32() as u32);
                stack.push(EvalValue::Int(value as i64));
            }
            OP_RIGHT_SHIFT => {
                let (lhs, rhs) = pop2(&mut stack)?;
                let value = lhs.as_i32().wrapping_shr(rhs.as_i32() as u32);
                stack.push(EvalValue::Int(value as i64));
            }
            OP_UNSIGNED_RIGHT_SHIFT => {
                let (lhs, rhs) = pop2(&mut stack)?;
                let value = (lhs.as_i32() as u32).wrapping_shr(rhs.as_i32() as u32) as i32;
                stack.push(EvalValue::Int(value as i64));
            }
            OP_BITWISE_AND => {
                let (lhs, rhs) = pop2(&mut stack)?;
                stack.push(EvalValue::Int((lhs.as_i32() & rhs.as_i32()) as i64));
            }
            OP_BITWISE_OR => {
                let (lhs, rhs) = pop2(&mut stack)?;
                stack.push(EvalValue::Int((lhs.as_i32() | rhs.as_i32()) as i64));
            }
            OP_BITWISE_XOR => {
                let (lhs, rhs) = pop2(&mut stack)?;
                stack.push(EvalValue::Int((lhs.as_i32() ^ rhs.as_i32()) as i64));
            }
            OP_BITWISE_NOT => {
                let value = stack.pop()?;
                stack.push(EvalValue::Int((!value.as_i32()) as i64));
            }
            OP_RETURN => {
                let result = stack.pop().unwrap_or(EvalValue::Int(0)).as_return_i64();
                if pc != code.len() {
                    return None;
                }
                return Some(build_constant_main_module(result));
            }
            _ => return None,
        }
    }

    let result = stack.pop().unwrap_or(EvalValue::Int(0)).as_return_i64();
    Some(build_constant_main_module(result))
}
