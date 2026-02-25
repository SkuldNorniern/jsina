use crate::ir::bytecode::{BytecodeChunk, ConstEntry};

use super::runtime::build_constant_main_module;

const OP_PUSH_CONST: u8 = 0x01;
const OP_POP: u8 = 0x02;
const OP_ADD: u8 = 0x10;
const OP_SUB: u8 = 0x11;
const OP_MUL: u8 = 0x12;
const OP_RETURN: u8 = 0x20;

pub fn bytecode_to_lamina_trivial(chunk: &BytecodeChunk) -> Option<lamina::ir::Module<'static>> {
    let code = &chunk.code;
    let constants = &chunk.constants;

    if chunk.num_locals != 0 || !chunk.handlers.is_empty() || chunk.rest_param_index.is_some() {
        return None;
    }

    let mut pc = 0usize;
    let mut stack: Vec<i64> = Vec::with_capacity(8);

    while pc < code.len() {
        let op = *code.get(pc)?;
        pc += 1;

        match op {
            OP_PUSH_CONST => {
                let idx = *code.get(pc)? as usize;
                pc += 1;
                let value = match constants.get(idx)? {
                    ConstEntry::Int(n) => *n,
                    _ => return None,
                };
                stack.push(value);
            }
            OP_POP => {
                stack.pop()?;
            }
            OP_ADD => {
                let rhs = stack.pop()?;
                let lhs = stack.pop()?;
                stack.push(lhs.saturating_add(rhs));
            }
            OP_SUB => {
                let rhs = stack.pop()?;
                let lhs = stack.pop()?;
                stack.push(lhs.saturating_sub(rhs));
            }
            OP_MUL => {
                let rhs = stack.pop()?;
                let lhs = stack.pop()?;
                stack.push(lhs.saturating_mul(rhs));
            }
            OP_RETURN => {
                let result = stack.pop().unwrap_or(0);
                if pc != code.len() {
                    return None;
                }
                return Some(build_constant_main_module(result));
            }
            _ => return None,
        }
    }

    let result = stack.pop().unwrap_or(0);
    Some(build_constant_main_module(result))
}
