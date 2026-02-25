use crate::ir::bytecode::BytecodeChunk;
use crate::runtime::builtins;
use crate::runtime::{Heap, Value};
use super::types::{BuiltinResult, VmError};

#[inline(always)]
pub(crate) fn read_u8(code: &[u8], pc: usize) -> u8 {
    debug_assert!(pc < code.len());
    // SAFETY: Loop exits when pc >= code.len(); each opcode consumes correct operand bytes.
    unsafe { *code.get_unchecked(pc) }
}

#[inline(always)]
pub(crate) fn read_i16(code: &[u8], pc: usize) -> i16 {
    i16::from_le_bytes([read_u8(code, pc), read_u8(code, pc + 1)])
}

#[inline(always)]
pub(crate) fn execute_builtin(
    builtin_id: u8,
    argc: usize,
    stack: &mut Vec<Value>,
    ctx: &mut builtins::BuiltinContext,
) -> Result<BuiltinResult, VmError> {
    if builtin_id > builtins::MAX_BUILTIN_ID {
        return Err(VmError::InvalidOpcode(builtin_id));
    }
    let result = if argc <= 16 {
        let mut buf: [Value; 16] = [
            Value::Undefined, Value::Undefined, Value::Undefined, Value::Undefined,
            Value::Undefined, Value::Undefined, Value::Undefined, Value::Undefined,
            Value::Undefined, Value::Undefined, Value::Undefined, Value::Undefined,
            Value::Undefined, Value::Undefined, Value::Undefined, Value::Undefined,
        ];
        for i in (0..argc).rev() {
            buf[i] = stack.pop().ok_or(VmError::StackUnderflow)?;
        }
        builtins::dispatch(builtin_id, &buf[..argc], ctx)
    } else {
        let mut args: Vec<Value> = (0..argc)
            .map(|_| stack.pop().ok_or(VmError::StackUnderflow))
            .collect::<Result<Vec<_>, _>>()?;
        args.reverse();
        builtins::dispatch(builtin_id, &args, ctx)
    };
    match result {
        Ok(v) => Ok(BuiltinResult::Push(v)),
        Err(builtins::BuiltinError::Throw(v)) => Ok(BuiltinResult::Throw(v)),
    }
}

/// Pop `argc` values from the stack, returning them in left-to-right order.
/// Uses `split_off` to avoid individual pops and reversal.
#[inline]
pub(crate) fn pop_args(stack: &mut Vec<Value>, argc: usize) -> Result<Vec<Value>, VmError> {
    let start = stack.len().checked_sub(argc).ok_or(VmError::StackUnderflow)?;
    Ok(stack.split_off(start))
}

/// Build the locals vec for a callee from the provided arguments.
/// Handles rest parameters by collecting trailing args into a heap array.
pub(crate) fn setup_callee_locals(
    chunk: &BytecodeChunk,
    args: &[Value],
    heap: &mut Heap,
) -> Vec<Value> {
    let mut locals = vec![Value::Undefined; chunk.num_locals as usize];
    match chunk.rest_param_index {
        Some(r) => {
            let r = r as usize;
            for (i, v) in args.iter().take(r).enumerate() {
                if i < locals.len() {
                    locals[i] = v.clone();
                }
            }
            if r < locals.len() {
                let rest_id = heap.alloc_array();
                if r < args.len() {
                    heap.array_push_values(rest_id, &args[r..]);
                }
                locals[r] = Value::Array(rest_id);
            }
        }
        None => {
            for (i, v) in args.iter().enumerate() {
                if i < locals.len() {
                    locals[i] = v.clone();
                }
            }
        }
    }
    locals
}
