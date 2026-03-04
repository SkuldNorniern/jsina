use crate::ir::bytecode::BytecodeChunk;
use crate::runtime::Value;

pub(crate) enum BuiltinResult {
    Push(Value),
    Throw(Value),
    Invoke {
        callee: Value,
        this_arg: Value,
        args: Vec<Value>,
        new_object: Option<usize>,
    },
}

#[derive(Debug, Clone)]
pub enum Completion {
    Normal(Value),
    Return(Value),
    Throw(Value),
}

#[derive(Debug)]
pub enum VmError {
    StackUnderflow {
        chunk_index: usize,
        pc: usize,
        opcode: u8,
        stack_len: usize,
    },
    InvalidOpcode(u8),
    InvalidConstIndex(usize),
    InfiniteLoopDetected,
    Cancelled,
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::StackUnderflow {
                chunk_index,
                pc,
                opcode,
                stack_len,
            } => {
                write!(
                    f,
                    "stack underflow at chunk={} pc={} op=0x{:02x} stack_len={}",
                    chunk_index, pc, opcode, stack_len
                )
            }
            Self::InvalidOpcode(b) => write!(f, "invalid opcode: 0x{:02x}", b),
            Self::InvalidConstIndex(i) => write!(f, "invalid constant index: {}", i),
            Self::InfiniteLoopDetected => write!(f, "infinite loop detected"),
            Self::Cancelled => write!(f, "execution cancelled (timeout)"),
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
