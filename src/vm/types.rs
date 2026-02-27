use crate::ir::bytecode::BytecodeChunk;
use crate::runtime::Value;

pub(crate) enum BuiltinResult {
    Push(Value),
    Throw(Value),
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
    InfiniteLoopDetected,
    Cancelled,
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::StackUnderflow => write!(f, "stack underflow"),
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
