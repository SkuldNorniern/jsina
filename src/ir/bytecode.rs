#[derive(Debug, Clone)]
pub struct ExceptionHandler {
    pub try_start: u32,
    pub try_end: u32,
    pub handler_pc: u32,
    pub catch_slot: u8,
    pub is_finally: bool,
}

#[derive(Debug, Clone)]
pub struct BytecodeChunk {
    pub code: Vec<u8>,
    pub constants: Vec<ConstEntry>,
    pub num_locals: u32,
    pub handlers: Vec<ExceptionHandler>,
}

#[derive(Debug, Clone)]
pub enum ConstEntry {
    Int(i64),
    Float(f64),
    String(String),
    Null,
    Undefined,
    Function(usize),
    Global(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
    PushConst = 0x01,
    Pop = 0x02,
    Dup = 0x06,
    Swap = 0x07,
    LoadLocal = 0x03,
    StoreLocal = 0x04,
    LoadThis = 0x05,
    Add = 0x10,
    Sub = 0x11,
    Mul = 0x12,
    Div = 0x13,
    Mod = 0x15,
    Pow = 0x16,
    Lt = 0x14,
    Lte = 0x19,
    Gt = 0x1a,
    Gte = 0x1b,
    StrictEq = 0x17,
    StrictNotEq = 0x1c,
    LeftShift = 0x1e,
    RightShift = 0x1f,
    UnsignedRightShift = 0x23,
    BitwiseAnd = 0x24,
    BitwiseOr = 0x25,
    BitwiseXor = 0x26,
    Not = 0x18,
    BitwiseNot = 0x27,
    Typeof = 0x1d,
    NewObject = 0x50,
    NewObjectWithProto = 0x56,
    NewArray = 0x51,
    GetProp = 0x52,
    SetProp = 0x53,
    GetPropDyn = 0x54,
    SetPropDyn = 0x55,
    Call = 0x40,
    CallBuiltin = 0x41,
    CallMethod = 0x42,
    New = 0x43,
    Throw = 0x21,
    Rethrow = 0x22,
    JumpIfFalse = 0x30,
    JumpIfNullish = 0x32,
    Jump = 0x31,
    Return = 0x20,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bytecode_chunk_creation() {
        let chunk = BytecodeChunk {
            code: vec![Opcode::PushConst as u8, 0, Opcode::Return as u8],
            constants: vec![ConstEntry::Int(42)],
            num_locals: 0,
            handlers: vec![],
        };
        assert_eq!(chunk.constants.len(), 1);
    }
}
