#[derive(Debug, Clone)]
pub struct BytecodeChunk {
    pub code: Vec<u8>,
    pub constants: Vec<ConstEntry>,
    pub num_locals: u32,
}

#[derive(Debug, Clone)]
pub enum ConstEntry {
    Int(i64),
    Float(f64),
    String(String),
    Null,
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
    Not = 0x18,
    NewObject = 0x50,
    NewArray = 0x51,
    GetProp = 0x52,
    SetProp = 0x53,
    GetPropDyn = 0x54,
    SetPropDyn = 0x55,
    Call = 0x40,
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
        };
        assert_eq!(chunk.constants.len(), 1);
    }
}
