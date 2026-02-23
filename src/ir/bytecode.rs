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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
    PushConst = 0x01,
    Pop = 0x02,
    LoadLocal = 0x03,
    StoreLocal = 0x04,
    Add = 0x10,
    Sub = 0x11,
    Mul = 0x12,
    Div = 0x13,
    Lt = 0x14,
    Call = 0x40,
    JumpIfFalse = 0x30,
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
