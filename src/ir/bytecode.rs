#[derive(Debug, Clone)]
pub struct BytecodeChunk {
    pub code: Vec<u8>,
    pub constants: Vec<ConstEntry>,
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
    Add = 0x10,
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
        };
        assert_eq!(chunk.constants.len(), 1);
    }
}
