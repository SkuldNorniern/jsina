use crate::ir::bytecode::{BytecodeChunk, ConstEntry, Opcode};

pub fn disassemble(chunk: &BytecodeChunk) -> String {
    let mut out = String::new();
    let mut pc: usize = 0;
    let code = &chunk.code;

    while pc < code.len() {
        let line_start = pc;
        let op = code[pc];
        pc += 1;

        let line = match op {
            x if x == Opcode::PushConst as u8 => {
                let idx = code.get(pc).copied().unwrap_or(0) as usize;
                pc += 1;
                let const_val = chunk.constants.get(idx).map(format_const).unwrap_or_else(|| "?".to_string());
                format!("  {:04}  PushConst  {}  ; {}", line_start, idx, const_val)
            }
            x if x == Opcode::Pop as u8 => format!("  {:04}  Pop", line_start),
            x if x == Opcode::LoadLocal as u8 => {
                let slot = code.get(pc).copied().unwrap_or(0);
                pc += 1;
                format!("  {:04}  LoadLocal  {}", line_start, slot)
            }
            x if x == Opcode::StoreLocal as u8 => {
                let slot = code.get(pc).copied().unwrap_or(0);
                pc += 1;
                format!("  {:04}  StoreLocal  {}", line_start, slot)
            }
            x if x == Opcode::Add as u8 => format!("  {:04}  Add", line_start),
            x if x == Opcode::Sub as u8 => format!("  {:04}  Sub", line_start),
            x if x == Opcode::JumpIfFalse as u8 => {
                let offset = code.get(pc..pc + 2)
                    .map(|b| i16::from_le_bytes([b[0], b[1]]) as i32)
                    .unwrap_or(0);
                pc += 2;
                format!("  {:04}  JumpIfFalse  {}", line_start, offset)
            }
            x if x == Opcode::Jump as u8 => {
                let offset = code.get(pc..pc + 2)
                    .map(|b| i16::from_le_bytes([b[0], b[1]]) as i32)
                    .unwrap_or(0);
                pc += 2;
                format!("  {:04}  Jump  {}", line_start, offset)
            }
            x if x == Opcode::Return as u8 => format!("  {:04}  Return", line_start),
            _ => format!("  {:04}  <unknown 0x{:02x}>", line_start, op),
        };
        out.push_str(&line);
        out.push('\n');
    }

    out
}

fn format_const(c: &ConstEntry) -> String {
    match c {
        ConstEntry::Int(n) => n.to_string(),
        ConstEntry::Float(n) => n.to_string(),
        ConstEntry::String(s) => format!("{:?}", s),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn disasm_push_return() {
        let chunk = BytecodeChunk {
            code: vec![Opcode::PushConst as u8, 0, Opcode::Return as u8],
            constants: vec![ConstEntry::Int(42)],
            num_locals: 0,
        };
        let s = disassemble(&chunk);
        assert!(s.contains("PushConst"));
        assert!(s.contains("42"));
        assert!(s.contains("Return"));
    }
}
