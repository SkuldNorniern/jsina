use crate::ir::bytecode::{BytecodeChunk, ConstEntry, Opcode};
use crate::ir::hir::*;

#[derive(Debug)]
pub struct CompiledFunction {
    pub name: Option<String>,
    pub chunk: BytecodeChunk,
}

pub fn hir_to_bytecode(func: &HirFunction) -> CompiledFunction {
    let mut code = Vec::new();
    let mut constants = Vec::new();

    for block in &func.blocks {
        for op in &block.ops {
            match op {
                HirOp::LoadConst { value, .. } => {
                    let idx = constants.len() as u8;
                    constants.push(match value {
                        HirConst::Int(n) => ConstEntry::Int(*n),
                        HirConst::Float(n) => ConstEntry::Float(*n),
                    });
                    code.push(Opcode::PushConst as u8);
                    code.push(idx);
                }
                HirOp::Pop { .. } => {
                    code.push(Opcode::Pop as u8);
                }
                HirOp::LoadLocal { id, .. } => {
                    let slot = (*id).min(255) as u8;
                    code.push(Opcode::LoadLocal as u8);
                    code.push(slot);
                }
                HirOp::StoreLocal { id, .. } => {
                    let slot = (*id).min(255) as u8;
                    code.push(Opcode::StoreLocal as u8);
                    code.push(slot);
                }
                HirOp::Add { .. } => code.push(Opcode::Add as u8),
                HirOp::Sub { .. } => code.push(Opcode::Sub as u8),
            }
        }
        match &block.terminator {
            HirTerminator::Return { .. } => {
                code.push(Opcode::Return as u8);
            }
            HirTerminator::Jump { .. } | HirTerminator::Branch { .. } => {}
        }
    }

    CompiledFunction {
        name: func.name.clone(),
        chunk: BytecodeChunk {
            code,
            constants,
            num_locals: func.num_locals,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::Position;
    use crate::ir::hir::HirBlock;

    #[test]
    fn compile_push_return() {
        let span = crate::diagnostics::Span::point(Position::start());
        let func =         HirFunction {
            name: Some("main".to_string()),
            params: vec![],
            num_locals: 0,
            entry_block: 0,
            blocks: vec![HirBlock {
                id: 0,
                ops: vec![HirOp::LoadConst {
                    value: HirConst::Int(42),
                    span,
                }],
                terminator: HirTerminator::Return { span },
            }],
        };
        let cf = hir_to_bytecode(&func);
        assert_eq!(cf.chunk.constants.len(), 1);
        assert!(cf.chunk.code.len() >= 3);
        assert_eq!(cf.chunk.code[0], Opcode::PushConst as u8);
        assert_eq!(cf.chunk.code[cf.chunk.code.len() - 1], Opcode::Return as u8);
    }
}
