use crate::ir::bytecode::{BytecodeChunk, ConstEntry, Opcode};
use crate::ir::hir::*;

#[derive(Debug)]
pub struct CompiledFunction {
    pub name: Option<String>,
    pub chunk: BytecodeChunk,
}

fn block_bytecode_size(block: &HirBlock, _constants_len: usize) -> usize {
    let mut size = 0;
    for op in &block.ops {
        size += match op {
            HirOp::LoadConst { .. } => 2,
            HirOp::Pop { .. } | HirOp::Dup { .. } | HirOp::Swap { .. } => 1,
            HirOp::LoadLocal { .. } | HirOp::StoreLocal { .. } => 2,
            HirOp::Add { .. } | HirOp::Sub { .. } | HirOp::Mul { .. } | HirOp::Div { .. } | HirOp::Mod { .. } | HirOp::Pow { .. }
            | HirOp::Lt { .. } | HirOp::Lte { .. } | HirOp::Gt { .. } | HirOp::Gte { .. } | HirOp::StrictEq { .. } | HirOp::StrictNotEq { .. } | HirOp::Not { .. } => 1,
            HirOp::NewObject { .. } | HirOp::NewArray { .. } => 1,
            HirOp::GetProp { .. } | HirOp::SetProp { .. } => 2,
            HirOp::GetPropDyn { .. } | HirOp::SetPropDyn { .. } => 1,
            HirOp::Call { .. } | HirOp::CallBuiltin { .. } => 3,
        };
    }
    size += match &block.terminator {
        HirTerminator::Return { .. } => 1,
        HirTerminator::Jump { .. } => 3,
        HirTerminator::Branch { .. } | HirTerminator::BranchNullish { .. } => 2 + 3 + 3,
    };
    size
}

pub fn hir_to_bytecode(func: &HirFunction) -> CompiledFunction {
    let mut constants = Vec::new();
    let mut block_offsets: Vec<usize> = vec![0];
    for block in &func.blocks {
        let size = block_bytecode_size(block, constants.len());
        block_offsets.push(block_offsets.last().copied().unwrap_or(0) + size);
    }

    let mut code = Vec::new();

    for (_block_idx, block) in func.blocks.iter().enumerate() {
        for op in &block.ops {
            match op {
                HirOp::LoadConst { value, .. } => {
                    let idx = constants.len() as u8;
                    constants.push(match value {
                        HirConst::Int(n) => ConstEntry::Int(*n),
                        HirConst::Float(n) => ConstEntry::Float(*n),
                        HirConst::Null => ConstEntry::Null,
                        HirConst::String(s) => ConstEntry::String(s.clone()),
                    });
                    code.push(Opcode::PushConst as u8);
                    code.push(idx);
                }
                HirOp::Pop { .. } => {
                    code.push(Opcode::Pop as u8);
                }
                HirOp::Dup { .. } => {
                    code.push(Opcode::Dup as u8);
                }
                HirOp::Swap { .. } => {
                    code.push(Opcode::Swap as u8);
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
                HirOp::Mul { .. } => code.push(Opcode::Mul as u8),
                HirOp::Div { .. } => code.push(Opcode::Div as u8),
                HirOp::Mod { .. } => code.push(Opcode::Mod as u8),
                HirOp::Pow { .. } => code.push(Opcode::Pow as u8),
                HirOp::Lt { .. } => code.push(Opcode::Lt as u8),
                HirOp::Lte { .. } => code.push(Opcode::Lte as u8),
                HirOp::Gt { .. } => code.push(Opcode::Gt as u8),
                HirOp::Gte { .. } => code.push(Opcode::Gte as u8),
                HirOp::StrictEq { .. } => code.push(Opcode::StrictEq as u8),
                HirOp::StrictNotEq { .. } => code.push(Opcode::StrictNotEq as u8),
                HirOp::Not { .. } => code.push(Opcode::Not as u8),
                HirOp::NewObject { .. } => code.push(Opcode::NewObject as u8),
                HirOp::NewArray { .. } => code.push(Opcode::NewArray as u8),
                HirOp::GetProp { key, .. } => {
                    let idx = constants.len();
                    constants.push(ConstEntry::String(key.clone()));
                    code.push(Opcode::GetProp as u8);
                    code.push(idx.min(255) as u8);
                }
                HirOp::SetProp { key, .. } => {
                    let idx = constants.len();
                    constants.push(ConstEntry::String(key.clone()));
                    code.push(Opcode::SetProp as u8);
                    code.push(idx.min(255) as u8);
                }
                HirOp::GetPropDyn { .. } => code.push(Opcode::GetPropDyn as u8),
                HirOp::SetPropDyn { .. } => code.push(Opcode::SetPropDyn as u8),
                HirOp::Call { func_index, argc, .. } => {
                    code.push(Opcode::Call as u8);
                    code.push((*func_index).min(255) as u8);
                    code.push((*argc).min(255) as u8);
                }
                HirOp::CallBuiltin { builtin, argc, .. } => {
                    code.push(Opcode::CallBuiltin as u8);
                    code.push(*builtin as u8);
                    code.push((*argc).min(255) as u8);
                }
            }
        }
        match &block.terminator {
            HirTerminator::Return { .. } => {
                code.push(Opcode::Return as u8);
            }
            HirTerminator::Jump { target } => {
                let target_offset = block_offsets.get(*target as usize).copied().unwrap_or(0);
                let rel = target_offset as i32 - code.len() as i32 - 3;
                code.push(Opcode::Jump as u8);
                code.extend_from_slice(&(rel as i16).to_le_bytes());
            }
            HirTerminator::Branch {
                cond,
                then_block,
                else_block,
            } => {
                let slot = (*cond).min(255) as u8;
                code.push(Opcode::LoadLocal as u8);
                code.push(slot);
                code.push(Opcode::JumpIfFalse as u8);
                let else_offset = block_offsets.get(*else_block as usize).copied().unwrap_or(0);
                let pc_after = code.len() + 2;
                let rel_else = else_offset as i32 - pc_after as i32;
                code.extend_from_slice(&(rel_else as i16).to_le_bytes());
                let then_offset = block_offsets.get(*then_block as usize).copied().unwrap_or(0);
                let pc_after_jump = code.len();
                let rel_then = then_offset as i32 - pc_after_jump as i32 - 3;
                code.push(Opcode::Jump as u8);
                code.extend_from_slice(&(rel_then as i16).to_le_bytes());
            }
            HirTerminator::BranchNullish {
                cond,
                then_block,
                else_block,
            } => {
                let slot = (*cond).min(255) as u8;
                code.push(Opcode::LoadLocal as u8);
                code.push(slot);
                code.push(Opcode::JumpIfNullish as u8);
                let then_offset = block_offsets.get(*then_block as usize).copied().unwrap_or(0);
                let pc_after = code.len() + 2;
                let rel_then = then_offset as i32 - pc_after as i32;
                code.extend_from_slice(&(rel_then as i16).to_le_bytes());
                let else_offset = block_offsets.get(*else_block as usize).copied().unwrap_or(0);
                let pc_after_jump = code.len();
                let rel_else = else_offset as i32 - pc_after_jump as i32 - 3;
                code.push(Opcode::Jump as u8);
                code.extend_from_slice(&(rel_else as i16).to_le_bytes());
            }
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
