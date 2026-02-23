use crate::diagnostics::Span;

#[derive(Debug, Clone)]
pub struct HirFunction {
    pub name: Option<String>,
    pub params: Vec<String>,
    pub num_locals: u32,
    pub entry_block: HirBlockId,
    pub blocks: Vec<HirBlock>,
}

pub type HirBlockId = u32;

#[derive(Debug, Clone)]
pub struct HirBlock {
    pub id: HirBlockId,
    pub ops: Vec<HirOp>,
    pub terminator: HirTerminator,
}

#[derive(Debug, Clone)]
pub enum HirOp {
    LoadConst { value: HirConst, span: Span },
    LoadLocal { id: u32, span: Span },
    StoreLocal { id: u32, span: Span },
    Add { span: Span },
    Sub { span: Span },
    Mul { span: Span },
    Div { span: Span },
    Mod { span: Span },
    Pow { span: Span },
    Lt { span: Span },
    Lte { span: Span },
    Gt { span: Span },
    Gte { span: Span },
    StrictEq { span: Span },
    StrictNotEq { span: Span },
    Not { span: Span },
    NewObject { span: Span },
    NewArray { span: Span },
    GetProp { key: String, span: Span },
    SetProp { key: String, span: Span },
    GetPropDyn { span: Span },
    SetPropDyn { span: Span },
    Pop { span: Span },
    Dup { span: Span },
    Swap { span: Span },
    Call { func_index: u32, argc: u32, span: Span },
    CallBuiltin { builtin: BuiltinId, argc: u32, span: Span },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinId {
    Print = 0,
    ArrayPush = 1,
}

#[derive(Debug, Clone)]
pub enum HirConst {
    Int(i64),
    Float(f64),
    Null,
    String(String),
}

#[derive(Debug, Clone)]
pub enum HirTerminator {
    Return { span: Span },
    Jump { target: HirBlockId },
    Branch { cond: u32, then_block: HirBlockId, else_block: HirBlockId },
    BranchNullish { cond: u32, then_block: HirBlockId, else_block: HirBlockId },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::Position;

    #[test]
    fn hir_function_create() {
        let span = Span::point(Position::start());
        let func = HirFunction {
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
        assert_eq!(func.blocks.len(), 1);
    }
}
