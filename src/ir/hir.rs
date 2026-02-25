use crate::diagnostics::Span;

#[derive(Debug, Clone)]
pub struct ExceptionRegion {
    pub try_entry_block: HirBlockId,
    pub handler_block: HirBlockId,
    pub catch_slot: u32,
    pub is_finally: bool,
}

#[derive(Debug, Clone)]
pub struct HirFunction {
    pub name: Option<String>,
    pub params: Vec<String>,
    pub num_locals: u32,
    pub rest_param_index: Option<u32>,
    pub entry_block: HirBlockId,
    pub blocks: Vec<HirBlock>,
    pub exception_regions: Vec<ExceptionRegion>,
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
    LoadThis { span: Span },
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
    LeftShift { span: Span },
    RightShift { span: Span },
    UnsignedRightShift { span: Span },
    BitwiseAnd { span: Span },
    BitwiseOr { span: Span },
    BitwiseXor { span: Span },
    Instanceof { span: Span },
    Not { span: Span },
    BitwiseNot { span: Span },
    Typeof { span: Span },
    Delete { span: Span },
    NewObject { span: Span },
    NewObjectWithProto { span: Span },
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
    CallMethod { argc: u32, span: Span },
    New { func_index: u32, argc: u32, span: Span },
    Rethrow { slot: u32, span: Span },
}

/// Builtin call IDs: (category << 4) | index. Bytecode stores this u8.
/// Host=0x0, Array=0x1, Math=0x2, Json=0x3, Object=0x4, Type=0x5,
/// String=0x6, Error=0x7, RegExp=0x8, Map=0x9, Set=0xA, Collection=0xB, Date=0xC.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum BuiltinId {
    Host0 = 0x00,
    Array0 = 0x10,
    Array1 = 0x11,
    Array2 = 0x12,
    Array3 = 0x13,
    Array4 = 0x14,
    Array5 = 0x15,
    Array6 = 0x16,
    Array7 = 0x17,
    Array8 = 0x18,
    Array9 = 0x19,
    Array10 = 0x1A,
    Array11 = 0x1B,
    Math0 = 0x20,
    Math1 = 0x21,
    Math2 = 0x22,
    Math3 = 0x23,
    Math4 = 0x24,
    Math5 = 0x25,
    Math6 = 0x26,
    Math7 = 0x27,
    Math8 = 0x28,
    Json0 = 0x30,
    Json1 = 0x31,
    Object0 = 0x40,
    Object1 = 0x41,
    Object2 = 0x42,
    Object3 = 0x43,
    Object4 = 0x44,
    Object5 = 0x45,
    Object6 = 0x46,
    Object7 = 0x47,
    Object8 = 0x48,
    Object9 = 0x49,
    Object10 = 0x4A,
    Object11 = 0x4B,
    Object12 = 0x4C,
    Object13 = 0x4D,
    Object14 = 0x4E,
    Type0 = 0x50,
    Type1 = 0x51,
    Type2 = 0x52,
    Type3 = 0x53,
    Number0 = 0x54,
    String0 = 0x60,
    String1 = 0x61,
    String2 = 0x62,
    String3 = 0x63,
    String4 = 0x64,
    String5 = 0x65,
    String6 = 0x66,
    Error0 = 0x70,
    RegExp0 = 0x80,
    RegExp1 = 0x81,
    RegExp2 = 0x82,
    Map0 = 0x90,
    Map1 = 0x91,
    Map2 = 0x92,
    Map3 = 0x93,
    Set0 = 0xA0,
    Set1 = 0xA1,
    Set2 = 0xA2,
    Set3 = 0xA3,
    Collection0 = 0xB0,
    Date0 = 0xC0,
    Date1 = 0xC1,
    Date2 = 0xC2,
    Date3 = 0xC3,
    Date4 = 0xC4,
    Symbol0 = 0xD0,
    RefErr0 = 0xD1,
    TypeErr0 = 0xD2,
    RangeErr0 = 0xD3,
    SyntaxErr0 = 0xD4,
    Eval0 = 0xD9,
    EncodeUri0 = 0xDA,
    EncodeUriComponent0 = 0xDB,
    ParseInt0 = 0xDC,
    ParseFloat0 = 0xDD,
    DecodeUri0 = 0xDE,
    DecodeUriComponent0 = 0xDF,
    Int32Array0 = 0xE0,
    Uint8Array0 = 0xE1,
    Uint8ClampedArray0 = 0xE2,
    ArrayBuffer0 = 0xE3,
    Function0 = 0xE4,
    IsNaN0 = 0xE5,
    IsFinite0 = 0xE6,
    DataView0 = 0xE9,
}

#[derive(Debug, Clone)]
pub enum HirConst {
    Int(i64),
    Float(f64),
    Null,
    Undefined,
    String(String),
    Function(u32),
    Global(String),
}

#[derive(Debug, Clone)]
pub enum HirTerminator {
    Return { span: Span },
    Throw { span: Span },
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
            rest_param_index: None,
            entry_block: 0,
            blocks: vec![HirBlock {
                id: 0,
                ops: vec![HirOp::LoadConst {
                    value: HirConst::Int(42),
                    span,
                }],
                terminator: HirTerminator::Return { span },
            }],
            exception_regions: vec![],
        };
        assert_eq!(func.blocks.len(), 1);
    }
}
