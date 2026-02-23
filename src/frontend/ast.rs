use crate::diagnostics::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

#[derive(Debug, Clone)]
pub struct Script {
    pub id: NodeId,
    pub span: Span,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Return(ReturnStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Expression(ExpressionStmt),
    VarDecl(VarDeclStmt),
    LetDecl(LetDeclStmt),
    ConstDecl(ConstDeclStmt),
    FunctionDecl(FunctionDeclStmt),
}

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub id: NodeId,
    pub span: Span,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub id: NodeId,
    pub span: Span,
    pub condition: Box<Expression>,
    pub then_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub id: NodeId,
    pub span: Span,
    pub condition: Box<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub id: NodeId,
    pub span: Span,
    pub init: Option<Box<Statement>>,
    pub condition: Option<Box<Expression>>,
    pub update: Option<Box<Expression>>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub id: NodeId,
    pub span: Span,
    pub argument: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct BreakStmt {
    pub id: NodeId,
    pub span: Span,
    pub label: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ContinueStmt {
    pub id: NodeId,
    pub span: Span,
    pub label: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ExpressionStmt {
    pub id: NodeId,
    pub span: Span,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct VarDeclStmt {
    pub id: NodeId,
    pub span: Span,
    pub declarations: Vec<VarDeclarator>,
}

#[derive(Debug, Clone)]
pub struct LetDeclStmt {
    pub id: NodeId,
    pub span: Span,
    pub declarations: Vec<VarDeclarator>,
}

#[derive(Debug, Clone)]
pub struct ConstDeclStmt {
    pub id: NodeId,
    pub span: Span,
    pub declarations: Vec<VarDeclarator>,
}

#[derive(Debug, Clone)]
pub struct VarDeclarator {
    pub id: NodeId,
    pub span: Span,
    pub name: String,
    pub init: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclStmt {
    pub id: NodeId,
    pub span: Span,
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(LiteralExpr),
    Identifier(IdentifierExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Call(CallExpr),
    Assign(AssignExpr),
}

#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub id: NodeId,
    pub span: Span,
    pub value: LiteralValue,
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Null,
    True,
    False,
    Number(f64),
    Int(i64),
    String(String),
}

#[derive(Debug, Clone)]
pub struct IdentifierExpr {
    pub id: NodeId,
    pub span: Span,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub id: NodeId,
    pub span: Span,
    pub op: BinaryOp,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    NotEq,
    StrictEq,
    StrictNotEq,
    Lt,
    Lte,
    Gt,
    Gte,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub id: NodeId,
    pub span: Span,
    pub op: UnaryOp,
    pub argument: Box<Expression>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Minus,
    Plus,
    LogicalNot,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub id: NodeId,
    pub span: Span,
    pub callee: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub id: NodeId,
    pub span: Span,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl Script {
    pub fn span(&self) -> Span {
        self.span
    }
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::Block(s) => s.span,
            Statement::If(s) => s.span,
            Statement::While(s) => s.span,
            Statement::For(s) => s.span,
            Statement::Return(s) => s.span,
            Statement::Break(s) => s.span,
            Statement::Continue(s) => s.span,
            Statement::Expression(s) => s.span,
            Statement::VarDecl(s) => s.span,
            Statement::LetDecl(s) => s.span,
            Statement::ConstDecl(s) => s.span,
            Statement::FunctionDecl(s) => s.span,
        }
    }
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::Literal(e) => e.span,
            Expression::Identifier(e) => e.span,
            Expression::Binary(e) => e.span,
            Expression::Unary(e) => e.span,
            Expression::Call(e) => e.span,
            Expression::Assign(e) => e.span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn node_id_equality() {
        assert_eq!(NodeId(0), NodeId(0));
        assert_ne!(NodeId(0), NodeId(1));
    }

    #[test]
    fn literal_value_variants() {
        let _ = LiteralValue::Int(42);
        let _ = LiteralValue::Number(3.14);
        let _ = LiteralValue::String("hi".to_string());
    }
}
