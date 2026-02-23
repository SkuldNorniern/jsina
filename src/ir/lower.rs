use crate::diagnostics::Span;
use crate::frontend::ast::*;
use crate::ir::hir::*;

#[derive(Debug)]
pub enum LowerError {
    Unsupported(String, Option<Span>),
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LowerError::Unsupported(msg, span) => {
                let loc = span.map(|s| format!(" at {}", s)).unwrap_or_default();
                write!(f, "unsupported: {}{}", msg, loc)
            }
        }
    }
}

impl std::error::Error for LowerError {}

pub fn script_to_hir(script: &Script) -> Result<Vec<HirFunction>, LowerError> {
    let mut functions = Vec::new();
    for stmt in &script.body {
        if let Statement::FunctionDecl(f) = stmt {
            functions.push(compile_function(f)?);
        }
    }
    Ok(functions)
}

fn compile_function(f: &FunctionDeclStmt) -> Result<HirFunction, LowerError> {
    let span = f.span;
    let mut blocks = Vec::new();
    let mut ops = Vec::new();
    let mut return_span = span;

    compile_statement(&f.body, &mut ops, &mut return_span)?;

    blocks.push(HirBlock {
        id: 0,
        ops,
        terminator: HirTerminator::Return { span: return_span },
    });

    Ok(HirFunction {
        name: Some(f.name.clone()),
        params: f.params.clone(),
        entry_block: 0,
        blocks,
    })
}

fn compile_statement(stmt: &Statement, ops: &mut Vec<HirOp>, return_span: &mut Span) -> Result<(), LowerError> {
    match stmt {
        Statement::Block(b) => {
            for s in &b.body {
                compile_statement(s, ops, return_span)?;
            }
        }
        Statement::Return(r) => {
            *return_span = r.span;
            if let Some(ref expr) = r.argument {
                compile_expression(expr, ops)?;
            }
        }
        Statement::Expression(e) => {
            compile_expression(&e.expression, ops)?;
            ops.push(HirOp::Pop { span: e.span });
        }
        Statement::FunctionDecl(_) => {}
        Statement::If(_) | Statement::While(_) | Statement::For(_)
        | Statement::VarDecl(_) | Statement::LetDecl(_) | Statement::ConstDecl(_)
        | Statement::Break(_) | Statement::Continue(_) => {
            return Err(LowerError::Unsupported(
                format!("{:?} not yet supported", stmt),
                Some(stmt.span()),
            ));
        }
    }
    Ok(())
}

fn compile_expression(expr: &Expression, ops: &mut Vec<HirOp>) -> Result<(), LowerError> {
    match expr {
        Expression::Literal(e) => {
            let value = match &e.value {
                LiteralValue::Int(n) => HirConst::Int(*n),
                LiteralValue::Number(n) => HirConst::Float(*n),
                LiteralValue::Null | LiteralValue::True | LiteralValue::False | LiteralValue::String(_) => {
                    return Err(LowerError::Unsupported(
                        format!("literal {:?} not yet supported", e.value),
                        Some(e.span),
                    ));
                }
            };
            ops.push(HirOp::LoadConst {
                value,
                span: e.span,
            });
        }
        Expression::Identifier(_) | Expression::Binary(_) | Expression::Unary(_)
        | Expression::Call(_) | Expression::Assign(_) => {
            return Err(LowerError::Unsupported(
                format!("expression {:?} not yet supported", expr),
                Some(expr.span()),
            ));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::Parser;

    #[test]
    fn lower_simple_return() {
        let mut parser = Parser::new("function main() { return 42; }");
        let script = parser.parse().expect("parse");
        let funcs = script_to_hir(&script).expect("lower");
        assert_eq!(funcs.len(), 1);
        assert_eq!(funcs[0].name.as_deref(), Some("main"));
        assert_eq!(funcs[0].blocks[0].ops.len(), 1);
    }
}
