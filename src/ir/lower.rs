use crate::diagnostics::Span;
use crate::frontend::ast::*;
use crate::ir::hir::*;
use std::collections::HashMap;

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
    let mut locals: HashMap<String, u32> = HashMap::new();
    let mut next_slot: u32 = 0;

    for param in &f.params {
        locals.insert(param.clone(), next_slot);
        next_slot += 1;
    }

    let mut ops = Vec::new();
    let mut return_span = span;

    compile_statement(&f.body, &mut ops, &mut return_span, &mut locals, &mut next_slot)?;

    let num_locals = next_slot;

    let blocks = vec![HirBlock {
        id: 0,
        ops,
        terminator: HirTerminator::Return { span: return_span },
    }];

    Ok(HirFunction {
        name: Some(f.name.clone()),
        params: f.params.clone(),
        num_locals,
        entry_block: 0,
        blocks,
    })
}

fn compile_statement(
    stmt: &Statement,
    ops: &mut Vec<HirOp>,
    return_span: &mut Span,
    locals: &mut HashMap<String, u32>,
    next_slot: &mut u32,
) -> Result<(), LowerError> {
    match stmt {
        Statement::Block(b) => {
            for s in &b.body {
                compile_statement(s, ops, return_span, locals, next_slot)?;
            }
        }
        Statement::Return(r) => {
            *return_span = r.span;
            if let Some(ref expr) = r.argument {
                compile_expression(expr, ops, locals)?;
            }
        }
        Statement::Expression(e) => {
            compile_expression(&e.expression, ops, locals)?;
            ops.push(HirOp::Pop { span: e.span });
        }
        Statement::VarDecl(d) => {
            for decl in &d.declarations {
                let slot = *locals.entry(decl.name.clone()).or_insert_with(|| {
                    let s = *next_slot;
                    *next_slot += 1;
                    s
                });
                if let Some(ref init) = decl.init {
                    compile_expression(init, ops, locals)?;
                    ops.push(HirOp::StoreLocal {
                        id: slot,
                        span: decl.span,
                    });
                }
            }
        }
        Statement::LetDecl(d) => {
            for decl in &d.declarations {
                let slot = *locals.entry(decl.name.clone()).or_insert_with(|| {
                    let s = *next_slot;
                    *next_slot += 1;
                    s
                });
                if let Some(ref init) = decl.init {
                    compile_expression(init, ops, locals)?;
                    ops.push(HirOp::StoreLocal {
                        id: slot,
                        span: decl.span,
                    });
                }
            }
        }
        Statement::ConstDecl(d) => {
            for decl in &d.declarations {
                let slot = *locals.entry(decl.name.clone()).or_insert_with(|| {
                    let s = *next_slot;
                    *next_slot += 1;
                    s
                });
                if let Some(ref init) = decl.init {
                    compile_expression(init, ops, locals)?;
                    ops.push(HirOp::StoreLocal {
                        id: slot,
                        span: decl.span,
                    });
                }
            }
        }
        Statement::FunctionDecl(_) => {}
        Statement::If(_) | Statement::While(_) | Statement::For(_)
        | Statement::Break(_) | Statement::Continue(_) => {
            return Err(LowerError::Unsupported(
                format!("{:?} not yet supported", stmt),
                Some(stmt.span()),
            ));
        }
    }
    Ok(())
}

fn compile_expression(
    expr: &Expression,
    ops: &mut Vec<HirOp>,
    locals: &HashMap<String, u32>,
) -> Result<(), LowerError> {
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
        Expression::Identifier(e) => {
            let slot = locals.get(&e.name).ok_or_else(|| {
                LowerError::Unsupported(format!("undefined variable '{}'", e.name), Some(e.span))
            })?;
            ops.push(HirOp::LoadLocal {
                id: *slot,
                span: e.span,
            });
        }
        Expression::Binary(e) => {
            compile_expression(&e.left, ops, locals)?;
            compile_expression(&e.right, ops, locals)?;
            match e.op {
                BinaryOp::Add => ops.push(HirOp::Add { span: e.span }),
                BinaryOp::Sub => ops.push(HirOp::Sub { span: e.span }),
                _ => {
                    return Err(LowerError::Unsupported(
                        format!("binary op {:?} not yet supported", e.op),
                        Some(e.span),
                    ));
                }
            }
        }
        Expression::Unary(e) => {
            match e.op {
                UnaryOp::Plus => compile_expression(&e.argument, ops, locals)?,
                UnaryOp::Minus => {
                    ops.push(HirOp::LoadConst {
                        value: HirConst::Int(0),
                        span: e.span,
                    });
                    compile_expression(&e.argument, ops, locals)?;
                    ops.push(HirOp::Sub { span: e.span });
                }
                UnaryOp::LogicalNot => {
                    return Err(LowerError::Unsupported(
                        "unary ! not yet supported".to_string(),
                        Some(e.span),
                    ));
                }
            }
        }
        Expression::Call(_) | Expression::Assign(_) => {
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
    use crate::ir::hir_to_bytecode;
    use crate::vm::interpret;

    #[test]
    fn lower_simple_return() {
        let mut parser = Parser::new("function main() { return 42; }");
        let script = parser.parse().expect("parse");
        let funcs = script_to_hir(&script).expect("lower");
        assert_eq!(funcs.len(), 1);
        assert_eq!(funcs[0].name.as_deref(), Some("main"));
        assert_eq!(funcs[0].blocks[0].ops.len(), 1);
    }

    #[test]
    fn lower_add_literals() {
        let mut parser = Parser::new("function main() { return 1+2; }");
        let script = parser.parse().expect("parse");
        let funcs = script_to_hir(&script).expect("lower");
        let ops = &funcs[0].blocks[0].ops;
        assert_eq!(ops.len(), 3, "expected 3 ops, got {:?}", ops);
        let cf = hir_to_bytecode(&funcs[0]);
        let completion = interpret(&cf.chunk).expect("interpret");
        if let crate::vm::Completion::Return(v) = completion {
            assert_eq!(v.to_i64(), 3);
        } else {
            panic!("expected Return");
        }
    }

    #[test]
    fn lower_locals_and_add() {
        let mut parser = Parser::new("function main() { let x = 1; let y = 2; return x + y; }");
        let script = parser.parse().expect("parse");
        let funcs = script_to_hir(&script).expect("lower");
        assert_eq!(funcs.len(), 1);
        assert_eq!(funcs[0].num_locals, 2);
        let cf = hir_to_bytecode(&funcs[0]);
        let completion = interpret(&cf.chunk).expect("interpret");
        if let crate::vm::Completion::Return(v) = completion {
            assert_eq!(v.to_i64(), 3, "got {:?}", v);
        } else {
            panic!("expected Return, got {:?}", completion);
        }
    }
}
