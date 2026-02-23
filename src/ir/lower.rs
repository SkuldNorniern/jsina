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

struct LowerCtx {
    blocks: Vec<HirBlock>,
    current_block: usize,
    locals: HashMap<String, u32>,
    next_slot: u32,
    return_span: Span,
}

fn compile_function(f: &FunctionDeclStmt) -> Result<HirFunction, LowerError> {
    let span = f.span;
    let mut ctx = LowerCtx {
        blocks: vec![HirBlock {
            id: 0,
            ops: Vec::new(),
            terminator: HirTerminator::Return { span },
        }],
        current_block: 0,
        locals: HashMap::new(),
        next_slot: 0,
        return_span: span,
    };

    for param in &f.params {
        ctx.locals.insert(param.clone(), ctx.next_slot);
        ctx.next_slot += 1;
    }

    let _ = compile_statement(&f.body, &mut ctx)?;

    ctx.blocks[ctx.current_block].terminator = HirTerminator::Return {
        span: ctx.return_span,
    };

    Ok(HirFunction {
        name: Some(f.name.clone()),
        params: f.params.clone(),
        num_locals: ctx.next_slot,
        entry_block: 0,
        blocks: ctx.blocks,
    })
}

fn compile_statement(stmt: &Statement, ctx: &mut LowerCtx) -> Result<bool, LowerError> {
    let ops = &mut ctx.blocks[ctx.current_block].ops;
    match stmt {
        Statement::Block(b) => {
            let mut hit_return = false;
            for s in &b.body {
                if compile_statement(s, ctx)? {
                    hit_return = true;
                    break;
                }
            }
            return Ok(hit_return);
        }
        Statement::Return(r) => {
            ctx.return_span = r.span;
            if let Some(ref expr) = r.argument {
                compile_expression(expr, ops, &ctx.locals)?;
            }
            return Ok(true);
        }
        Statement::Expression(e) => {
            compile_expression(&e.expression, ops, &ctx.locals)?;
            ops.push(HirOp::Pop { span: e.span });
            return Ok(false);
        }
        Statement::If(i) => {
            let cond_slot = ctx.next_slot;
            ctx.next_slot += 1;
            compile_expression(&i.condition, ops, &ctx.locals)?;
            ops.push(HirOp::StoreLocal {
                id: cond_slot,
                span: i.span,
            });

            let then_id = ctx.blocks.len() as HirBlockId;
            ctx.blocks.push(HirBlock {
                id: then_id,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: 0 },
            });
            let else_id = ctx.blocks.len() as HirBlockId;
            ctx.blocks.push(HirBlock {
                id: else_id,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: 0 },
            });
            let merge_id = ctx.blocks.len() as HirBlockId;
            ctx.blocks.push(HirBlock {
                id: merge_id,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: 0 },
            });

            ctx.blocks[ctx.current_block].terminator = HirTerminator::Branch {
                cond: cond_slot,
                then_block: then_id,
                else_block: else_id,
            };

            ctx.current_block = then_id as usize;
            let then_returns = compile_statement(&i.then_branch, ctx)?;
            ctx.blocks[ctx.current_block].terminator = if then_returns {
                HirTerminator::Return { span: ctx.return_span }
            } else {
                HirTerminator::Jump { target: merge_id }
            };

            ctx.current_block = else_id as usize;
            let else_returns = if let Some(ref else_b) = i.else_branch {
                compile_statement(else_b, ctx)?
            } else {
                false
            };
            ctx.blocks[ctx.current_block].terminator = if else_returns {
                HirTerminator::Return { span: ctx.return_span }
            } else {
                HirTerminator::Jump { target: merge_id }
            };

            ctx.current_block = merge_id as usize;
        }
        Statement::While(w) => {
            let cond_slot = ctx.next_slot;
            ctx.next_slot += 1;

            let loop_id = ctx.blocks.len() as HirBlockId;
            ctx.blocks.push(HirBlock {
                id: loop_id,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: 0 },
            });
            let body_id = ctx.blocks.len() as HirBlockId;
            ctx.blocks.push(HirBlock {
                id: body_id,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: 0 },
            });
            let exit_id = ctx.blocks.len() as HirBlockId;
            ctx.blocks.push(HirBlock {
                id: exit_id,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: 0 },
            });

            ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: loop_id };

            ctx.current_block = loop_id as usize;
            let loop_ops = &mut ctx.blocks[ctx.current_block].ops;
            compile_expression(&w.condition, loop_ops, &ctx.locals)?;
            loop_ops.push(HirOp::StoreLocal {
                id: cond_slot,
                span: w.span,
            });
            ctx.blocks[ctx.current_block].terminator = HirTerminator::Branch {
                cond: cond_slot,
                then_block: body_id,
                else_block: exit_id,
            };

            ctx.current_block = body_id as usize;
            compile_statement(&w.body, ctx)?;
            ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: loop_id };

            ctx.current_block = exit_id as usize;
        }
        Statement::VarDecl(d) => {
            for decl in &d.declarations {
                let slot = *ctx.locals.entry(decl.name.clone()).or_insert_with(|| {
                    let s = ctx.next_slot;
                    ctx.next_slot += 1;
                    s
                });
                if let Some(ref init) = decl.init {
                    compile_expression(init, ops, &ctx.locals)?;
                    ops.push(HirOp::StoreLocal {
                        id: slot,
                        span: decl.span,
                    });
                }
            }
            return Ok(false);
        }
        Statement::LetDecl(d) => {
            for decl in &d.declarations {
                let slot = *ctx.locals.entry(decl.name.clone()).or_insert_with(|| {
                    let s = ctx.next_slot;
                    ctx.next_slot += 1;
                    s
                });
                if let Some(ref init) = decl.init {
                    compile_expression(init, ops, &ctx.locals)?;
                    ops.push(HirOp::StoreLocal {
                        id: slot,
                        span: decl.span,
                    });
                }
            }
            return Ok(false);
        }
        Statement::ConstDecl(d) => {
            for decl in &d.declarations {
                let slot = *ctx.locals.entry(decl.name.clone()).or_insert_with(|| {
                    let s = ctx.next_slot;
                    ctx.next_slot += 1;
                    s
                });
                if let Some(ref init) = decl.init {
                    compile_expression(init, ops, &ctx.locals)?;
                    ops.push(HirOp::StoreLocal {
                        id: slot,
                        span: decl.span,
                    });
                }
            }
            return Ok(false);
        }
        Statement::FunctionDecl(_) => return Ok(false),
        Statement::For(f) => {
            let cond_slot = ctx.next_slot;
            ctx.next_slot += 1;

            if let Some(ref init) = f.init {
                compile_statement(init, ctx)?;
            }

            let loop_id = ctx.blocks.len() as HirBlockId;
            ctx.blocks.push(HirBlock {
                id: loop_id,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: 0 },
            });
            let body_id = ctx.blocks.len() as HirBlockId;
            ctx.blocks.push(HirBlock {
                id: body_id,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: 0 },
            });
            let update_id = ctx.blocks.len() as HirBlockId;
            ctx.blocks.push(HirBlock {
                id: update_id,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: 0 },
            });
            let exit_id = ctx.blocks.len() as HirBlockId;
            ctx.blocks.push(HirBlock {
                id: exit_id,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: 0 },
            });

            ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: loop_id };

            ctx.current_block = loop_id as usize;
            let loop_ops = &mut ctx.blocks[ctx.current_block].ops;
            if let Some(ref cond) = f.condition {
                compile_expression(cond, loop_ops, &ctx.locals)?;
            } else {
                loop_ops.push(HirOp::LoadConst {
                    value: HirConst::Int(1),
                    span: f.span,
                });
            }
            loop_ops.push(HirOp::StoreLocal {
                id: cond_slot,
                span: f.span,
            });
            ctx.blocks[ctx.current_block].terminator = HirTerminator::Branch {
                cond: cond_slot,
                then_block: body_id,
                else_block: exit_id,
            };

            ctx.current_block = body_id as usize;
            compile_statement(&f.body, ctx)?;
            ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: update_id };

            ctx.current_block = update_id as usize;
            let update_ops = &mut ctx.blocks[ctx.current_block].ops;
            if let Some(ref upd) = f.update {
                compile_expression(upd, update_ops, &ctx.locals)?;
                update_ops.push(HirOp::Pop { span: f.span });
            }
            ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: loop_id };

            ctx.current_block = exit_id as usize;
        }
        Statement::Break(_) | Statement::Continue(_) => {
            return Err(LowerError::Unsupported(
                format!("{:?} not yet supported", stmt),
                Some(stmt.span()),
            ));
        }
    }
    Ok(false)
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
                LiteralValue::True => HirConst::Int(1),
                LiteralValue::False => HirConst::Int(0),
                LiteralValue::Null | LiteralValue::String(_) => {
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
                BinaryOp::Mul => ops.push(HirOp::Mul { span: e.span }),
                BinaryOp::Div => ops.push(HirOp::Div { span: e.span }),
                BinaryOp::Lt => ops.push(HirOp::Lt { span: e.span }),
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
        Expression::Assign(e) => {
            let slot = match e.left.as_ref() {
                Expression::Identifier(id) => *locals.get(&id.name).ok_or_else(|| {
                    LowerError::Unsupported(
                        format!("assignment to undefined variable '{}'", id.name),
                        Some(id.span),
                    )
                })?,
                _ => {
                    return Err(LowerError::Unsupported(
                        "assignment to non-identifier not yet supported".to_string(),
                        Some(e.span),
                    ));
                }
            };
            compile_expression(&e.right, ops, locals)?;
            ops.push(HirOp::StoreLocal {
                id: slot,
                span: e.span,
            });
            ops.push(HirOp::LoadLocal {
                id: slot,
                span: e.span,
            });
        }
        Expression::Call(_) => {
            return Err(LowerError::Unsupported(
                "call expression not yet supported".to_string(),
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

    #[test]
    fn lower_if_then() {
        let mut parser = Parser::new("function main() { if (1) return 1; return 0; }");
        let script = parser.parse().expect("parse");
        let funcs = script_to_hir(&script).expect("lower");
        let cf = hir_to_bytecode(&funcs[0]);
        let completion = interpret(&cf.chunk).expect("interpret");
        if let crate::vm::Completion::Return(v) = completion {
            assert_eq!(v.to_i64(), 1);
        } else {
            panic!("expected Return(1)");
        }
    }

    #[test]
    fn lower_if_else() {
        let mut parser = Parser::new("function main() { if (0) return 1; else return 2; }");
        let script = parser.parse().expect("parse");
        let funcs = script_to_hir(&script).expect("lower");
        let cf = hir_to_bytecode(&funcs[0]);
        let completion = interpret(&cf.chunk).expect("interpret");
        if let crate::vm::Completion::Return(v) = completion {
            assert_eq!(v.to_i64(), 2);
        } else {
            panic!("expected Return(2)");
        }
    }

    #[test]
    fn lower_while() {
        let mut parser = Parser::new("function main() { let n = 0; while (n < 3) { n = n + 1; } return n; }");
        let script = parser.parse().expect("parse");
        let funcs = script_to_hir(&script).expect("lower");
        let cf = hir_to_bytecode(&funcs[0]);
        let completion = interpret(&cf.chunk).expect("interpret");
        if let crate::vm::Completion::Return(v) = completion {
            assert_eq!(v.to_i64(), 3);
        } else {
            panic!("expected Return(3)");
        }
    }

    #[test]
    fn lower_while_simple() {
        let mut parser = Parser::new("function main() { let n = 0; while (0) {} return n; }");
        let script = parser.parse().expect("parse");
        let funcs = script_to_hir(&script).expect("lower");
        let cf = hir_to_bytecode(&funcs[0]);
        let completion = interpret(&cf.chunk).expect("interpret");
        if let crate::vm::Completion::Return(v) = completion {
            assert_eq!(v.to_i64(), 0);
        } else {
            panic!("expected Return(0)");
        }
    }

    #[test]
    fn lower_for() {
        let mut parser = Parser::new("function main() { let s = 0; for (let i = 0; i < 4; i = i + 1) { s = s + i; } return s; }");
        let script = parser.parse().expect("parse");
        let funcs = script_to_hir(&script).expect("lower");
        let cf = hir_to_bytecode(&funcs[0]);
        let completion = interpret(&cf.chunk).expect("interpret");
        if let crate::vm::Completion::Return(v) = completion {
            assert_eq!(v.to_i64(), 6);
        } else {
            panic!("expected Return(6), got {:?}", completion);
        }
    }
}
