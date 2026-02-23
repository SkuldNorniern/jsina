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
    let mut func_index: HashMap<String, u32> = HashMap::new();
    let mut func_decls: Vec<&FunctionDeclStmt> = Vec::new();
    for stmt in &script.body {
        if let Statement::FunctionDecl(f) = stmt {
            let idx = func_index.len() as u32;
            func_index.insert(f.name.clone(), idx);
            func_decls.push(f);
        }
    }
    let mut functions = Vec::new();
    for f in func_decls {
        functions.push(compile_function(f, &func_index)?);
    }
    Ok(functions)
}

struct LowerCtx {
    blocks: Vec<HirBlock>,
    current_block: usize,
    locals: HashMap<String, u32>,
    next_slot: u32,
    return_span: Span,
    func_index: HashMap<String, u32>,
    loop_stack: Vec<(HirBlockId, HirBlockId)>,
}

fn loop_stack_push(ctx: &mut LowerCtx, continue_target: HirBlockId, exit_target: HirBlockId) {
    ctx.loop_stack.push((continue_target, exit_target));
}

fn loop_stack_pop(ctx: &mut LowerCtx) {
    ctx.loop_stack.pop();
}

fn compile_function(f: &FunctionDeclStmt, func_index: &HashMap<String, u32>) -> Result<HirFunction, LowerError> {
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
        func_index: func_index.clone(),
        loop_stack: Vec::new(),
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
                compile_expression(expr, ctx)?;
            }
            return Ok(true);
        }
        Statement::Expression(e) => {
            compile_expression(&e.expression, ctx)?;
            ctx.blocks[ctx.current_block].ops.push(HirOp::Pop { span: e.span });
            return Ok(false);
        }
        Statement::If(i) => {
            let cond_slot = ctx.next_slot;
            ctx.next_slot += 1;
            compile_expression(&i.condition, ctx)?;
            ctx.blocks[ctx.current_block].ops.push(HirOp::StoreLocal {
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

            ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: loop_id };

            ctx.current_block = loop_id as usize;
            compile_expression(&w.condition, ctx)?;
            ctx.blocks[ctx.current_block].ops.push(HirOp::StoreLocal {
                id: cond_slot,
                span: w.span,
            });

            const WHILE_EXIT_PLACEHOLDER: HirBlockId = u32::MAX - 3;
            loop_stack_push(ctx, loop_id, WHILE_EXIT_PLACEHOLDER);
            ctx.current_block = body_id as usize;
            compile_statement(&w.body, ctx)?;
            loop_stack_pop(ctx);
            ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: loop_id };

            ctx.blocks.push(HirBlock {
                id: ctx.blocks.len() as HirBlockId,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: 0 },
            });
            let exit_id = ctx.blocks.len() as HirBlockId - 1;
            for block in &mut ctx.blocks {
                match &mut block.terminator {
                    HirTerminator::Jump { target } => {
                        if *target == WHILE_EXIT_PLACEHOLDER {
                            *target = exit_id;
                        }
                    }
                    HirTerminator::Branch { else_block, .. } => {
                        if *else_block == WHILE_EXIT_PLACEHOLDER {
                            *else_block = exit_id;
                        }
                    }
                    _ => {}
                }
            }
            ctx.blocks[loop_id as usize].terminator = HirTerminator::Branch {
                cond: cond_slot,
                then_block: body_id,
                else_block: exit_id,
            };

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
                    compile_expression(init, ctx)?;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::StoreLocal {
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
                    compile_expression(init, ctx)?;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::StoreLocal {
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
                    compile_expression(init, ctx)?;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::StoreLocal {
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

            const FOR_UPDATE_PLACEHOLDER: HirBlockId = u32::MAX - 1;
            const FOR_EXIT_PLACEHOLDER: HirBlockId = u32::MAX - 2;

            ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: loop_id };

            ctx.current_block = loop_id as usize;
            if let Some(ref cond) = f.condition {
                compile_expression(cond, ctx)?;
            } else {
                ctx.blocks[ctx.current_block].ops.push(HirOp::LoadConst {
                    value: HirConst::Int(1),
                    span: f.span,
                });
            }
            ctx.blocks[ctx.current_block].ops.push(HirOp::StoreLocal {
                id: cond_slot,
                span: f.span,
            });
            ctx.blocks[ctx.current_block].terminator = HirTerminator::Branch {
                cond: cond_slot,
                then_block: body_id,
                else_block: FOR_EXIT_PLACEHOLDER,
            };

            loop_stack_push(ctx, FOR_UPDATE_PLACEHOLDER, FOR_EXIT_PLACEHOLDER);
            ctx.current_block = body_id as usize;
            compile_statement(&f.body, ctx)?;
            loop_stack_pop(ctx);
            ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: FOR_UPDATE_PLACEHOLDER };

            ctx.blocks.push(HirBlock {
                id: ctx.blocks.len() as HirBlockId,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: 0 },
            });
            let update_id = ctx.blocks.len() as HirBlockId - 1;
            ctx.current_block = update_id as usize;
            if let Some(ref upd) = f.update {
                compile_expression(upd, ctx)?;
                ctx.blocks[ctx.current_block].ops.push(HirOp::Pop { span: f.span });
            }
            ctx.blocks[update_id as usize].terminator = HirTerminator::Jump { target: loop_id };

            ctx.blocks.push(HirBlock {
                id: ctx.blocks.len() as HirBlockId,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: 0 },
            });
            let exit_id = ctx.blocks.len() as HirBlockId - 1;

            for block in &mut ctx.blocks {
                match &mut block.terminator {
                    HirTerminator::Jump { target } => {
                        if *target == FOR_UPDATE_PLACEHOLDER {
                            *target = update_id;
                        } else if *target == FOR_EXIT_PLACEHOLDER {
                            *target = exit_id;
                        }
                    }
                    HirTerminator::Branch { else_block, .. } => {
                        if *else_block == FOR_EXIT_PLACEHOLDER {
                            *else_block = exit_id;
                        }
                    }
                    _ => {}
                }
            }

            ctx.current_block = exit_id as usize;
        }
        Statement::Break(b) => {
            let (_, exit) = ctx.loop_stack.last().ok_or_else(|| {
                LowerError::Unsupported("break outside loop".to_string(), Some(b.span))
            })?;
            let break_block_id = ctx.blocks.len() as HirBlockId;
            ctx.blocks.push(HirBlock {
                id: break_block_id,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: *exit },
            });
            ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump {
                target: break_block_id,
            };
            let unreachable_id = ctx.blocks.len() as HirBlockId;
            ctx.blocks.push(HirBlock {
                id: unreachable_id,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: *exit },
            });
            ctx.current_block = unreachable_id as usize;
        }
        Statement::Continue(c) => {
            let (cont, exit) = ctx.loop_stack.last().ok_or_else(|| {
                LowerError::Unsupported("continue outside loop".to_string(), Some(c.span))
            })?;
            let cont_block_id = ctx.blocks.len() as HirBlockId;
            ctx.blocks.push(HirBlock {
                id: cont_block_id,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: *cont },
            });
            ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump {
                target: cont_block_id,
            };
            let unreachable_id = ctx.blocks.len() as HirBlockId;
            ctx.blocks.push(HirBlock {
                id: unreachable_id,
                ops: Vec::new(),
                terminator: HirTerminator::Jump { target: *exit },
            });
            ctx.current_block = unreachable_id as usize;
        }
    }
    Ok(false)
}

fn compile_expression(expr: &Expression, ctx: &mut LowerCtx) -> Result<(), LowerError> {
    let locals = &ctx.locals;
    let func_index = &ctx.func_index;
    match expr {
        Expression::Literal(e) => {
            let value = match &e.value {
                LiteralValue::Int(n) => HirConst::Int(*n),
                LiteralValue::Number(n) => HirConst::Float(*n),
                LiteralValue::True => HirConst::Int(1),
                LiteralValue::False => HirConst::Int(0),
                LiteralValue::Null => HirConst::Null,
                LiteralValue::String(_) => {
                    return Err(LowerError::Unsupported(
                        format!("literal {:?} not yet supported", e.value),
                        Some(e.span),
                    ));
                }
            };
            ctx.blocks[ctx.current_block].ops.push(HirOp::LoadConst {
                value,
                span: e.span,
            });
        }
        Expression::Identifier(e) => {
            let slot = locals.get(&e.name).ok_or_else(|| {
                LowerError::Unsupported(format!("undefined variable '{}'", e.name), Some(e.span))
            })?;
            ctx.blocks[ctx.current_block].ops.push(HirOp::LoadLocal {
                id: *slot,
                span: e.span,
            });
        }
        Expression::Binary(e) => {
            match e.op {
                BinaryOp::LogicalAnd => {
                    let result_slot = ctx.next_slot;
                    ctx.next_slot += 1;
                    compile_expression(&e.left, ctx)?;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::StoreLocal {
                        id: result_slot,
                        span: e.span,
                    });
                    let else_id = ctx.blocks.len() as HirBlockId;
                    ctx.blocks.push(HirBlock {
                        id: else_id,
                        ops: Vec::new(),
                        terminator: HirTerminator::Jump { target: 0 },
                    });
                    let right_id = ctx.blocks.len() as HirBlockId;
                    ctx.blocks.push(HirBlock {
                        id: right_id,
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
                        cond: result_slot,
                        then_block: right_id,
                        else_block: else_id,
                    };
                    ctx.current_block = right_id as usize;
                    compile_expression(&e.right, ctx)?;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::StoreLocal {
                        id: result_slot,
                        span: e.span,
                    });
                    ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: merge_id };
                    ctx.current_block = else_id as usize;
                    ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: merge_id };
                    ctx.current_block = merge_id as usize;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::LoadLocal {
                        id: result_slot,
                        span: e.span,
                    });
                }
                BinaryOp::LogicalOr => {
                    let result_slot = ctx.next_slot;
                    ctx.next_slot += 1;
                    compile_expression(&e.left, ctx)?;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::StoreLocal {
                        id: result_slot,
                        span: e.span,
                    });
                    let else_id = ctx.blocks.len() as HirBlockId;
                    ctx.blocks.push(HirBlock {
                        id: else_id,
                        ops: Vec::new(),
                        terminator: HirTerminator::Jump { target: 0 },
                    });
                    let right_id = ctx.blocks.len() as HirBlockId;
                    ctx.blocks.push(HirBlock {
                        id: right_id,
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
                        cond: result_slot,
                        then_block: right_id,
                        else_block: else_id,
                    };
                    ctx.current_block = right_id as usize;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::LoadLocal {
                        id: result_slot,
                        span: e.span,
                    });
                    ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: merge_id };
                    ctx.current_block = else_id as usize;
                    compile_expression(&e.right, ctx)?;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::StoreLocal {
                        id: result_slot,
                        span: e.span,
                    });
                    ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: merge_id };
                    ctx.current_block = merge_id as usize;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::LoadLocal {
                        id: result_slot,
                        span: e.span,
                    });
                }
                BinaryOp::NullishCoalescing => {
                    let result_slot = ctx.next_slot;
                    ctx.next_slot += 1;
                    compile_expression(&e.left, ctx)?;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::StoreLocal {
                        id: result_slot,
                        span: e.span,
                    });
                    let else_id = ctx.blocks.len() as HirBlockId;
                    ctx.blocks.push(HirBlock {
                        id: else_id,
                        ops: Vec::new(),
                        terminator: HirTerminator::Jump { target: 0 },
                    });
                    let right_id = ctx.blocks.len() as HirBlockId;
                    ctx.blocks.push(HirBlock {
                        id: right_id,
                        ops: Vec::new(),
                        terminator: HirTerminator::Jump { target: 0 },
                    });
                    let merge_id = ctx.blocks.len() as HirBlockId;
                    ctx.blocks.push(HirBlock {
                        id: merge_id,
                        ops: Vec::new(),
                        terminator: HirTerminator::Jump { target: 0 },
                    });
                    ctx.blocks[ctx.current_block].terminator = HirTerminator::BranchNullish {
                        cond: result_slot,
                        then_block: right_id,
                        else_block: else_id,
                    };
                    ctx.current_block = right_id as usize;
                    compile_expression(&e.right, ctx)?;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::StoreLocal {
                        id: result_slot,
                        span: e.span,
                    });
                    ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: merge_id };
                    ctx.current_block = else_id as usize;
                    ctx.blocks[ctx.current_block].terminator = HirTerminator::Jump { target: merge_id };
                    ctx.current_block = merge_id as usize;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::LoadLocal {
                        id: result_slot,
                        span: e.span,
                    });
                }
                _ => {
                    compile_expression(&e.left, ctx)?;
                    compile_expression(&e.right, ctx)?;
                    match e.op {
                        BinaryOp::Add => ctx.blocks[ctx.current_block].ops.push(HirOp::Add { span: e.span }),
                        BinaryOp::Sub => ctx.blocks[ctx.current_block].ops.push(HirOp::Sub { span: e.span }),
                        BinaryOp::Mul => ctx.blocks[ctx.current_block].ops.push(HirOp::Mul { span: e.span }),
                        BinaryOp::Div => ctx.blocks[ctx.current_block].ops.push(HirOp::Div { span: e.span }),
                        BinaryOp::Mod => ctx.blocks[ctx.current_block].ops.push(HirOp::Mod { span: e.span }),
                        BinaryOp::Pow => ctx.blocks[ctx.current_block].ops.push(HirOp::Pow { span: e.span }),
                        BinaryOp::Lt => ctx.blocks[ctx.current_block].ops.push(HirOp::Lt { span: e.span }),
                        BinaryOp::Lte => ctx.blocks[ctx.current_block].ops.push(HirOp::Lte { span: e.span }),
                        BinaryOp::Gt => ctx.blocks[ctx.current_block].ops.push(HirOp::Gt { span: e.span }),
                        BinaryOp::Gte => ctx.blocks[ctx.current_block].ops.push(HirOp::Gte { span: e.span }),
                        BinaryOp::StrictEq => ctx.blocks[ctx.current_block].ops.push(HirOp::StrictEq { span: e.span }),
                        BinaryOp::StrictNotEq => ctx.blocks[ctx.current_block].ops.push(HirOp::StrictNotEq { span: e.span }),
                        BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::LogicalAnd | BinaryOp::LogicalOr
                        | BinaryOp::NullishCoalescing => {
                            return Err(LowerError::Unsupported(
                                format!("binary op {:?} not yet supported", e.op),
                                Some(e.span),
                            ));
                        }
                    }
                }
            }
        }
        Expression::Unary(e) => {
            match e.op {
                UnaryOp::Plus => compile_expression(&e.argument, ctx)?,
                UnaryOp::Minus => {
                    ctx.blocks[ctx.current_block].ops.push(HirOp::LoadConst {
                        value: HirConst::Int(0),
                        span: e.span,
                    });
                    compile_expression(&e.argument, ctx)?;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::Sub { span: e.span });
                }
                UnaryOp::LogicalNot => {
                    compile_expression(&e.argument, ctx)?;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::Not { span: e.span });
                }
            }
        }
        Expression::Assign(e) => {
            match e.left.as_ref() {
                Expression::Identifier(id) => {
                    let slot = *locals.get(&id.name).ok_or_else(|| {
                        LowerError::Unsupported(
                            format!("assignment to undefined variable '{}'", id.name),
                            Some(id.span),
                        )
                    })?;
                    compile_expression(&e.right, ctx)?;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::StoreLocal {
                        id: slot,
                        span: e.span,
                    });
                    ctx.blocks[ctx.current_block].ops.push(HirOp::LoadLocal {
                        id: slot,
                        span: e.span,
                    });
                }
                Expression::Member(m) => {
                    compile_expression(&m.object, ctx)?;
                    match &m.property {
                        MemberProperty::Identifier(key) => {
                            compile_expression(&e.right, ctx)?;
                            ctx.blocks[ctx.current_block].ops.push(HirOp::Swap { span: e.span });
                            ctx.blocks[ctx.current_block].ops.push(HirOp::SetProp {
                                key: key.clone(),
                                span: e.span,
                            });
                        }
                        MemberProperty::Expression(key_expr) => {
                            compile_expression(key_expr, ctx)?;
                            compile_expression(&e.right, ctx)?;
                            ctx.blocks[ctx.current_block].ops.push(HirOp::SetPropDyn { span: e.span });
                        }
                    }
                }
                _ => {
                    return Err(LowerError::Unsupported(
                        "assignment to unsupported target".to_string(),
                        Some(e.span),
                    ));
                }
            }
        }
        Expression::Call(e) => {
            let idx = match e.callee.as_ref() {
                Expression::Identifier(id) => *func_index.get(&id.name).ok_or_else(|| {
                    LowerError::Unsupported(
                        format!("call to undefined function '{}'", id.name),
                        Some(id.span),
                    )
                })?,
                _ => {
                    return Err(LowerError::Unsupported(
                        "call to non-identifier not yet supported".to_string(),
                        Some(e.span),
                    ));
                }
            };
            for arg in &e.args {
                compile_expression(arg, ctx)?;
            }
            ctx.blocks[ctx.current_block].ops.push(HirOp::Call {
                func_index: idx,
                argc: e.args.len() as u32,
                span: e.span,
            });
        }
        Expression::ObjectLiteral(e) => {
            ctx.blocks[ctx.current_block].ops.push(HirOp::NewObject { span: e.span });
            for (key, value) in &e.properties {
                ctx.blocks[ctx.current_block].ops.push(HirOp::Dup { span: e.span });
                compile_expression(value, ctx)?;
                ctx.blocks[ctx.current_block].ops.push(HirOp::Swap { span: e.span });
                ctx.blocks[ctx.current_block].ops.push(HirOp::SetProp {
                    key: key.clone(),
                    span: e.span,
                });
                ctx.blocks[ctx.current_block].ops.push(HirOp::Pop { span: e.span });
            }
        }
        Expression::ArrayLiteral(e) => {
            ctx.blocks[ctx.current_block].ops.push(HirOp::NewArray { span: e.span });
            for (i, elem) in e.elements.iter().enumerate() {
                ctx.blocks[ctx.current_block].ops.push(HirOp::Dup { span: e.span });
                compile_expression(elem, ctx)?;
                ctx.blocks[ctx.current_block].ops.push(HirOp::Swap { span: e.span });
                ctx.blocks[ctx.current_block].ops.push(HirOp::SetProp {
                    key: i.to_string(),
                    span: e.span,
                });
                ctx.blocks[ctx.current_block].ops.push(HirOp::Pop { span: e.span });
            }
        }
        Expression::Member(e) => {
            compile_expression(&e.object, ctx)?;
            match &e.property {
                MemberProperty::Identifier(key) => {
                    ctx.blocks[ctx.current_block].ops.push(HirOp::GetProp {
                        key: key.clone(),
                        span: e.span,
                    });
                }
                MemberProperty::Expression(key_expr) => {
                    compile_expression(key_expr, ctx)?;
                    ctx.blocks[ctx.current_block].ops.push(HirOp::GetPropDyn { span: e.span });
                }
            }
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
    fn lower_call() {
        let mut parser = Parser::new("function add(a,b) { return a+b; } function main() { return add(1,2); }");
        let script = parser.parse().expect("parse");
        let funcs = script_to_hir(&script).expect("lower");
        let chunks: Vec<_> = funcs.iter().map(|f| hir_to_bytecode(f).chunk).collect();
        let program = crate::vm::Program {
            chunks: chunks.clone(),
            entry: funcs.iter().position(|f| f.name.as_deref() == Some("main")).expect("main"),
        };
        let completion = crate::vm::interpret_program(&program).expect("interpret");
        if let crate::vm::Completion::Return(v) = completion {
            assert_eq!(v.to_i64(), 3);
        } else {
            panic!("expected Return(3), got {:?}", completion);
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

    #[test]
    fn lower_object_literal() {
        let mut parser = Parser::new(
            "function main() { let o = { x: 10, y: 20 }; return o.x + o.y; }",
        );
        let script = parser.parse().expect("parse");
        let funcs = script_to_hir(&script).expect("lower");
        let cf = hir_to_bytecode(&funcs[0]);
        let completion = interpret(&cf.chunk).expect("interpret");
        if let crate::vm::Completion::Return(v) = completion {
            assert_eq!(v.to_i64(), 30);
        } else {
            panic!("expected Return(30), got {:?}", completion);
        }
    }

    #[test]
    fn lower_array_literal() {
        let mut parser = Parser::new(
            "function main() { let a = [1, 2, 3]; return a.length; }",
        );
        let script = parser.parse().expect("parse");
        let funcs = script_to_hir(&script).expect("lower");
        let cf = hir_to_bytecode(&funcs[0]);
        let completion = interpret(&cf.chunk).expect("interpret");
        if let crate::vm::Completion::Return(v) = completion {
            assert_eq!(v.to_i64(), 3);
        } else {
            panic!("expected Return(3), got {:?}", completion);
        }
    }

    #[test]
    fn lower_strict_eq_and_not() {
        let mut parser = Parser::new(
            "function main() { let a = 1; let b = 2; if (a === b) return 0; if (!(a < b)) return 0; return 1; }",
        );
        let script = parser.parse().expect("parse");
        let funcs = script_to_hir(&script).expect("lower");
        let cf = hir_to_bytecode(&funcs[0]);
        let completion = interpret(&cf.chunk).expect("interpret");
        if let crate::vm::Completion::Return(v) = completion {
            assert_eq!(v.to_i64(), 1, "1 !== 2 and 1 < 2, so should return 1");
        } else {
            panic!("expected Return(1), got {:?}", completion);
        }
    }

    #[test]
    fn lower_while_break_continue() {
        let mut parser = Parser::new(
            "function main() { let n = 0; while (n < 10) { n = n + 1; if (n < 5) continue; break; } return n; }",
        );
        let script = parser.parse().expect("parse");
        let funcs = script_to_hir(&script).expect("lower");
        let cf = hir_to_bytecode(&funcs[0]);
        let completion = interpret(&cf.chunk).expect("interpret");
        if let crate::vm::Completion::Return(v) = completion {
            assert_eq!(v.to_i64(), 5, "expected 5 when breaking after n reaches 5");
        } else {
            panic!("expected Return(5), got {:?}", completion);
        }
    }

    #[test]
    fn lower_prop_assignment() {
        let result = crate::driver::Driver::run(
            "function main() { let o = { x: 0 }; o.x = 42; return o.x; }",
        )
        .expect("run");
        assert_eq!(result, 42, "property assignment should mutate and read back");
    }

    #[test]
    fn lower_computed_prop() {
        let result = crate::driver::Driver::run(
            "function main() { let a = [10, 20, 30]; return a[1]; }",
        )
        .expect("run");
        assert_eq!(result, 20, "a[1] should be 20");
    }

    #[test]
    fn lower_computed_prop_assignment() {
        let result = crate::driver::Driver::run(
            "function main() { let a = [1, 2, 3]; a[1] = 99; return a[1]; }",
        )
        .expect("run");
        assert_eq!(result, 99, "a[1] = 99 should mutate and read back");
    }

    #[test]
    fn lower_comparison_ops() {
        let result = crate::driver::Driver::run(
            "function main() { if (1 !== 2) return 1; if (5 > 4) return 2; if (3 >= 3) return 3; if (1 <= 2) return 4; return 0; }",
        )
        .expect("run");
        assert_eq!(result, 1, "!==, >, >=, <= should work");
    }

    #[test]
    fn lower_logical_and_or() {
        let result = crate::driver::Driver::run(
            "function main() { return 1 && 2; }",
        )
        .expect("run");
        assert_eq!(result, 2, "1 && 2 should short-circuit to 2");

        let result = crate::driver::Driver::run(
            "function main() { return 0 && 99; }",
        )
        .expect("run");
        assert_eq!(result, 0, "0 && 99 should short-circuit to 0");

        let result = crate::driver::Driver::run(
            "function main() { return 0 || 1; }",
        )
        .expect("run");
        assert_eq!(result, 1, "0 || 1 should return 1");

        let result = crate::driver::Driver::run(
            "function main() { return 1 || 99; }",
        )
        .expect("run");
        assert_eq!(result, 1, "1 || 99 should short-circuit to 1");
    }

    #[test]
    fn lower_nullish_coalescing() {
        let result = crate::driver::Driver::run(
            "function main() { return null ?? 42; }",
        )
        .expect("run");
        assert_eq!(result, 42, "null ?? 42 should return 42");

        let result = crate::driver::Driver::run(
            "function main() { return 0 ?? 99; }",
        )
        .expect("run");
        assert_eq!(result, 0, "0 ?? 99 should return 0 (0 is not nullish)");

        let result = crate::driver::Driver::run(
            "function main() { let x; return x ?? 7; }",
        )
        .expect("run");
        assert_eq!(result, 7, "undefined ?? 7 should return 7");
    }
}
