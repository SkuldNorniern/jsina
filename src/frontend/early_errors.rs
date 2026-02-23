use crate::diagnostics::{Diagnostic, Span};
use crate::frontend::ast::*;

fn is_use_strict(stmt: &Statement) -> bool {
    if let Statement::Expression(e) = stmt {
        if let Expression::Literal(lit) = &*e.expression {
            if let LiteralValue::String(s) = &lit.value {
                return s == "use strict";
            }
        }
    }
    false
}

fn script_is_strict(script: &Script) -> bool {
    script.body.first().map_or(false, is_use_strict)
}

fn block_is_strict(body: &[Statement]) -> bool {
    body.first().map_or(false, is_use_strict)
}

#[derive(Debug)]
pub struct EarlyError {
    pub code: String,
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for EarlyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({}) at {}", self.message, self.code, self.span)
    }
}

impl std::error::Error for EarlyError {}

pub fn check(script: &Script) -> Result<(), Vec<EarlyError>> {
    let mut errors = Vec::new();
    check_script(script, &mut errors);
    if errors.is_empty() {
        Ok(())
    } else {
        errors.sort_by_key(|e| (e.span.start.line, e.span.start.column));
        Err(errors)
    }
}

const STRICT_RESERVED: [&str; 2] = ["eval", "arguments"];

fn is_strict_reserved(name: &str) -> bool {
    STRICT_RESERVED.contains(&name)
}

struct CheckContext {
    in_function: bool,
    in_iteration: bool,
    strict: bool,
}

fn check_script(script: &Script, errors: &mut Vec<EarlyError>) {
    let mut scope = Scope::new();
    let script_strict = script_is_strict(script);
    let ctx = CheckContext {
        in_function: false,
        in_iteration: false,
        strict: script_strict,
    };
    for stmt in &script.body {
        check_statement(stmt, &mut scope, &ctx, errors);
    }
}

fn check_statement(
    stmt: &Statement,
    scope: &mut Scope,
    ctx: &CheckContext,
    errors: &mut Vec<EarlyError>,
) {
    match stmt {
        Statement::Block(b) => {
            scope.enter_block();
            for s in &b.body {
                check_statement(s, scope, ctx, errors);
            }
            scope.leave_block();
        }
        Statement::FunctionDecl(f) => {
            scope.enter_function();
            let fn_strict = ctx.strict
                || (if let Statement::Block(b) = &*f.body {
                    block_is_strict(&b.body)
                } else {
                    false
                });
            for param in &f.params {
                if fn_strict && is_strict_reserved(param) {
                    errors.push(EarlyError {
                        code: "JSINA-EARLY-008".to_string(),
                        message: format!("'{}' may not be used as parameter name in strict mode", param),
                        span: f.span,
                    });
                }
                if let Some(prev) = scope.add_lexical(param, f.span) {
                    errors.push(EarlyError {
                        code: "JSINA-EARLY-001".to_string(),
                        message: format!("duplicate parameter name '{}'", param),
                        span: prev,
                    });
                }
            }
            let fn_ctx = CheckContext {
                in_function: true,
                in_iteration: ctx.in_iteration,
                strict: fn_strict,
            };
            check_statement(&f.body, scope, &fn_ctx, errors);
            scope.leave_function();
        }
        Statement::Return(r) => {
            if !ctx.in_function {
                errors.push(EarlyError {
                    code: "JSINA-EARLY-002".to_string(),
                    message: "illegal return statement outside function".to_string(),
                    span: r.span,
                });
            }
        }
        Statement::Break(b) => {
            if let Some(ref label) = b.label {
                errors.push(EarlyError {
                    code: "JSINA-EARLY-004".to_string(),
                    message: format!("unknown label '{}'", label),
                    span: b.span,
                });
            } else if !ctx.in_iteration {
                errors.push(EarlyError {
                    code: "JSINA-EARLY-005".to_string(),
                    message: "illegal break statement: not inside iteration".to_string(),
                    span: b.span,
                });
            }
        }
        Statement::Continue(c) => {
            if let Some(ref label) = c.label {
                errors.push(EarlyError {
                    code: "JSINA-EARLY-006".to_string(),
                    message: format!("unknown label '{}'", label),
                    span: c.span,
                });
            } else if !ctx.in_iteration {
                errors.push(EarlyError {
                    code: "JSINA-EARLY-007".to_string(),
                    message: "illegal continue statement: not inside iteration".to_string(),
                    span: c.span,
                });
            }
        }
        Statement::LetDecl(d) => {
            for decl in &d.declarations {
                if ctx.strict && is_strict_reserved(&decl.name) {
                    errors.push(EarlyError {
                        code: "JSINA-EARLY-008".to_string(),
                        message: format!("'{}' may not be used as binding in strict mode", decl.name),
                        span: decl.span,
                    });
                }
                if let Some(prev) = scope.add_lexical(&decl.name, decl.span) {
                    errors.push(EarlyError {
                        code: "JSINA-EARLY-003".to_string(),
                        message: format!("duplicate lexical declaration '{}'", decl.name),
                        span: prev,
                    });
                }
            }
        }
        Statement::ConstDecl(d) => {
            for decl in &d.declarations {
                if ctx.strict && is_strict_reserved(&decl.name) {
                    errors.push(EarlyError {
                        code: "JSINA-EARLY-008".to_string(),
                        message: format!("'{}' may not be used as binding in strict mode", decl.name),
                        span: decl.span,
                    });
                }
                if let Some(prev) = scope.add_lexical(&decl.name, decl.span) {
                    errors.push(EarlyError {
                        code: "JSINA-EARLY-003".to_string(),
                        message: format!("duplicate lexical declaration '{}'", decl.name),
                        span: prev,
                    });
                }
            }
        }
        Statement::VarDecl(d) => {
            for decl in &d.declarations {
                if ctx.strict && is_strict_reserved(&decl.name) {
                    errors.push(EarlyError {
                        code: "JSINA-EARLY-008".to_string(),
                        message: format!("'{}' may not be used as binding in strict mode", decl.name),
                        span: decl.span,
                    });
                }
                scope.add_var(&decl.name);
            }
        }
        Statement::If(i) => {
            check_statement(&i.then_branch, scope, ctx, errors);
            if let Some(else_b) = &i.else_branch {
                check_statement(else_b, scope, ctx, errors);
            }
        }
        Statement::While(w) => {
            let iter_ctx = CheckContext {
                in_function: ctx.in_function,
                in_iteration: true,
                strict: ctx.strict,
            };
            check_statement(&w.body, scope, &iter_ctx, errors);
        }
        Statement::For(f) => {
            if let Some(ref init) = f.init {
                check_statement(init, scope, ctx, errors);
            }
            let iter_ctx = CheckContext {
                in_function: ctx.in_function,
                in_iteration: true,
                strict: ctx.strict,
            };
            check_statement(&f.body, scope, &iter_ctx, errors);
        }
        Statement::Expression(_) => {}
        Statement::Throw(_) => {}
        Statement::Try(t) => {
            check_statement(&t.body, scope, ctx, errors);
            if let Some(ref c) = t.catch_body {
                check_statement(c, scope, ctx, errors);
            }
            if let Some(ref f) = t.finally_body {
                check_statement(f, scope, ctx, errors);
            }
        }
    }
}

struct Scope {
    lexical: Vec<std::collections::HashMap<String, Span>>,
    var: Vec<std::collections::HashSet<String>>,
}

impl Scope {
    fn new() -> Self {
        Self {
            lexical: vec![std::collections::HashMap::new()],
            var: vec![std::collections::HashSet::new()],
        }
    }

    fn enter_block(&mut self) {
        self.lexical.push(std::collections::HashMap::new());
        self.var.push(std::collections::HashSet::new());
    }

    fn leave_block(&mut self) {
        self.lexical.pop();
        self.var.pop();
    }

    fn enter_function(&mut self) {
        self.lexical.push(std::collections::HashMap::new());
        self.var.push(std::collections::HashSet::new());
    }

    fn leave_function(&mut self) {
        self.lexical.pop();
        self.var.pop();
    }

    fn add_lexical(&mut self, name: &str, span: Span) -> Option<Span> {
        let top = self.lexical.last_mut()?;
        top.insert(name.to_string(), span)
    }

    fn add_var(&mut self, name: &str) {
        if let Some(top) = self.var.last_mut() {
            top.insert(name.to_string());
        }
    }
}

impl EarlyError {
    pub fn to_diagnostic(&self) -> Diagnostic {
        Diagnostic::error(
            self.code.clone(),
            self.message.clone(),
            Some(self.span),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::Parser;

    fn parse_and_check(source: &str) -> Result<(), Vec<EarlyError>> {
        let mut parser = Parser::new(source);
        let script = parser.parse().map_err(|_| vec![])?;
        check(&script)
    }

    #[test]
    fn check_ok_simple() {
        let r = parse_and_check("function main() { return 1; }");
        assert!(r.is_ok());
    }

    #[test]
    fn check_duplicate_param() {
        let r = parse_and_check("function f() { let x = 1; let x = 2; }");
        assert!(r.is_err());
        let errs = r.unwrap_err();
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("duplicate"));
    }

    #[test]
    fn check_duplicate_let() {
        let r = parse_and_check("function f() { let x = 1; let x = 2; }");
        assert!(r.is_err());
        let errs = r.unwrap_err();
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("duplicate"));
    }

    #[test]
    fn check_duplicate_const() {
        let r = parse_and_check("function f() { const x = 1; const x = 2; }");
        assert!(r.is_err());
        let errs = r.unwrap_err();
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("duplicate"));
    }

    #[test]
    fn check_return_top_level() {
        let r = parse_and_check("return 1;");
        assert!(r.is_err());
        let errs = r.unwrap_err();
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("illegal return"));
    }

    #[test]
    fn check_let_const_same_scope() {
        let r = parse_and_check("function f() { let x; const x = 1; }");
        assert!(r.is_err());
    }

    #[test]
    fn check_break_outside_loop() {
        let r = parse_and_check("function f() { break; }");
        assert!(r.is_err());
        let errs = r.unwrap_err();
        assert!(errs.iter().any(|e| e.message.contains("illegal break")));
    }

    #[test]
    fn check_continue_outside_loop() {
        let r = parse_and_check("function f() { continue; }");
        assert!(r.is_err());
        let errs = r.unwrap_err();
        assert!(errs.iter().any(|e| e.message.contains("illegal continue")));
    }

    #[test]
    fn check_break_inside_while_ok() {
        let r = parse_and_check("while (true) { break; }");
        assert!(r.is_ok());
    }

    #[test]
    fn check_continue_inside_for_ok() {
        let r = parse_and_check("for (;;) { continue; }");
        assert!(r.is_ok());
    }

    #[test]
    fn check_break_with_unknown_label() {
        let r = parse_and_check("while (true) { break loop; }");
        assert!(r.is_err());
        let errs = r.unwrap_err();
        assert!(errs.iter().any(|e| e.message.contains("unknown label")));
    }

    #[test]
    fn check_strict_eval_param() {
        let r = parse_and_check(r#" "use strict"; function f(eval) {} "#);
        assert!(r.is_err());
        let errs = r.unwrap_err();
        assert!(errs.iter().any(|e| e.message.contains("eval")));
    }

    #[test]
    fn check_strict_arguments_let() {
        let r = parse_and_check(r#" "use strict"; let arguments = 1; "#);
        assert!(r.is_err());
        let errs = r.unwrap_err();
        assert!(errs.iter().any(|e| e.message.contains("arguments")));
    }

    #[test]
    fn check_strict_in_function() {
        let r = parse_and_check(r#" function f() { "use strict"; let eval = 1; } "#);
        assert!(r.is_err());
    }

    #[test]
    fn check_non_strict_eval_ok() {
        let r = parse_and_check("function f(eval) { return eval; }");
        assert!(r.is_ok());
    }
}
