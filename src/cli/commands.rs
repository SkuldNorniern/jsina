use crate::driver::Driver;
use crate::frontend::{Script, Statement, Expression, TokenType};

pub fn tokens(source: &str) {
    let tokens = Driver::tokens(source);
    for (i, t) in tokens.iter().enumerate() {
        let tt = match &t.token_type {
            TokenType::Eof => "EOF".to_string(),
            TokenType::Error(msg) => format!("Error({})", msg),
            _ => format!("{:?}", t.token_type),
        };
        println!("{:4}  {}  {:?}", i, tt, t.lexeme);
    }
}

pub fn ast(script: &Script) {
    println!("Script ({} stmts)", script.body.len());
    for (i, stmt) in script.body.iter().enumerate() {
        print_stmt(i, stmt, 0);
    }
}

fn print_stmt(idx: usize, stmt: &Statement, indent: usize) {
    let pad = "  ".repeat(indent);
    match stmt {
        Statement::Block(s) => {
            println!("{}[{}] Block", pad, idx);
            for (i, s) in s.body.iter().enumerate() {
                print_stmt(i, s, indent + 1);
            }
        }
        Statement::FunctionDecl(s) => {
            println!("{}[{}] FunctionDecl {} ({})", pad, idx, s.name, s.params.join(", "));
            print_stmt(0, &s.body, indent + 1);
        }
        Statement::Return(s) => {
            let arg = s.argument.as_ref().map(|e| format_expr(e)).unwrap_or_else(|| "".to_string());
            println!("{}[{}] Return {}", pad, idx, arg);
        }
        Statement::If(s) => {
            println!("{}[{}] If", pad, idx);
            println!("{}  cond: {}", pad, format_expr(&s.condition));
            print_stmt(0, &s.then_branch, indent + 1);
            if let Some(else_b) = &s.else_branch {
                println!("{}  else:", pad);
                print_stmt(0, else_b, indent + 1);
            }
        }
        Statement::While(s) => {
            println!("{}[{}] While cond: {}", pad, idx, format_expr(&s.condition));
            print_stmt(0, &s.body, indent + 1);
        }
        Statement::For(s) => {
            println!("{}[{}] For", pad, idx);
            print_stmt(0, &s.body, indent + 1);
        }
        Statement::VarDecl(s) => {
            for d in &s.declarations {
                let init = d.init.as_ref().map(|e| format_expr(e)).unwrap_or_else(|| "".to_string());
                println!("{}[{}] var {} = {}", pad, idx, d.name, init);
            }
        }
        Statement::LetDecl(s) => {
            for d in &s.declarations {
                let init = d.init.as_ref().map(|e| format_expr(e)).unwrap_or_else(|| "".to_string());
                println!("{}[{}] let {} = {}", pad, idx, d.name, init);
            }
        }
        Statement::ConstDecl(s) => {
            for d in &s.declarations {
                let init = d.init.as_ref().map(|e| format_expr(e)).unwrap_or_else(|| "".to_string());
                println!("{}[{}] const {} = {}", pad, idx, d.name, init);
            }
        }
        Statement::Expression(s) => {
            println!("{}[{}] Expr {}", pad, idx, format_expr(&s.expression));
        }
    }
}

fn format_expr(expr: &Expression) -> String {
    match expr {
        Expression::Literal(e) => format!("{:?}", e.value),
        Expression::Identifier(e) => e.name.clone(),
        Expression::Binary(e) => format!("({} {:?} {})", format_expr(&e.left), e.op, format_expr(&e.right)),
        Expression::Unary(e) => format!("({:?} {})", e.op, format_expr(&e.argument)),
        Expression::Call(e) => {
            let args: Vec<String> = e.args.iter().map(format_expr).collect();
            format!("{}({})", format_expr(&e.callee), args.join(", "))
        }
        Expression::Assign(e) => format!("{} = {}", format_expr(&e.left), format_expr(&e.right)),
    }
}
