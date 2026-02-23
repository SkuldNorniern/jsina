use crate::cli::CliError;
use crate::driver::Driver;
use crate::frontend::{Expression, Script, Statement, TokenType};
use crate::test262::{load_allowlist, run_test, TestStatus};
use std::path::Path;

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
        Statement::Break(s) => {
            let lbl = s.label.as_deref().unwrap_or("");
            println!("{}[{}] Break {}", pad, idx, lbl);
        }
        Statement::Continue(s) => {
            let lbl = s.label.as_deref().unwrap_or("");
            println!("{}[{}] Continue {}", pad, idx, lbl);
        }
        Statement::Throw(s) => {
            println!("{}[{}] Throw {}", pad, idx, format_expr(&s.argument));
        }
        Statement::Try(s) => {
            println!("{}[{}] Try", pad, idx);
            print_stmt(0, &s.body, indent + 1);
            if let (Some(p), Some(c)) = (&s.catch_param, &s.catch_body) {
                println!("{}  catch ({}):", pad, p);
                print_stmt(0, c, indent + 2);
            }
            if let Some(f) = &s.finally_body {
                println!("{}  finally:", pad);
                print_stmt(0, f, indent + 2);
            }
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
        Expression::Conditional(e) => format!("{} ? {} : {}", format_expr(&e.condition), format_expr(&e.then_expr), format_expr(&e.else_expr)),
        Expression::ObjectLiteral(e) => {
            let props: Vec<String> = e.properties.iter()
                .map(|(k, v)| format!("{}: {}", k, format_expr(v)))
                .collect();
            format!("{{{}}}", props.join(", "))
        }
        Expression::ArrayLiteral(e) => {
            let elems: Vec<String> = e.elements.iter().map(format_expr).collect();
            format!("[{}]", elems.join(", "))
        }
        Expression::Member(e) => match &e.property {
            crate::frontend::ast::MemberProperty::Identifier(s) => {
                format!("{}.{}", format_expr(&e.object), s)
            }
            crate::frontend::ast::MemberProperty::Expression(inner) => {
                format!("{}[{}]", format_expr(&e.object), format_expr(inner))
            }
        },
    }
}

#[cfg_attr(not(debug_assertions), allow(dead_code))]
pub fn test262(test262_dir: Option<&str>) -> Result<(), CliError> {
    let cwd = std::env::current_dir().map_err(|e| CliError::Usage(e.to_string()))?;
    let allowlist_path = cwd.join("test262").join("allowlist.txt");
    let entries = load_allowlist(&allowlist_path);

    let test262_root: Option<std::path::PathBuf> = test262_dir
        .map(Path::new)
        .filter(|p| p.exists())
        .map(|p| p.to_path_buf())
        .or_else(|| {
            std::env::var_os("TEST262_ROOT")
                .map(std::path::PathBuf::from)
                .filter(|p| p.exists())
        });

    let mut pass = 0;
    let mut fail = 0;
    let mut skip = 0;

    for entry in &entries {
        let test_path = Path::new(&entry.test_path);
        let result = run_test(test_path, test262_root.as_deref());
        match result.status {
            TestStatus::Pass => {
                pass += 1;
                println!("PASS  {}", result.path);
            }
            TestStatus::Fail => {
                fail += 1;
                println!("FAIL  {}  {}", result.path, result.message.as_deref().unwrap_or(""));
            }
            TestStatus::SkipFeature | TestStatus::SkipParse => {
                skip += 1;
                println!("SKIP  {}", result.path);
            }
            TestStatus::HarnessError => {
                fail += 1;
                println!("ERROR {}  {}", result.path, result.message.as_deref().unwrap_or(""));
            }
        }
    }

    println!(
        "\ntest262: {} passed, {} failed, {} skipped (allowlist: {})",
        pass,
        fail,
        skip,
        entries.len()
    );

    if fail > 0 {
        return Err(CliError::Usage(format!("{} test(s) failed", fail)));
    }
    Ok(())
}
