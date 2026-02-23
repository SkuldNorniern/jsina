mod commands;

use std::fs;
use std::path::Path;

use crate::driver::Driver;

#[derive(Debug)]
pub enum CliError {
    Io(std::io::Error),
    Driver(crate::driver::DriverError),
    Usage(String),
}

impl std::fmt::Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CliError::Io(e) => write!(f, "I/O error: {}", e),
            CliError::Driver(e) => write!(f, "{}", e),
            CliError::Usage(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for CliError {}

impl From<std::io::Error> for CliError {
    fn from(err: std::io::Error) -> Self {
        CliError::Io(err)
    }
}

impl From<crate::driver::DriverError> for CliError {
    fn from(err: crate::driver::DriverError) -> Self {
        CliError::Driver(err)
    }
}

const HELP: &str = r#"jsina - JavaScript engine (ECMAScript 2026 aware)

USAGE:
    jsina <command> [options] [file]

COMMANDS:
    run      Execute a JavaScript file (default)
    tokens   Dump tokens from source
    ast      Dump AST
    hir      Dump HIR / Lamina IR
    bc       Dump bytecode
    ir       Alias for hir - dump Lamina IR
    test262  Run test262 allowlist (debug build only; use --test262-dir for repo path)

EXAMPLES:
    jsina run script.js
    jsina tokens script.js
    jsina hir script.js
"#;

pub fn run(args: &[String]) -> Result<(), CliError> {
    let (command, path) = parse_args(args)?;
    let source = load_source(path)?;

    match command.as_deref().unwrap_or("run") {
        "run" => {
            let result = Driver::run(&source)?;
            println!("{}", result);
        }
        "tokens" => {
            commands::tokens(&source);
        }
        "ast" => {
            let script = Driver::ast(&source)?;
            commands::ast(&script);
        }
        "hir" | "ir" => {
            let ir = Driver::hir(&source)?;
            println!("{}", ir);
        }
        "bc" => {
            let bc = Driver::bc(&source)?;
            println!("{}", bc);
        }
        "test262" => {
            #[cfg(debug_assertions)]
            {
                commands::test262(path)?;
            }
            #[cfg(not(debug_assertions))]
            {
                return Err(CliError::Usage(
                    "test262 is only available in debug builds (cargo build); use for CI/dev only".to_string(),
                ));
            }
        }
        "help" | "-h" | "--help" => {
            print!("{}", HELP);
        }
        _ => {
            return Err(CliError::Usage(format!("unknown command: {}", command.as_deref().unwrap_or(""))));
        }
    }

    Ok(())
}

fn parse_args(args: &[String]) -> Result<(Option<String>, Option<&str>), CliError> {
    let mut command = None;
    let mut path = None;

    let mut i = 1;
    while i < args.len() {
        let arg = &args[i];
        if arg == "-h" || arg == "--help" {
            command = Some("help".to_string());
        } else if arg == "--test262-dir" {
            i += 1;
            if i < args.len() {
                path = Some(args[i].as_str());
            }
            i += 1;
            continue;
        } else if ["run", "tokens", "ast", "hir", "bc", "ir", "test262"].contains(&arg.as_str()) {
            if command.is_none() {
                command = Some(arg.clone());
            } else if path.is_none() {
                path = Some(arg.as_str());
            }
        } else if !arg.starts_with('-') && path.is_none() {
            path = Some(arg.as_str());
        }
        i += 1;
    }

    Ok((command, path))
}

fn load_source(path: Option<&str>) -> Result<String, CliError> {
    match path {
        Some(p) => {
            let content = fs::read_to_string(Path::new(p))?;
            Ok(content)
        }
        None => {
            Ok("function main() { let x = 10; let y = 40; return x + y; }".to_string())
        }
    }
}
