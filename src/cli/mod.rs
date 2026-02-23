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
    run     Execute a JavaScript file (default)
    tokens  Dump tokens from source
    ast     Dump AST (not implemented)
    hir     Dump HIR / Lamina IR
    bc      Dump bytecode (not implemented)
    ir      Alias for hir - dump Lamina IR

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
            Driver::ast(&source)?;
        }
        "hir" | "ir" => {
            let ir = Driver::hir(&source)?;
            println!("{}", ir);
        }
        "bc" => {
            Driver::bc(&source)?;
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

    for arg in args.iter().skip(1) {
        if arg == "-h" || arg == "--help" {
            command = Some("help".to_string());
        } else if ["run", "tokens", "ast", "hir", "bc", "ir"].contains(&arg.as_str()) {
            if command.is_none() {
                command = Some(arg.clone());
            } else if path.is_none() {
                path = Some(arg.as_str());
            }
        } else if !arg.starts_with('-') && path.is_none() {
            path = Some(arg.as_str());
        }
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
            Ok("function main() { return 50; }".to_string())
        }
    }
}
