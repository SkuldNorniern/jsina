use crate::backend::{translate_to_lamina_ir, run_via_jit};
use crate::diagnostics::Diagnostic;
use crate::frontend::{Lexer, Parser};

#[derive(Debug)]
pub enum DriverError {
    Backend(crate::backend::BackendError),
    Diagnostic(Vec<Diagnostic>),
    Parse(crate::frontend::ParseError),
}

impl std::fmt::Display for DriverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DriverError::Backend(e) => write!(f, "{}", e),
            DriverError::Diagnostic(diags) => {
                for d in diags {
                    write!(f, "{}", d.format(None))?;
                }
                Ok(())
            }
            DriverError::Parse(e) => write!(f, "{}", e),
        }
    }
}

impl std::error::Error for DriverError {}

impl From<crate::backend::BackendError> for DriverError {
    fn from(e: crate::backend::BackendError) -> Self {
        DriverError::Backend(e)
    }
}

impl From<crate::frontend::ParseError> for DriverError {
    fn from(e: crate::frontend::ParseError) -> Self {
        DriverError::Parse(e)
    }
}

pub struct Driver;

impl Driver {
    pub fn tokens(source: &str) -> Vec<crate::frontend::Token> {
        let mut lexer = Lexer::new(source.to_string());
        lexer.tokenize()
    }

    pub fn ast(source: &str) -> Result<crate::frontend::Script, DriverError> {
        let mut parser = Parser::new(source);
        parser.parse().map_err(DriverError::Parse)
    }

    pub fn hir(source: &str) -> Result<String, DriverError> {
        translate_to_lamina_ir(source).map_err(DriverError::Backend)
    }

    pub fn bc(_source: &str) -> Result<(), DriverError> {
        Err(DriverError::Diagnostic(vec![Diagnostic::error(
            "JSINA-BC-001",
            "Bytecode output not yet implemented",
            None,
        )]))
    }

    pub fn ir(source: &str) -> Result<String, DriverError> {
        translate_to_lamina_ir(source).map_err(DriverError::Backend)
    }

    pub fn run(source: &str) -> Result<i64, DriverError> {
        run_via_jit(source).map_err(DriverError::Backend)
    }
}
