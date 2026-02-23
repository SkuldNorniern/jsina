use crate::backend::translate_to_lamina_ir;
use crate::diagnostics::Diagnostic;
use crate::frontend::{Lexer, Parser};
use crate::ir::{hir_to_bytecode, script_to_hir};
use crate::vm::{interpret, Completion};

#[derive(Debug)]
pub enum DriverError {
    Backend(crate::backend::BackendError),
    Diagnostic(Vec<Diagnostic>),
    Parse(crate::frontend::ParseError),
    Lower(crate::ir::LowerError),
    Vm(crate::vm::interpreter::VmError),
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
            DriverError::Lower(e) => write!(f, "{}", e),
            DriverError::Vm(e) => write!(f, "{}", e),
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

impl From<crate::ir::LowerError> for DriverError {
    fn from(e: crate::ir::LowerError) -> Self {
        DriverError::Lower(e)
    }
}

impl From<crate::vm::interpreter::VmError> for DriverError {
    fn from(e: crate::vm::interpreter::VmError) -> Self {
        DriverError::Vm(e)
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

    pub fn bc(source: &str) -> Result<String, DriverError> {
        let script = Self::ast(source)?;
        let funcs = script_to_hir(&script)?;
        let main = funcs
            .iter()
            .find(|f| f.name.as_deref() == Some("main"))
            .or(funcs.first())
            .ok_or_else(|| DriverError::Diagnostic(vec![Diagnostic::error(
                "JSINA-BC-001",
                "no function to compile",
                None,
            )]))?;
        let cf = hir_to_bytecode(main);
        Ok(crate::ir::disassemble(&cf.chunk))
    }

    pub fn ir(source: &str) -> Result<String, DriverError> {
        translate_to_lamina_ir(source).map_err(DriverError::Backend)
    }

    pub fn run(source: &str) -> Result<i64, DriverError> {
        let script = Self::ast(source)?;
        let funcs = script_to_hir(&script)?;
        let main = funcs
            .iter()
            .find(|f| f.name.as_deref() == Some("main"))
            .ok_or_else(|| DriverError::Diagnostic(vec![Diagnostic::error(
                "JSINA-RUN-001",
                "no main function found",
                None,
            )]))?;
        let cf = hir_to_bytecode(main);
        let completion = interpret(&cf.chunk)?;
        let value = match completion {
            Completion::Return(v) => v,
            Completion::Normal(v) => v,
            Completion::Throw(v) => {
                return Err(DriverError::Diagnostic(vec![Diagnostic::error(
                    "JSINA-RUN-002",
                    format!("uncaught exception: {}", v),
                    None,
                )]));
            }
        };
        Ok(value.to_i64())
    }
}
