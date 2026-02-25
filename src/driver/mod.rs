use crate::backend::translate_to_lamina_ir;
use crate::diagnostics::Diagnostic;
use crate::frontend::{check_early_errors, Lexer, Parser};
use crate::host::{with_host, CliHost, HostHooks};
use crate::diagnostics::ErrorCode;
use crate::ir::{hir_to_bytecode, script_to_hir};
use crate::vm::{Completion, Program};
use std::sync::atomic::AtomicBool;

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
        let script = parser.parse().map_err(DriverError::Parse)?;
        if let Err(early) = check_early_errors(&script) {
            return Err(DriverError::Diagnostic(
                early.into_iter().map(|e| e.to_diagnostic()).collect(),
            ));
        }
        Ok(script)
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
                ErrorCode::BcNoFunction,
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
        Self::run_with_trace(source, false)
    }

    pub fn run_with_trace(source: &str, trace: bool) -> Result<i64, DriverError> {
        let host = CliHost;
        Self::run_with_host(&host, source, trace, false)
    }

    pub fn run_with_jit(source: &str, trace: bool) -> Result<i64, DriverError> {
        let host = CliHost;
        Self::run_with_host(&host, source, trace, true)
    }

    /// Run with custom host. Use for browser embedding (provide HostHooks impl).
    pub fn run_with_host<H: HostHooks + 'static>(
        host: &H,
        source: &str,
        trace: bool,
        enable_jit: bool,
    ) -> Result<i64, DriverError> {
        with_host(host, || Self::run_with_host_inner(source, trace, enable_jit))
    }

    /// Run with step limit. Use for test262 to prevent infinite loops from exhausting memory.
    pub fn run_with_step_limit(source: &str, step_limit: u64) -> Result<i64, DriverError> {
        let host = CliHost;
        with_host(&host, || Self::run_with_host_and_limit_inner(source, false, Some(step_limit), None, false))
    }

    /// Run with step limit and cancellation flag. When cancel is set, execution stops.
    /// Use for test262 to avoid zombie threads when wall-clock timeout fires.
    pub fn run_with_step_limit_and_cancel(
        source: &str,
        step_limit: u64,
        cancel: Option<&AtomicBool>,
    ) -> Result<i64, DriverError> {
        let host = CliHost;
        with_host(&host, || Self::run_with_host_and_limit_inner(source, false, Some(step_limit), cancel, false))
    }

    fn run_with_host_inner(source: &str, trace: bool, enable_jit: bool) -> Result<i64, DriverError> {
        Self::run_with_host_and_limit_inner(source, trace, None, None, enable_jit)
    }

    fn run_with_host_and_limit_inner(
        source: &str,
        trace: bool,
        step_limit: Option<u64>,
        cancel: Option<&AtomicBool>,
        enable_jit: bool,
    ) -> Result<i64, DriverError> {
        let script = Self::ast(source)?;
        let funcs = script_to_hir(&script)?;
        let entry = funcs
            .iter()
            .position(|f| f.name.as_deref() == Some("main"))
            .ok_or_else(|| DriverError::Diagnostic(vec![Diagnostic::error(
                ErrorCode::RunNoMain,
                "no main function found",
                None,
            )]))?;
        let chunks: Vec<_> = funcs.iter().map(|f| hir_to_bytecode(f).chunk).collect();
        let init_entry = funcs
            .iter()
            .position(|f| f.name.as_deref() == Some("__init__"));

        if enable_jit && step_limit.is_none() && init_entry.is_none() {
            let mut jit = crate::backend::JitSession::new();
            let chunk = &chunks[entry];
            if let Ok(Some(result)) = jit.try_compile(entry, chunk) {
                return Ok(result);
            }
        }

        let program = Program {
            chunks,
            entry,
            init_entry,
        };
        let completion = crate::vm::interpret_program_with_limit_and_cancel(
            &program,
            trace,
            step_limit,
            cancel,
            step_limit.is_some(),
        )?;
        let value = match completion {
            Completion::Return(v) => v,
            Completion::Normal(v) => v,
            Completion::Throw(v) => {
                return Err(DriverError::Diagnostic(vec![Diagnostic::error(
                    ErrorCode::RunUncaughtException,
                    format!("uncaught exception: {}", v),
                    None,
                )]));
            }
        };
        Ok(value.to_i64())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;
    use std::rc::Rc;

    struct CaptureHost(Rc<RefCell<Vec<String>>>);

    impl crate::host::HostHooks for CaptureHost {
        fn print(&self, args: &[&str]) {
            for s in args {
                self.0.borrow_mut().push((*s).to_string());
            }
        }
    }

    #[test]
    fn run_with_host_custom_print() {
        let captured = Rc::new(RefCell::new(Vec::new()));
        let host = CaptureHost(captured.clone());
        let r = Driver::run_with_host(&host, "function main() { print(\"hi\"); return 0; }", false, false);
        assert!(r.is_ok());
        let v = captured.borrow();
        assert_eq!(v.as_slice(), &["hi"]);
    }
}
