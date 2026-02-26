mod error;
mod lower;
mod runtime;
mod session;
mod source;

pub use error::BackendError;
pub use session::JitSession;
pub use source::{run_via_jit, translate_to_lamina_ir};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::bytecode::{BytecodeChunk, ConstEntry};

    #[test]
    fn translate_simple_main() {
        let ir = translate_to_lamina_ir("function main() { return 42; }").expect("translate");
        assert!(ir.contains("ret.i64 42"));
        assert!(ir.contains("@main"));
    }

    #[test]
    fn jit_session_trivial_add() {
        let chunk = BytecodeChunk {
            code: vec![0x01, 0, 0x01, 1, 0x10, 0x20],
            constants: vec![ConstEntry::Int(10), ConstEntry::Int(32)],
            num_locals: 0,
            named_locals: vec![],
            captured_names: vec![],
            rest_param_index: None,
            handlers: vec![],
        };
        let mut jit = JitSession::new();
        let result = jit.try_compile(0, &chunk).expect("compile");
        assert_eq!(result, Some(42));
    }

    #[test]
    fn jit_session_trivial_compare() {
        let chunk = BytecodeChunk {
            code: vec![0x01, 0, 0x01, 1, 0x1a, 0x20],
            constants: vec![ConstEntry::Int(10), ConstEntry::Int(2)],
            num_locals: 0,
            named_locals: vec![],
            captured_names: vec![],
            rest_param_index: None,
            handlers: vec![],
        };
        let mut jit = JitSession::new();
        let result = jit.try_compile(0, &chunk).expect("compile");
        assert_eq!(result, Some(1));
    }

    #[test]
    fn jit_session_trivial_bitwise() {
        let chunk = BytecodeChunk {
            code: vec![0x01, 0, 0x01, 1, 0x24, 0x20],
            constants: vec![ConstEntry::Int(42), ConstEntry::Int(15)],
            num_locals: 0,
            named_locals: vec![],
            captured_names: vec![],
            rest_param_index: None,
            handlers: vec![],
        };
        let mut jit = JitSession::new();
        let result = jit.try_compile(0, &chunk).expect("compile");
        assert_eq!(result, Some(10));
    }

    #[test]
    fn jit_session_reuses_compiled_cache_entry() {
        let chunk = BytecodeChunk {
            code: vec![0x01, 0, 0x20],
            constants: vec![ConstEntry::Int(9)],
            num_locals: 0,
            named_locals: vec![],
            captured_names: vec![],
            rest_param_index: None,
            handlers: vec![],
        };
        let mut jit = JitSession::new();
        let first_result = jit.try_compile(0, &chunk).expect("compile first time");
        let second_result = jit.try_compile(0, &chunk).expect("compile second time");
        assert_eq!(first_result, Some(9));
        assert_eq!(second_result, Some(9));
        assert_eq!(jit.compilation_attempt_count(), 1);
        assert!(jit.has_compiled(0));
    }

    #[test]
    fn jit_session_reuses_rejected_cache_entry() {
        let chunk = BytecodeChunk {
            code: vec![0x03, 0, 0x20],
            constants: vec![],
            num_locals: 1,
            named_locals: vec![],
            captured_names: vec![],
            rest_param_index: None,
            handlers: vec![],
        };
        let mut jit = JitSession::new();
        let first_result = jit.try_compile(0, &chunk).expect("compile first time");
        let second_result = jit.try_compile(0, &chunk).expect("compile second time");
        assert_eq!(first_result, None);
        assert_eq!(second_result, None);
        assert_eq!(jit.compilation_attempt_count(), 1);
        assert!(!jit.has_compiled(0));
    }
}
