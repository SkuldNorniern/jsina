//! Reflect builtin stubs for test262. apply/construct throw; improves error over "callee is not a function".

use super::{BuiltinContext, BuiltinError};
use crate::runtime::Value;

pub fn reflect_apply(args: &[Value], _ctx: &mut BuiltinContext) -> Result<Value, BuiltinError> {
    let _ = args;
    Err(BuiltinError::Throw(Value::String(
        "Reflect.apply is not implemented".to_string(),
    )))
}

pub fn reflect_construct(args: &[Value], _ctx: &mut BuiltinContext) -> Result<Value, BuiltinError> {
    let _ = args;
    Err(BuiltinError::Throw(Value::String(
        "Reflect.construct is not implemented".to_string(),
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Heap;

    #[test]
    fn reflect_apply_throws_not_implemented() {
        let mut heap = Heap::new();
        let mut dynamic_chunks = Vec::new();
        let mut ctx = BuiltinContext {
            heap: &mut heap,
            dynamic_chunks: &mut dynamic_chunks,
        };
        let r = reflect_apply(&[], &mut ctx);
        assert!(r.is_err());
        if let Err(BuiltinError::Throw(Value::String(s))) = r {
            assert!(s.contains("Reflect.apply"));
        } else {
            panic!("expected Throw with string");
        }
    }

    #[test]
    fn reflect_construct_throws_not_implemented() {
        let mut heap = Heap::new();
        let mut dynamic_chunks = Vec::new();
        let mut ctx = BuiltinContext {
            heap: &mut heap,
            dynamic_chunks: &mut dynamic_chunks,
        };
        let r = reflect_construct(&[], &mut ctx);
        assert!(r.is_err());
        if let Err(BuiltinError::Throw(Value::String(s))) = r {
            assert!(s.contains("Reflect.construct"));
        } else {
            panic!("expected Throw with string");
        }
    }
}
