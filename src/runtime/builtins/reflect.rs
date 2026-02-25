//! Reflect builtin stubs for test262. apply/construct throw; improves error over "callee is not a function".

use crate::runtime::{Heap, Value};
use super::BuiltinError;

pub fn reflect_apply(args: &[Value], _heap: &mut Heap) -> Result<Value, BuiltinError> {
    let _ = args;
    Err(BuiltinError::Throw(Value::String(
        "Reflect.apply is not implemented".to_string(),
    )))
}

pub fn reflect_construct(args: &[Value], _heap: &mut Heap) -> Result<Value, BuiltinError> {
    let _ = args;
    Err(BuiltinError::Throw(Value::String(
        "Reflect.construct is not implemented".to_string(),
    )))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reflect_apply_throws_not_implemented() {
        let mut heap = Heap::new();
        let r = reflect_apply(&[], &mut heap);
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
        let r = reflect_construct(&[], &mut heap);
        assert!(r.is_err());
        if let Err(BuiltinError::Throw(Value::String(s))) = r {
            assert!(s.contains("Reflect.construct"));
        } else {
            panic!("expected Throw with string");
        }
    }
}
