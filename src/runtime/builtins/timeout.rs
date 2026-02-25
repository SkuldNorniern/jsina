//! timeout(callback, delay) - test262 host hook stub. Throws when called.

use crate::runtime::{Heap, Value};
use super::BuiltinError;

pub fn timeout(args: &[Value], _heap: &mut Heap) -> Result<Value, BuiltinError> {
    let _ = args;
    Err(BuiltinError::Throw(Value::String(
        "timeout is not implemented".to_string(),
    )))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn timeout_throws_not_implemented() {
        let mut heap = Heap::new();
        let r = timeout(&[], &mut heap);
        assert!(r.is_err());
        if let Err(BuiltinError::Throw(Value::String(s))) = r {
            assert!(s.contains("timeout"));
        } else {
            panic!("expected Throw with string");
        }
    }
}
