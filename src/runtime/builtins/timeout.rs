//! timeout(callback, delay) - test262 host hook stub. Throws when called.

use super::{BuiltinContext, BuiltinError};
use crate::runtime::Value;

pub fn timeout(args: &[Value], _ctx: &mut BuiltinContext) -> Result<Value, BuiltinError> {
    let _ = args;
    Err(BuiltinError::Throw(Value::String(
        "timeout is not implemented".to_string(),
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Heap;

    #[test]
    fn timeout_throws_not_implemented() {
        let mut heap = Heap::new();
        let mut dynamic_chunks = Vec::new();
        let mut ctx = BuiltinContext {
            heap: &mut heap,
            dynamic_chunks: &mut dynamic_chunks,
        };
        let r = timeout(&[], &mut ctx);
        assert!(r.is_err());
        if let Err(BuiltinError::Throw(Value::String(s))) = r {
            assert!(s.contains("timeout"));
        } else {
            panic!("expected Throw with string");
        }
    }
}
