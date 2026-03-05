//! timeout(callback, delay) - test262 host hook stub.
//!
//! test262 harness scripts may call this helper to schedule asynchronous cleanup.
//! jsina executes callbacks immediately so async harness tests can complete.

use super::{BuiltinContext, BuiltinError};
use crate::runtime::Value;

fn is_callable(value: &Value) -> bool {
    matches!(
        value,
        Value::Function(_)
            | Value::DynamicFunction(_)
            | Value::Builtin(_)
            | Value::BoundBuiltin(_, _, _)
            | Value::BoundFunction(_, _, _)
    )
}

pub fn timeout(args: &[Value], _ctx: &mut BuiltinContext) -> Result<Value, BuiltinError> {
    let callback = match args.first() {
        None => return Ok(Value::Int(0)),
        Some(value) => value.clone(),
    };

    if is_callable(&callback) {
        return Err(BuiltinError::Invoke {
            callee: callback,
            this_arg: Value::Undefined,
            args: Vec::new(),
            new_object: None,
        });
    }

    Ok(Value::Undefined)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Heap;

    #[test]
    fn timeout_no_args_returns_number() {
        let mut heap = Heap::new();
        let mut ctx = BuiltinContext { heap: &mut heap };
        let r = timeout(&[], &mut ctx);
        assert!(matches!(r, Ok(Value::Int(0))));
    }

    #[test]
    fn timeout_callable_requests_invoke() {
        let mut heap = Heap::new();
        let mut ctx = BuiltinContext { heap: &mut heap };
        let r = timeout(&[Value::Builtin(0)], &mut ctx);
        assert!(matches!(r, Err(BuiltinError::Invoke { .. })));
    }
}
