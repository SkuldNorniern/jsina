//! test262 $262 host object stubs. Minimal implementation for harness-dependent tests.

use super::BuiltinContext;
use crate::runtime::Value;

pub fn create_realm(
    args: &[Value],
    ctx: &mut BuiltinContext,
) -> Result<Value, super::BuiltinError> {
    let _ = (args, ctx);
    Err(super::BuiltinError::Throw(Value::String(
        "$262.createRealm is not implemented".to_string(),
    )))
}

pub fn eval_script(args: &[Value], ctx: &mut BuiltinContext) -> Result<Value, super::BuiltinError> {
    let _ = (args, ctx);
    Err(super::BuiltinError::Throw(Value::String(
        "$262.evalScript is not implemented".to_string(),
    )))
}

pub fn gc(args: &[Value], ctx: &mut BuiltinContext) -> Result<Value, super::BuiltinError> {
    let _ = (args, ctx);
    Err(super::BuiltinError::Throw(Value::String(
        "$262.gc is not implemented".to_string(),
    )))
}

pub fn detach_array_buffer(
    args: &[Value],
    ctx: &mut BuiltinContext,
) -> Result<Value, super::BuiltinError> {
    let _ = (args, ctx);
    Err(super::BuiltinError::Throw(Value::String(
        "$262.detachArrayBuffer is not implemented".to_string(),
    )))
}
