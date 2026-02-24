//! test262 $262 host object stubs. Minimal implementation for harness-dependent tests.

use crate::runtime::{Heap, Value};

pub fn create_realm(args: &[Value], heap: &mut Heap) -> Result<Value, super::BuiltinError> {
    let _ = (args, heap);
    Err(super::BuiltinError::Throw(Value::String(
        "$262.createRealm is not implemented".to_string(),
    )))
}

pub fn eval_script(args: &[Value], heap: &mut Heap) -> Result<Value, super::BuiltinError> {
    let _ = (args, heap);
    Err(super::BuiltinError::Throw(Value::String(
        "$262.evalScript is not implemented".to_string(),
    )))
}

pub fn gc(args: &[Value], heap: &mut Heap) -> Result<Value, super::BuiltinError> {
    let _ = (args, heap);
    Err(super::BuiltinError::Throw(Value::String(
        "$262.gc is not implemented".to_string(),
    )))
}

pub fn detach_array_buffer(args: &[Value], heap: &mut Heap) -> Result<Value, super::BuiltinError> {
    let _ = (args, heap);
    Err(super::BuiltinError::Throw(Value::String(
        "$262.detachArrayBuffer is not implemented".to_string(),
    )))
}
