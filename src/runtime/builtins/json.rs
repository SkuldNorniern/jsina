use super::{BuiltinContext, BuiltinError};
use crate::runtime::{Heap, Value};

pub fn parse(args: &[Value], ctx: &mut BuiltinContext) -> Result<Value, BuiltinError> {
    let s = match args.first() {
        Some(Value::String(s)) => s.clone(),
        _ => return Err(BuiltinError::Throw(Value::String("JSON.parse requires a string".to_string()))),
    };
    match crate::runtime::json_parse(&s, ctx.heap) {
        Ok(v) => Ok(v),
        Err(e) => Err(BuiltinError::Throw(Value::String(e.message))),
    }
}

pub fn stringify(args: &[Value], heap: &mut Heap) -> Value {
    let arg = args.first().unwrap_or(&Value::Undefined);
    match crate::runtime::json_stringify(arg, heap) {
        Some(s) => Value::String(s),
        None => Value::Undefined,
    }
}
