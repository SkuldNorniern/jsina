use super::is_truthy;
use crate::runtime::Value;

pub fn boolean(args: &[Value]) -> Value {
    let b = args.first().map(is_truthy).unwrap_or(false);
    Value::Bool(b)
}
