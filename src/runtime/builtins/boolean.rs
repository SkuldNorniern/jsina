use super::is_truthy;
use crate::runtime::{Heap, Value};

pub fn boolean(args: &[Value], _heap: &mut Heap) -> Value {
    let b = args.first().map(is_truthy).unwrap_or(false);
    Value::Bool(b)
}
