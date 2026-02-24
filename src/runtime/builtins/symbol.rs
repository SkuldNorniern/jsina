//! Symbol builtin: Symbol(description?) returns a unique symbol value.

use crate::runtime::{Heap, Value};

pub fn symbol(args: &[Value], heap: &mut Heap) -> Value {
    let desc = args.first().and_then(|v| {
        if v == &Value::Undefined {
            None
        } else {
            Some(super::to_prop_key(v))
        }
    });
    let id = heap.alloc_symbol(desc);
    Value::Symbol(id)
}
