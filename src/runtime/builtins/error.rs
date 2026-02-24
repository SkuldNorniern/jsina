use crate::runtime::{Heap, Value};

pub fn error(args: &[Value], heap: &mut Heap) -> Value {
    let msg = args.first().map(|v| v.to_string()).unwrap_or_default();
    let obj_id = heap.alloc_object();
    heap.record_error_object(obj_id);
    heap.set_prop(obj_id, "message", Value::String(msg));
    Value::Object(obj_id)
}

pub fn is_error(args: &[Value], heap: &Heap) -> Value {
    let result = match args.first() {
        Some(Value::Object(id)) => heap.is_error_object(*id),
        _ => false,
    };
    Value::Bool(result)
}
