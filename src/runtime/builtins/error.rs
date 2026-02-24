use crate::runtime::{Heap, Value};

pub fn error(args: &[Value], heap: &mut Heap) -> Value {
    let msg = args.first().map(|v| v.to_string()).unwrap_or_default();
    let obj_id = heap.alloc_object();
    heap.set_prop(obj_id, "message", Value::String(msg));
    Value::Object(obj_id)
}
