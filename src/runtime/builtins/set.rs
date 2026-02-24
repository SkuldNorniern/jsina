use crate::runtime::{Heap, Value};

pub fn create(_args: &[Value], heap: &mut Heap) -> Value {
    let id = heap.alloc_set();
    Value::Set(id)
}

pub fn add(args: &[Value], heap: &mut Heap) -> Value {
    let (set_val, value) = match args {
        [s, v] => (s, v),
        _ => return Value::Undefined,
    };
    let set_id = match set_val.as_set_id() {
        Some(id) => id,
        None => return Value::Undefined,
    };
    let key = crate::runtime::builtins::to_prop_key(value);
    heap.set_add(set_id, &key);
    value.clone()
}

pub fn has(args: &[Value], heap: &mut Heap) -> Value {
    let (set_val, value) = match args {
        [s, v] => (s, v),
        _ => return Value::Bool(false),
    };
    let set_id = match set_val.as_set_id() {
        Some(id) => id,
        None => return Value::Bool(false),
    };
    let key = crate::runtime::builtins::to_prop_key(value);
    Value::Bool(heap.set_has(set_id, &key))
}

pub fn size(args: &[Value], heap: &mut Heap) -> Value {
    let set_val = match args.first() {
        Some(s) => s,
        None => return Value::Int(0),
    };
    let set_id = match set_val.as_set_id() {
        Some(id) => id,
        None => return Value::Int(0),
    };
    Value::Int(heap.set_size(set_id) as i32)
}
