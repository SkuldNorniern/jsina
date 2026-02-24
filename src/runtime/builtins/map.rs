use crate::runtime::{Heap, Value};

pub fn create(_args: &[Value], heap: &mut Heap) -> Value {
    let id = heap.alloc_map();
    Value::Map(id)
}

pub fn set(args: &[Value], heap: &mut Heap) -> Value {
    let (map_val, key_val, value) = match args {
        [m, k, v] => (m, k, v),
        _ => return Value::Undefined,
    };
    let map_id = match map_val.as_map_id() {
        Some(id) => id,
        None => return Value::Undefined,
    };
    let key = crate::runtime::builtins::to_prop_key(key_val);
    heap.map_set(map_id, &key, value.clone());
    value.clone()
}

pub fn get(args: &[Value], heap: &mut Heap) -> Value {
    let (map_val, key_val) = match args {
        [m, k] => (m, k),
        _ => return Value::Undefined,
    };
    let map_id = match map_val.as_map_id() {
        Some(id) => id,
        None => return Value::Undefined,
    };
    let key = crate::runtime::builtins::to_prop_key(key_val);
    heap.map_get(map_id, &key)
}

pub fn has(args: &[Value], heap: &mut Heap) -> Value {
    let (map_val, key_val) = match args {
        [m, k] => (m, k),
        _ => return Value::Bool(false),
    };
    let map_id = match map_val.as_map_id() {
        Some(id) => id,
        None => return Value::Bool(false),
    };
    let key = crate::runtime::builtins::to_prop_key(key_val);
    Value::Bool(heap.map_has(map_id, &key))
}
