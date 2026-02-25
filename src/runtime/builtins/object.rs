use super::to_prop_key;
use crate::runtime::{Heap, Value};

pub fn create(args: &[Value], heap: &mut Heap) -> Value {
    let prototype = args.first().and_then(|p| match p {
        Value::Null | Value::Undefined => None,
        Value::Object(id) => Some(*id),
        _ => None,
    });
    let id = heap.alloc_object_with_prototype(prototype);
    Value::Object(id)
}

pub fn keys(args: &[Value], heap: &mut Heap) -> Value {
    let arr_id = heap.alloc_array();
    if let Some(Value::Object(obj_id)) = args.first() {
        for key in heap.object_keys(*obj_id) {
            heap.array_push(arr_id, Value::String(key));
        }
    }
    Value::Array(arr_id)
}

pub fn assign(args: &[Value], heap: &mut Heap) -> Value {
    let target = match args.first() {
        Some(v) => v,
        None => return Value::Undefined,
    };
    let target_id = match target {
        Value::Object(id) => *id,
        _ => return target.clone(),
    };
    for source in args.iter().skip(1) {
        if let Value::Object(src_id) = source {
            for key in heap.object_keys(*src_id) {
                let val = heap.get_prop(*src_id, &key);
                heap.set_prop(target_id, &key, val);
            }
        }
    }
    Value::Object(target_id)
}

pub fn has_own_property(args: &[Value], heap: &mut Heap) -> Value {
    let key = args.get(1).map(to_prop_key).unwrap_or_default();
    let result = match args.first() {
        Some(Value::Object(id)) => heap.object_has_own_property(*id, &key),
        _ => false,
    };
    Value::Bool(result)
}

pub fn prevent_extensions(args: &[Value], _heap: &mut Heap) -> Value {
    args.first().cloned().unwrap_or(Value::Undefined)
}

pub fn seal(args: &[Value], _heap: &mut Heap) -> Value {
    args.first().cloned().unwrap_or(Value::Undefined)
}

pub fn set_prototype_of(args: &[Value], heap: &mut Heap) -> Value {
    let target = match args.first() {
        Some(Value::Object(id)) => *id,
        _ => return Value::Bool(false),
    };
    let proto = match args.get(1) {
        Some(Value::Object(id)) => Some(*id),
        Some(Value::Null) => None,
        _ => return Value::Bool(false),
    };
    heap.set_prototype(target, proto);
    args.first().cloned().unwrap_or(Value::Undefined)
}

pub fn property_is_enumerable(args: &[Value], heap: &mut Heap) -> Value {
    let key = args.get(1).map(to_prop_key).unwrap_or_default();
    let result = match args.first() {
        Some(Value::Object(id)) => heap.object_has_own_property(*id, &key),
        _ => false,
    };
    Value::Bool(result)
}

pub fn get_prototype_of(args: &[Value], heap: &mut Heap) -> Value {
    match args.first() {
        Some(Value::Object(id)) => match heap.get_proto(*id) {
            Some(proto_id) => Value::Object(proto_id),
            None => Value::Null,
        },
        _ => Value::Null,
    }
}

pub fn freeze(args: &[Value], _heap: &mut Heap) -> Value {
    args.first().cloned().unwrap_or(Value::Undefined)
}

pub fn is_extensible(args: &[Value], _heap: &mut Heap) -> Value {
    Value::Bool(args.first().map(|v| matches!(v, Value::Object(_))).unwrap_or(false))
}

pub fn is_frozen(_args: &[Value], _heap: &mut Heap) -> Value {
    Value::Bool(false)
}

pub fn is_sealed(_args: &[Value], _heap: &mut Heap) -> Value {
    Value::Bool(false)
}

pub fn has_own(args: &[Value], heap: &mut Heap) -> Value {
    let key = args.get(1).map(to_prop_key).unwrap_or_default();
    let result = match args.first() {
        Some(Value::Object(id)) => heap.object_has_own_property(*id, &key),
        _ => false,
    };
    Value::Bool(result)
}
