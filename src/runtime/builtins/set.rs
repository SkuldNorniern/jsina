use crate::runtime::builtins::to_prop_key;
use crate::runtime::{Heap, Value};

fn add_from_array(heap: &mut Heap, set_id: usize, arr_id: usize) {
    let len = heap.array_len(arr_id);
    for i in 0..len {
        let val = heap.get_array_prop(arr_id, &i.to_string());
        let key = to_prop_key(&val);
        heap.set_add(set_id, &key);
    }
}

pub fn create(args: &[Value], heap: &mut Heap) -> Value {
    let id = heap.alloc_set();
    if let Some(iterable) = args.first()
        && let Value::Array(arr_id) = iterable
    {
        add_from_array(heap, id, *arr_id);
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_empty() {
        let mut heap = Heap::new();
        let v = create(&[], &mut heap);
        assert!(matches!(v, Value::Set(_)));
    }

    #[test]
    fn create_from_array() {
        let mut heap = Heap::new();
        let arr = heap.alloc_array();
        heap.array_push(arr, Value::Int(1));
        heap.array_push(arr, Value::Int(2));
        heap.array_push(arr, Value::Int(2));
        heap.array_push(arr, Value::Int(3));
        let v = create(&[Value::Array(arr)], &mut heap);
        let set_id = match v {
            Value::Set(id) => id,
            _ => panic!("expected Set"),
        };
        assert_eq!(heap.set_size(set_id), 3);
    }
}
