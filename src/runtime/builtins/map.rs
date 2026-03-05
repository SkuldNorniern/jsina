use crate::runtime::builtins::to_prop_key;
use crate::runtime::{Heap, Value};

fn add_from_entries_array(heap: &mut Heap, map_id: usize, arr_id: usize) {
    let len = heap.array_len(arr_id);
    for i in 0..len {
        let entry = heap.get_array_prop(arr_id, &i.to_string());
        let (key_val, val) = match &entry {
            Value::Array(entry_id) => {
                let k = heap.get_array_prop(*entry_id, "0");
                let v = heap.get_array_prop(*entry_id, "1");
                (k, v)
            }
            _ => continue,
        };
        let key = to_prop_key(&key_val);
        heap.map_set(map_id, &key, val);
    }
}

pub fn create(args: &[Value], heap: &mut Heap) -> Value {
    let id = heap.alloc_map();
    if let Some(iterable) = args.first() {
        if let Value::Array(arr_id) = iterable {
            add_from_entries_array(heap, id, *arr_id);
        }
    }
    Value::Map(id)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_empty() {
        let mut heap = Heap::new();
        let v = create(&[], &mut heap);
        assert!(matches!(v, Value::Map(_)));
    }

    #[test]
    fn create_from_entries() {
        let mut heap = Heap::new();
        let e1 = heap.alloc_array();
        heap.array_push(e1, Value::String("a".to_string()));
        heap.array_push(e1, Value::Int(1));
        let e2 = heap.alloc_array();
        heap.array_push(e2, Value::String("b".to_string()));
        heap.array_push(e2, Value::Int(2));
        let arr = heap.alloc_array();
        heap.array_push(arr, Value::Array(e1));
        heap.array_push(arr, Value::Array(e2));
        let v = create(&[Value::Array(arr)], &mut heap);
        let map_id = match v {
            Value::Map(id) => id,
            _ => panic!("expected Map"),
        };
        assert_eq!(heap.map_get(map_id, "a"), Value::Int(1));
        assert_eq!(heap.map_get(map_id, "b"), Value::Int(2));
    }
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
