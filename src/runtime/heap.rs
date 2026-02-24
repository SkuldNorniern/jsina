use super::Value;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
struct HeapObject {
    props: HashMap<String, Value>,
    prototype: Option<usize>,
}

#[derive(Debug, Default)]
pub struct Heap {
    objects: Vec<HeapObject>,
    arrays: Vec<Vec<Value>>,
    error_object_ids: HashSet<usize>,
}

impl Heap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc_object(&mut self) -> usize {
        self.alloc_object_with_prototype(None)
    }

    pub fn alloc_object_with_prototype(&mut self, prototype: Option<usize>) -> usize {
        let id = self.objects.len();
        self.objects.push(HeapObject {
            props: HashMap::new(),
            prototype,
        });
        id
    }

    pub fn alloc_array(&mut self) -> usize {
        let id = self.arrays.len();
        self.arrays.push(Vec::new());
        id
    }

    pub fn get_prop(&self, obj_id: usize, key: &str) -> Value {
        let mut current = Some(obj_id);
        while let Some(id) = current {
            if let Some(obj) = self.objects.get(id) {
                if let Some(v) = obj.props.get(key) {
                    return v.clone();
                }
                current = obj.prototype;
            } else {
                break;
            }
        }
        Value::Undefined
    }

    pub fn set_prototype(&mut self, obj_id: usize, prototype: Option<usize>) {
        if let Some(obj) = self.objects.get_mut(obj_id) {
            obj.prototype = prototype;
        }
    }

    pub fn get_array_prop(&self, arr_id: usize, key: &str) -> Value {
        if let Some(elements) = self.arrays.get(arr_id) {
            if key == "length" {
                return Value::Int(elements.len() as i32);
            }
            if let Ok(idx) = key.parse::<usize>() {
                return elements.get(idx).cloned().unwrap_or(Value::Undefined);
            }
        }
        Value::Undefined
    }

    pub fn set_prop(&mut self, obj_id: usize, key: &str, value: Value) {
        if let Some(obj) = self.objects.get_mut(obj_id) {
            obj.props.insert(key.to_string(), value);
        }
    }

    pub fn set_array_prop(&mut self, arr_id: usize, key: &str, value: Value) {
        if let Some(elements) = self.arrays.get_mut(arr_id) {
            if key == "length" {
                if let Value::Int(n) = value {
                    if n >= 0 {
                        elements.truncate(n as usize);
                    }
                }
                return;
            }
            if let Ok(idx) = key.parse::<usize>() {
                while elements.len() <= idx {
                    elements.push(Value::Undefined);
                }
                elements[idx] = value;
            }
        }
    }

    pub fn array_push(&mut self, arr_id: usize, value: Value) {
        if let Some(elements) = self.arrays.get_mut(arr_id) {
            elements.push(value);
        }
    }

    pub fn array_push_values(&mut self, arr_id: usize, values: &[Value]) -> i32 {
        if let Some(elements) = self.arrays.get_mut(arr_id) {
            elements.extend(values.iter().cloned());
            elements.len() as i32
        } else {
            0
        }
    }

    pub fn array_pop(&mut self, arr_id: usize) -> Value {
        if let Some(elements) = self.arrays.get_mut(arr_id) {
            elements.pop().unwrap_or(Value::Undefined)
        } else {
            Value::Undefined
        }
    }

    pub fn array_shift(&mut self, arr_id: usize) -> Value {
        if let Some(elements) = self.arrays.get_mut(arr_id) {
            if elements.is_empty() {
                Value::Undefined
            } else {
                elements.remove(0)
            }
        } else {
            Value::Undefined
        }
    }

    pub fn array_unshift(&mut self, arr_id: usize, values: &[Value]) -> i32 {
        if let Some(elements) = self.arrays.get_mut(arr_id) {
            for v in values.iter().rev() {
                elements.insert(0, v.clone());
            }
            elements.len() as i32
        } else {
            0
        }
    }

    pub fn array_reverse(&mut self, arr_id: usize) {
        if let Some(elements) = self.arrays.get_mut(arr_id) {
            elements.reverse();
        }
    }

    pub fn array_fill(&mut self, arr_id: usize, value: Value, start: usize, end: usize) {
        if let Some(elements) = self.arrays.get_mut(arr_id) {
            let len = elements.len();
            let end = end.min(len);
            for i in start..end {
                elements[i] = value.clone();
            }
        }
    }

    pub fn object_has_own_property(&self, obj_id: usize, key: &str) -> bool {
        self.objects
            .get(obj_id)
            .map(|o| o.props.contains_key(key))
            .unwrap_or(false)
    }

    pub fn record_error_object(&mut self, obj_id: usize) {
        self.error_object_ids.insert(obj_id);
    }

    pub fn is_error_object(&self, obj_id: usize) -> bool {
        self.error_object_ids.contains(&obj_id)
    }

    pub fn object_keys(&self, obj_id: usize) -> Vec<String> {
        self.objects
            .get(obj_id)
            .map(|o| o.props.keys().cloned().collect())
            .unwrap_or_default()
    }

    pub fn array_elements(&self, arr_id: usize) -> Option<&[Value]> {
        self.arrays.get(arr_id).map(|v| v.as_slice())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn heap_set_get_prop() {
        let mut heap = Heap::new();
        let id = heap.alloc_object();
        heap.set_prop(id, "x", Value::Int(0));
        assert_eq!(heap.get_prop(id, "x").to_i64(), 0);
        heap.set_prop(id, "x", Value::Int(42));
        assert_eq!(heap.get_prop(id, "x").to_i64(), 42);
    }

    #[test]
    fn heap_prototype_chain() {
        let mut heap = Heap::new();
        let proto = heap.alloc_object();
        heap.set_prop(proto, "y", Value::Int(10));
        let obj = heap.alloc_object_with_prototype(Some(proto));
        heap.set_prop(obj, "x", Value::Int(1));
        assert_eq!(heap.get_prop(obj, "x").to_i64(), 1);
        assert_eq!(heap.get_prop(obj, "y").to_i64(), 10);
    }
}
