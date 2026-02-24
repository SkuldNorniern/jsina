use super::Value;
use std::collections::{HashMap, HashSet};

const MAX_ARRAY_LENGTH: usize = 10_000_000;

#[derive(Debug)]
struct HeapObject {
    props: HashMap<String, Value>,
    prototype: Option<usize>,
}

#[derive(Debug, Default)]
pub struct Heap {
    objects: Vec<HeapObject>,
    arrays: Vec<Vec<Value>>,
    array_props: Vec<HashMap<String, Value>>,
    maps: Vec<std::collections::HashMap<String, Value>>,
    sets: Vec<std::collections::HashSet<String>>,
    dates: Vec<f64>,
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
        self.array_props.push(HashMap::new());
        id
    }

    pub fn alloc_map(&mut self) -> usize {
        let id = self.maps.len();
        self.maps.push(std::collections::HashMap::new());
        id
    }

    pub fn map_set(&mut self, map_id: usize, key: &str, value: Value) {
        if let Some(m) = self.maps.get_mut(map_id) {
            m.insert(key.to_string(), value);
        }
    }

    pub fn map_get(&self, map_id: usize, key: &str) -> Value {
        self.maps
            .get(map_id)
            .and_then(|m| m.get(key).cloned())
            .unwrap_or(Value::Undefined)
    }

    pub fn map_has(&self, map_id: usize, key: &str) -> bool {
        self.maps
            .get(map_id)
            .map(|m| m.contains_key(key))
            .unwrap_or(false)
    }

    pub fn map_size(&self, map_id: usize) -> usize {
        self.maps.get(map_id).map(|m| m.len()).unwrap_or(0)
    }

    pub fn alloc_set(&mut self) -> usize {
        let id = self.sets.len();
        self.sets.push(std::collections::HashSet::new());
        id
    }

    pub fn set_add(&mut self, set_id: usize, key: &str) {
        if let Some(s) = self.sets.get_mut(set_id) {
            s.insert(key.to_string());
        }
    }

    pub fn set_has(&self, set_id: usize, key: &str) -> bool {
        self.sets
            .get(set_id)
            .map(|s| s.contains(key))
            .unwrap_or(false)
    }

    pub fn set_size(&self, set_id: usize) -> usize {
        self.sets.get(set_id).map(|s| s.len()).unwrap_or(0)
    }

    pub fn alloc_date(&mut self, timestamp_ms: f64) -> usize {
        let id = self.dates.len();
        self.dates.push(timestamp_ms);
        id
    }

    pub fn date_timestamp(&self, date_id: usize) -> f64 {
        self.dates.get(date_id).copied().unwrap_or(0.0)
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
                if idx < elements.len() {
                    return elements.get(idx).cloned().unwrap_or(Value::Undefined);
                }
                if let Some(props) = self.array_props.get(arr_id) {
                    if let Some(v) = props.get(key) {
                        return v.clone();
                    }
                }
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
                        let n = n as usize;
                        elements.truncate(n.min(MAX_ARRAY_LENGTH));
                    }
                }
                return;
            }
            if let Ok(idx) = key.parse::<usize>() {
                if idx < MAX_ARRAY_LENGTH {
                    while elements.len() <= idx {
                        elements.push(Value::Undefined);
                    }
                    elements[idx] = value;
                } else if let Some(props) = self.array_props.get_mut(arr_id) {
                    props.insert(key.to_string(), value);
                }
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
