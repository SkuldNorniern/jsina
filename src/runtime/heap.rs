use super::Value;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct Heap {
    objects: Vec<HashMap<String, Value>>,
    arrays: Vec<Vec<Value>>,
}

impl Heap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc_object(&mut self) -> usize {
        let id = self.objects.len();
        self.objects.push(HashMap::new());
        id
    }

    pub fn alloc_array(&mut self) -> usize {
        let id = self.arrays.len();
        self.arrays.push(Vec::new());
        id
    }

    pub fn get_prop(&self, obj_id: usize, key: &str) -> Value {
        if let Some(props) = self.objects.get(obj_id) {
            props.get(key).cloned().unwrap_or(Value::Undefined)
        } else {
            Value::Undefined
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
        if let Some(props) = self.objects.get_mut(obj_id) {
            props.insert(key.to_string(), value);
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
}
