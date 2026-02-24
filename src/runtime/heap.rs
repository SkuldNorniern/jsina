use super::Value;
use std::collections::{HashMap, HashSet};

const MAX_ARRAY_LENGTH: usize = 10_000_000;

#[derive(Debug)]
struct HeapObject {
    props: HashMap<String, Value>,
    prototype: Option<usize>,
}

#[derive(Debug)]
pub struct Heap {
    objects: Vec<HeapObject>,
    arrays: Vec<Vec<Value>>,
    array_props: Vec<HashMap<String, Value>>,
    maps: Vec<std::collections::HashMap<String, Value>>,
    sets: Vec<std::collections::HashSet<String>>,
    dates: Vec<f64>,
    symbols: Vec<Option<String>>,
    error_object_ids: HashSet<usize>,
    global_object_id: usize,
}

impl Default for Heap {
    fn default() -> Self {
        let mut heap = Self {
            objects: Vec::new(),
            arrays: Vec::new(),
            array_props: Vec::new(),
            maps: Vec::new(),
            sets: Vec::new(),
            dates: Vec::new(),
            symbols: Vec::new(),
            error_object_ids: HashSet::new(),
            global_object_id: 0,
        };
        heap.init_globals();
        heap
    }
}

impl Heap {
    pub fn new() -> Self {
        Self::default()
    }

    fn init_globals(&mut self) {
        let global_id = self.alloc_object();
        self.global_object_id = global_id;

        let obj_id = self.alloc_object();
        self.set_prop(obj_id, "create", Value::Builtin(0x40));
        self.set_prop(obj_id, "keys", Value::Builtin(0x41));
        self.set_prop(obj_id, "assign", Value::Builtin(0x42));
        self.set_prop(obj_id, "hasOwnProperty", Value::Builtin(0x43));
        self.set_prop(global_id, "Object", Value::Object(obj_id));

        let arr_id = self.alloc_object();
        self.set_prop(arr_id, "push", Value::Builtin(0x10));
        self.set_prop(arr_id, "pop", Value::Builtin(0x11));
        self.set_prop(arr_id, "isArray", Value::Builtin(0x12));
        self.set_prop(arr_id, "slice", Value::Builtin(0x13));
        self.set_prop(arr_id, "concat", Value::Builtin(0x14));
        self.set_prop(arr_id, "indexOf", Value::Builtin(0x15));
        self.set_prop(arr_id, "join", Value::Builtin(0x16));
        self.set_prop(arr_id, "shift", Value::Builtin(0x17));
        self.set_prop(arr_id, "unshift", Value::Builtin(0x18));
        self.set_prop(arr_id, "reverse", Value::Builtin(0x19));
        self.set_prop(arr_id, "includes", Value::Builtin(0x1A));
        self.set_prop(arr_id, "fill", Value::Builtin(0x1B));
        self.set_prop(global_id, "Array", Value::Object(arr_id));

        let math_id = self.alloc_object();
        self.set_prop(math_id, "floor", Value::Builtin(0x20));
        self.set_prop(math_id, "abs", Value::Builtin(0x21));
        self.set_prop(math_id, "min", Value::Builtin(0x22));
        self.set_prop(math_id, "max", Value::Builtin(0x23));
        self.set_prop(math_id, "pow", Value::Builtin(0x24));
        self.set_prop(math_id, "ceil", Value::Builtin(0x25));
        self.set_prop(math_id, "round", Value::Builtin(0x26));
        self.set_prop(math_id, "sqrt", Value::Builtin(0x27));
        self.set_prop(math_id, "random", Value::Builtin(0x28));
        self.set_prop(global_id, "Math", Value::Object(math_id));

        let json_id = self.alloc_object();
        self.set_prop(json_id, "parse", Value::Builtin(0x30));
        self.set_prop(json_id, "stringify", Value::Builtin(0x31));
        self.set_prop(global_id, "JSON", Value::Object(json_id));

        let str_id = self.alloc_object();
        self.set_prop(str_id, "split", Value::Builtin(0x60));
        self.set_prop(str_id, "trim", Value::Builtin(0x61));
        self.set_prop(str_id, "toLowerCase", Value::Builtin(0x62));
        self.set_prop(str_id, "toUpperCase", Value::Builtin(0x63));
        self.set_prop(str_id, "charAt", Value::Builtin(0x64));
        self.set_prop(str_id, "repeat", Value::Builtin(0x65));
        self.set_prop(global_id, "String", Value::Object(str_id));

        self.set_prop(global_id, "Number", Value::Builtin(0x52));
        self.set_prop(global_id, "Boolean", Value::Builtin(0x53));

        let err_id = self.alloc_object();
        self.set_prop(err_id, "isError", Value::Builtin(0x70));
        self.set_prop(err_id, "__call__", Value::Builtin(0x51));
        self.set_prop(global_id, "Error", Value::Object(err_id));

        let ref_err_id = self.alloc_object();
        self.set_prop(ref_err_id, "__call__", Value::Builtin(0x51));
        self.set_prop(global_id, "ReferenceError", Value::Object(ref_err_id));

        let type_err_id = self.alloc_object();
        self.set_prop(type_err_id, "__call__", Value::Builtin(0x51));
        self.set_prop(global_id, "TypeError", Value::Object(type_err_id));

        let range_err_id = self.alloc_object();
        self.set_prop(range_err_id, "__call__", Value::Builtin(0x51));
        self.set_prop(global_id, "RangeError", Value::Object(range_err_id));

        let syntax_err_id = self.alloc_object();
        self.set_prop(syntax_err_id, "__call__", Value::Builtin(0x51));
        self.set_prop(global_id, "SyntaxError", Value::Object(syntax_err_id));

        let regexp_id = self.alloc_object();
        self.set_prop(regexp_id, "escape", Value::Builtin(0x80));
        self.set_prop(global_id, "RegExp", Value::Object(regexp_id));

        let map_id = self.alloc_object();
        self.set_prop(map_id, "set", Value::Builtin(0x91));
        self.set_prop(map_id, "get", Value::Builtin(0x92));
        self.set_prop(map_id, "has", Value::Builtin(0x93));
        self.set_prop(global_id, "Map", Value::Object(map_id));

        let set_id = self.alloc_object();
        self.set_prop(set_id, "add", Value::Builtin(0xA1));
        self.set_prop(set_id, "has", Value::Builtin(0xA2));
        self.set_prop(set_id, "size", Value::Builtin(0xA3));
        self.set_prop(global_id, "Set", Value::Object(set_id));

        let date_id = self.alloc_object();
        self.set_prop(date_id, "now", Value::Builtin(0xC1));
        self.set_prop(date_id, "getTime", Value::Builtin(0xC2));
        self.set_prop(date_id, "toString", Value::Builtin(0xC3));
        self.set_prop(date_id, "toISOString", Value::Builtin(0xC4));
        self.set_prop(global_id, "Date", Value::Object(date_id));

        self.set_prop(global_id, "NaN", Value::Number(f64::NAN));
        self.set_prop(global_id, "Infinity", Value::Number(f64::INFINITY));
        self.set_prop(global_id, "globalThis", Value::Object(global_id));
        self.set_prop(global_id, "Symbol", Value::Builtin(0xD0));

        let console_id = self.alloc_object();
        self.set_prop(console_id, "log", Value::Builtin(0x00));
        self.set_prop(global_id, "console", Value::Object(console_id));

        self.set_prop(global_id, "print", Value::Builtin(0x00));
    }

    /// Add $262 host object for test262 harness. Match V8/Bun/Deno: $262 only exists when running via test262.
    pub fn init_test262_globals(&mut self) {
        let global_id = self.global_object_id;
        let dollar262_id = self.alloc_object();
        self.set_prop(dollar262_id, "global", Value::Object(global_id));
        self.set_prop(dollar262_id, "createRealm", Value::Builtin(0xD5));
        self.set_prop(dollar262_id, "evalScript", Value::Builtin(0xD6));
        self.set_prop(dollar262_id, "gc", Value::Builtin(0xD7));
        self.set_prop(dollar262_id, "detachArrayBuffer", Value::Builtin(0xD8));
        self.set_prop(global_id, "$262", Value::Object(dollar262_id));
    }

    pub fn get_global(&self, name: &str) -> Value {
        self.get_prop(self.global_object_id, name)
    }

    pub fn global_object(&self) -> usize {
        self.global_object_id
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

    pub fn get_proto(&self, obj_id: usize) -> Option<usize> {
        self.objects.get(obj_id).and_then(|o| o.prototype)
    }

    pub fn alloc_array(&mut self) -> usize {
        let id = self.arrays.len();
        self.arrays.push(Vec::new());
        self.array_props.push(HashMap::new());
        id
    }

    pub fn alloc_symbol(&mut self, description: Option<String>) -> usize {
        let id = self.symbols.len();
        self.symbols.push(description);
        id
    }

    pub fn symbol_description(&self, id: usize) -> Option<&str> {
        self.symbols.get(id).and_then(|o| o.as_deref())
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
