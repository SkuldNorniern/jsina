use crate::runtime::{Heap, Value};

pub(crate) struct GetPropCache {
    obj_id: usize,
    is_array: bool,
    key: String,
    value: Option<Value>,
}

impl GetPropCache {
    pub fn new() -> Self {
        Self {
            obj_id: usize::MAX,
            is_array: false,
            key: String::new(),
            value: None,
        }
    }

    #[inline(always)]
    pub fn get(&mut self, obj_id: usize, is_array: bool, key: &str, heap: &Heap) -> Value {
        if self.obj_id == obj_id && self.is_array == is_array && self.key == key {
            if let Some(ref v) = self.value {
                return v.clone();
            }
        }
        let result = if is_array {
            heap.get_array_prop(obj_id, key)
        } else {
            heap.get_prop(obj_id, key)
        };
        self.obj_id = obj_id;
        self.is_array = is_array;
        self.key = key.to_string();
        self.value = Some(result.clone());
        result
    }

    pub fn invalidate(&mut self, obj_id: usize, is_array: bool, key: &str) {
        if self.obj_id == obj_id && self.is_array == is_array && self.key == key {
            self.value = None;
        }
    }

    pub fn invalidate_all(&mut self) {
        self.value = None;
    }
}

/// Shared property resolution for both const-key (GetProp) and dynamic-key (GetPropDynamic).
/// When `cache` is `Some`, the result is cached (used for const-key access).
pub(crate) fn resolve_get_prop(
    obj: &Value,
    key: &str,
    cache: Option<&mut GetPropCache>,
    heap: &Heap,
) -> Value {
    match obj {
        Value::Object(id) => {
            if let Some(c) = cache {
                c.get(*id, false, key, heap)
            } else {
                heap.get_prop(*id, key)
            }
        }
        Value::Array(id) => {
            if let Some(c) = cache {
                c.get(*id, true, key, heap)
            } else {
                heap.get_array_prop(*id, key)
            }
        }
        Value::Map(id) if key == "size" => Value::Int(heap.map_size(*id) as i32),
        Value::Map(_) => primitive_map_method(key),
        Value::Set(id) if key == "size" => Value::Int(heap.set_size(*id) as i32),
        Value::Set(_) => primitive_set_method(key),
        Value::String(s) if key == "length" => Value::Int(s.len() as i32),
        Value::String(s) => {
            if let Ok(idx) = key.parse::<usize>() {
                s.chars()
                    .nth(idx)
                    .map(|c| Value::String(c.to_string()))
                    .unwrap_or(Value::Undefined)
            } else {
                primitive_string_method(key)
            }
        }
        Value::Date(_) => primitive_date_method(key),
        Value::Number(_) | Value::Int(_) => primitive_number_method(key),
        Value::Bool(_) => primitive_bool_method(key),
        Value::Function(i) => heap.get_function_prop(*i, key),
        _ => Value::Undefined,
    }
}

pub(crate) fn primitive_string_method(key: &str) -> Value {
    match key {
        "includes" => Value::Builtin(0x1A),
        "indexOf" => Value::Builtin(0x19),
        "split" => Value::Builtin(0x60),
        "trim" => Value::Builtin(0x61),
        "toLowerCase" => Value::Builtin(0x62),
        "toUpperCase" => Value::Builtin(0x63),
        "charAt" => Value::Builtin(0x64),
        "repeat" => Value::Builtin(0x65),
        "anchor" => Value::Builtin(0x67),
        "big" => Value::Builtin(0x68),
        "blink" => Value::Builtin(0x69),
        "bold" => Value::Builtin(0x6A),
        "fixed" => Value::Builtin(0x6B),
        "fontcolor" => Value::Builtin(0x6C),
        "fontsize" => Value::Builtin(0x6D),
        "italics" => Value::Builtin(0x6E),
        "link" => Value::Builtin(0x6F),
        "small" => Value::Builtin(0xB1),
        "strike" => Value::Builtin(0xB2),
        "sub" => Value::Builtin(0xB3),
        "sup" => Value::Builtin(0xB4),
        _ => Value::Undefined,
    }
}

pub(crate) fn primitive_date_method(key: &str) -> Value {
    match key {
        "getTime" => Value::Builtin(0xC2),
        "valueOf" => Value::Builtin(0xC2),
        "toString" => Value::Builtin(0xC3),
        "toISOString" => Value::Builtin(0xC4),
        "getYear" => Value::Builtin(0xC5),
        "setYear" => Value::Builtin(0xC6),
        "toGMTString" => Value::Builtin(0xC7),
        _ => Value::Undefined,
    }
}

pub(crate) fn primitive_number_method(key: &str) -> Value {
    match key {
        "toString" => Value::Builtin(0x55),
        "valueOf" => Value::Builtin(0x56),
        _ => Value::Undefined,
    }
}

pub(crate) fn primitive_bool_method(key: &str) -> Value {
    match key {
        "toString" => Value::Builtin(0x55),
        "valueOf" => Value::Builtin(0x56),
        _ => Value::Undefined,
    }
}

pub(crate) fn primitive_map_method(key: &str) -> Value {
    match key {
        "set" => Value::Builtin(0x91),
        "get" => Value::Builtin(0x92),
        "has" => Value::Builtin(0x93),
        _ => Value::Undefined,
    }
}

pub(crate) fn primitive_set_method(key: &str) -> Value {
    match key {
        "add" => Value::Builtin(0xA1),
        "has" => Value::Builtin(0xA2),
        _ => Value::Undefined,
    }
}
