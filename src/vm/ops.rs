use crate::runtime::builtins;
use crate::runtime::{Heap, Value};

#[inline]
pub(crate) fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Undefined | Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(n) => *n != 0,
        Value::Number(n) => *n != 0.0 && !n.is_nan(),
        _ => true,
    }
}

#[inline]
pub(crate) fn is_nullish(v: &Value) -> bool {
    matches!(v, Value::Undefined | Value::Null)
}

#[inline]
pub(crate) fn strict_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Undefined, Value::Undefined) | (Value::Null, Value::Null) => true,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Int(x), Value::Number(y)) => !y.is_nan() && (*x as f64) == *y,
        (Value::Number(x), Value::Int(y)) => !x.is_nan() && *x == (*y as f64),
        (Value::Number(x), Value::Number(y)) => !x.is_nan() && !y.is_nan() && x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Object(x), Value::Object(y)) => x == y,
        (Value::Array(x), Value::Array(y)) => x == y,
        (Value::Map(x), Value::Map(y)) => x == y,
        (Value::Set(x), Value::Set(y)) => x == y,
        (Value::Date(x), Value::Date(y)) => x == y,
        (Value::Function(x), Value::Function(y)) => x == y,
        (Value::Builtin(x), Value::Builtin(y)) => x == y,
        _ => false,
    }
}

pub(crate) fn value_to_prop_key(v: &Value) -> String {
    match v {
        Value::String(s) => s.clone(),
        Value::Int(n) => n.to_string(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Null => "null".to_string(),
        Value::Undefined => "undefined".to_string(),
        Value::Symbol(_) => "Symbol()".to_string(),
        Value::Object(_) | Value::Array(_) | Value::Map(_) | Value::Set(_) | Value::Date(_) => {
            "[object Object]".to_string()
        }
        Value::Function(_) | Value::DynamicFunction(_) | Value::Builtin(_) => {
            "function".to_string()
        }
    }
}

#[inline]
pub(crate) fn add_values(a: &Value, b: &Value) -> Value {
    match (a, b) {
        (Value::String(x), Value::String(y)) => Value::String(format!("{}{}", x, y)),
        (Value::String(x), y) => Value::String(format!("{}{}", x, y)),
        (x, Value::String(y)) => Value::String(format!("{}{}", x, y)),
        (Value::Int(x), Value::Int(y)) => Value::Int(x.saturating_add(*y)),
        (Value::Number(x), Value::Number(y)) => Value::Number(x + y),
        (Value::Int(x), Value::Number(y)) => Value::Number(*x as f64 + y),
        (Value::Number(x), Value::Int(y)) => Value::Number(x + *y as f64),
        _ => Value::Number(builtins::to_number(a) + builtins::to_number(b)),
    }
}

#[inline]
pub(crate) fn sub_values(a: &Value, b: &Value) -> Value {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Int(x.saturating_sub(*y)),
        (Value::Number(x), Value::Number(y)) => Value::Number(x - y),
        (Value::Int(x), Value::Number(y)) => Value::Number(*x as f64 - y),
        (Value::Number(x), Value::Int(y)) => Value::Number(x - *y as f64),
        _ => Value::Number(builtins::to_number(a) - builtins::to_number(b)),
    }
}

#[inline]
pub(crate) fn mul_values(a: &Value, b: &Value) -> Value {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Int(x.saturating_mul(*y)),
        (Value::Number(x), Value::Number(y)) => Value::Number(x * y),
        (Value::Int(x), Value::Number(y)) => Value::Number(*x as f64 * y),
        (Value::Number(x), Value::Int(y)) => Value::Number(x * *y as f64),
        _ => Value::Number(builtins::to_number(a) * builtins::to_number(b)),
    }
}

pub(crate) fn div_values(a: &Value, b: &Value) -> Value {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => {
            if *y == 0 {
                if *x == 0 {
                    Value::Number(f64::NAN)
                } else if *x > 0 {
                    Value::Number(f64::INFINITY)
                } else {
                    Value::Number(f64::NEG_INFINITY)
                }
            } else {
                Value::Number(*x as f64 / *y as f64)
            }
        }
        (Value::Number(x), Value::Number(y)) => Value::Number(x / y),
        (Value::Int(x), Value::Number(y)) => Value::Number(*x as f64 / y),
        (Value::Number(x), Value::Int(y)) => Value::Number(x / *y as f64),
        _ => Value::Number(builtins::to_number(a) / builtins::to_number(b)),
    }
}

pub(crate) fn mod_values(a: &Value, b: &Value) -> Value {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => {
            if *y == 0 {
                Value::Number(f64::NAN)
            } else {
                Value::Int(x.wrapping_rem(*y))
            }
        }
        (Value::Number(x), Value::Number(y)) => Value::Number(x % y),
        (Value::Int(x), Value::Number(y)) => Value::Number(*x as f64 % y),
        (Value::Number(x), Value::Int(y)) => Value::Number(x % *y as f64),
        _ => Value::Number(builtins::to_number(a) % builtins::to_number(b)),
    }
}

pub(crate) fn pow_values(a: &Value, b: &Value) -> Value {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) if *y >= 0 && *y <= 31 => {
            Value::Int(x.saturating_pow(*y as u32))
        }
        (Value::Number(x), Value::Number(y)) => Value::Number(x.powf(*y)),
        (Value::Int(x), Value::Number(y)) => Value::Number((*x as f64).powf(*y)),
        (Value::Number(x), Value::Int(y)) => Value::Number(x.powi(*y)),
        _ => Value::Number(builtins::to_number(a).powf(builtins::to_number(b))),
    }
}

#[inline]
pub(crate) fn lt_values(a: &Value, b: &Value) -> Value {
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => x < y,
        (Value::Number(x), Value::Number(y)) => x < y,
        (Value::Int(x), Value::Number(y)) => (*x as f64) < *y,
        (Value::Number(x), Value::Int(y)) => *x < (*y as f64),
        (Value::String(x), Value::String(y)) => x < y,
        _ => builtins::to_number(a) < builtins::to_number(b),
    };
    Value::Bool(result)
}

#[inline]
pub(crate) fn lte_values(a: &Value, b: &Value) -> Value {
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => x <= y,
        (Value::Number(x), Value::Number(y)) => x <= y,
        (Value::Int(x), Value::Number(y)) => (*x as f64) <= *y,
        (Value::Number(x), Value::Int(y)) => *x <= (*y as f64),
        (Value::String(x), Value::String(y)) => x <= y,
        _ => builtins::to_number(a) <= builtins::to_number(b),
    };
    Value::Bool(result)
}

#[inline]
pub(crate) fn gt_values(a: &Value, b: &Value) -> Value {
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => x > y,
        (Value::Number(x), Value::Number(y)) => x > y,
        (Value::Int(x), Value::Number(y)) => (*x as f64) > *y,
        (Value::Number(x), Value::Int(y)) => *x > (*y as f64),
        (Value::String(x), Value::String(y)) => x > y,
        _ => builtins::to_number(a) > builtins::to_number(b),
    };
    Value::Bool(result)
}

#[inline]
pub(crate) fn gte_values(a: &Value, b: &Value) -> Value {
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => x >= y,
        (Value::Number(x), Value::Number(y)) => x >= y,
        (Value::Int(x), Value::Number(y)) => (*x as f64) >= *y,
        (Value::Number(x), Value::Int(y)) => *x >= (*y as f64),
        (Value::String(x), Value::String(y)) => x >= y,
        _ => builtins::to_number(a) >= builtins::to_number(b),
    };
    Value::Bool(result)
}

pub(crate) fn instanceof_check(value: &Value, constructor: &Value, heap: &Heap) -> bool {
    let constructor_name = get_constructor_name(constructor, heap);
    match (value, constructor_name.as_deref()) {
        (Value::Array(_), Some("Array")) => true,
        (Value::Map(_), Some("Map")) => true,
        (Value::Set(_), Some("Set")) => true,
        (Value::Date(_), Some("Date")) => true,
        (Value::Object(id), Some("Error")) => heap.is_error_object(*id),
        (Value::Object(id), Some("ReferenceError")) => {
            heap.is_error_object(*id)
                && matches!(heap.get_prop(*id, "name"), Value::String(s) if s == "ReferenceError")
        }
        (Value::Object(id), Some("TypeError")) => {
            heap.is_error_object(*id)
                && matches!(heap.get_prop(*id, "name"), Value::String(s) if s == "TypeError")
        }
        (Value::Object(id), Some("RangeError")) => {
            heap.is_error_object(*id)
                && matches!(heap.get_prop(*id, "name"), Value::String(s) if s == "RangeError")
        }
        (Value::Object(id), Some("SyntaxError")) => {
            heap.is_error_object(*id)
                && matches!(heap.get_prop(*id, "name"), Value::String(s) if s == "SyntaxError")
        }
        (Value::Object(id), Some("URIError")) => {
            heap.is_error_object(*id)
                && matches!(heap.get_prop(*id, "name"), Value::String(s) if s == "URIError")
        }
        (Value::Object(id), Some("Object")) => !heap.is_error_object(*id),
        (Value::Object(id), _) => {
            let constructor_proto = match constructor {
                Value::Object(cid) => heap.get_prop(*cid, "prototype"),
                _ => return false,
            };
            let proto_id = match &constructor_proto {
                Value::Object(pid) => *pid,
                _ => return false,
            };
            let mut current = Some(*id);
            let mut depth = 0;
            while let Some(obj_id) = current {
                if depth > 100 {
                    break;
                }
                depth += 1;
                match heap.get_proto(obj_id) {
                    Some(pid) if pid == proto_id => return true,
                    Some(pid) => current = Some(pid),
                    None => break,
                }
            }
            false
        }
        _ => false,
    }
}

fn get_constructor_name(constructor: &Value, heap: &Heap) -> Option<String> {
    match constructor {
        Value::Object(id) => {
            if let Value::String(name) = heap.get_prop(*id, "name") {
                return Some(name);
            }
            let global = heap.global_object();
            for name in [
                "Array",
                "Object",
                "Error",
                "ReferenceError",
                "TypeError",
                "RangeError",
                "SyntaxError",
                "URIError",
                "Map",
                "Set",
                "Date",
            ] {
                if let Value::Object(gid) = heap.get_prop(global, name) {
                    if gid == *id {
                        return Some(name.to_string());
                    }
                }
            }
            None
        }
        _ => None,
    }
}
