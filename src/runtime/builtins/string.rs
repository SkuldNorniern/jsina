use super::to_number;
use crate::runtime::{Heap, Value};

pub fn string(args: &[Value]) -> Value {
    let arg = args.first().map(|v| v.to_string()).unwrap_or_default();
    Value::String(arg)
}

pub fn trim(args: &[Value]) -> Value {
    let s = match args.first() {
        Some(Value::String(x)) => x.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    Value::String(s.trim().to_string())
}

pub fn to_lower_case(args: &[Value]) -> Value {
    let s = match args.first() {
        Some(Value::String(x)) => x.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    Value::String(s.to_lowercase())
}

pub fn to_upper_case(args: &[Value]) -> Value {
    let s = match args.first() {
        Some(Value::String(x)) => x.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    Value::String(s.to_uppercase())
}

pub fn char_at(args: &[Value]) -> Value {
    let s = match args.first() {
        Some(Value::String(x)) => x.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    let idx = args.get(1).map(to_number).unwrap_or(0.0);
    let i = if idx.is_nan() || idx.is_infinite() {
        0
    } else {
        idx as i32
    };
    let chars: Vec<char> = s.chars().collect();
    let len = chars.len() as i32;
    let pos = if i < 0 { (len + i).max(0) } else { i.min(len) };
    let ch = chars.get(pos as usize).map(|c| c.to_string()).unwrap_or_default();
    Value::String(ch)
}

pub fn split(args: &[Value], heap: &mut Heap) -> Value {
    let receiver = match args.first() {
        Some(v) => v,
        None => return Value::Undefined,
    };
    let sep_val = args.get(1);
    let s = match receiver {
        Value::String(x) => x.clone(),
        _ => receiver.to_string(),
    };
    let parts: Vec<Value> = match sep_val {
        None | Some(Value::Undefined) => vec![Value::String(s.clone())],
        Some(v) => {
            let sep = v.to_string();
            if sep.is_empty() {
                s.chars().map(|c| Value::String(c.to_string())).collect()
            } else {
                s.split(&sep).map(|p| Value::String(p.to_string())).collect()
            }
        }
    };
    let new_id = heap.alloc_array();
    for p in parts {
        heap.array_push(new_id, p);
    }
    Value::Array(new_id)
}
