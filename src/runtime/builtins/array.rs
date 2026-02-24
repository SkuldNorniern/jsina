use super::{strict_eq, to_number};
use crate::runtime::{Heap, Value};

pub fn push(args: &[Value], heap: &mut Heap) -> Value {
    let (arr, vals) = match args.split_first() {
        Some(p) => p,
        None => return Value::Undefined,
    };
    let arr_id = match arr {
        Value::Array(id) => *id,
        _ => return Value::Undefined,
    };
    let new_len = heap.array_push_values(arr_id, vals);
    Value::Int(new_len)
}

pub fn pop(args: &[Value], heap: &mut Heap) -> Value {
    let arr = match args.first() {
        Some(v) => v,
        None => return Value::Undefined,
    };
    match arr {
        Value::Array(id) => heap.array_pop(*id),
        _ => Value::Undefined,
    }
}

pub fn shift(args: &[Value], heap: &mut Heap) -> Value {
    match args.first() {
        Some(Value::Array(id)) => heap.array_shift(*id),
        _ => Value::Undefined,
    }
}

pub fn unshift(args: &[Value], heap: &mut Heap) -> Value {
    let new_len = match args.first() {
        Some(Value::Array(id)) => heap.array_unshift(*id, &args[1..]),
        _ => 0,
    };
    Value::Int(new_len)
}

pub fn is_array(args: &[Value]) -> Value {
    Value::Bool(matches!(args.first(), Some(Value::Array(_))))
}

pub fn reverse(args: &[Value], heap: &mut Heap) -> Value {
    if let Some(Value::Array(id)) = args.first() {
        heap.array_reverse(*id);
        Value::Array(*id)
    } else {
        Value::Undefined
    }
}

pub fn slice(args: &[Value], heap: &mut Heap) -> Value {
    let receiver = match args.first() {
        Some(v) => v,
        None => return Value::Undefined,
    };
    let start_val = args.get(1);
    let end_val = args.get(2);
    if let Value::String(s) = receiver {
        let len = s.len() as i32;
        let start = start_val
            .map(|v| {
                let n = to_number(v) as i32;
                if n < 0 { (len + n).max(0) } else { n.min(len) }
            })
            .unwrap_or(0) as usize;
        let end = end_val
            .map(|v| {
                let n = to_number(v);
                if n.is_nan() || n.is_infinite() {
                    len
                } else {
                    let n = n as i32;
                    if n < 0 { (len + n).max(0) } else { n.min(len) }
                }
            })
            .unwrap_or(len) as usize;
        let end = end.max(start);
        Value::String(s[start..end].to_string())
    } else if let Value::Array(id) = receiver {
        let elements: Vec<Value> = heap.array_elements(*id).map(|s| s.to_vec()).unwrap_or_default();
        let len = elements.len() as i32;
        let start = start_val.map(|v| to_number(v) as i32).unwrap_or(0);
        let end = end_val
            .map(|v| {
                let n = to_number(v);
                if n.is_nan() || n.is_infinite() { len } else { n as i32 }
            })
            .unwrap_or(len);
        let start = start.max(0).min(len) as usize;
        let end = end.max(0).min(len as i32) as usize;
        let end = end.max(start);
        let new_id = heap.alloc_array();
        for i in start..end {
            if let Some(v) = elements.get(i) {
                heap.array_push(new_id, v.clone());
            }
        }
        Value::Array(new_id)
    } else {
        Value::Undefined
    }
}

pub fn concat(args: &[Value], heap: &mut Heap) -> Value {
    let receiver = match args.first() {
        Some(v) => v,
        None => return Value::Undefined,
    };
    if let Value::String(s) = receiver {
        let mut out = s.clone();
        for v in args.iter().skip(1) {
            out.push_str(&v.to_string());
        }
        Value::String(out)
    } else if let Value::Array(arr_id) = receiver {
        let mut to_push: Vec<Value> = heap.array_elements(*arr_id).map(|s| s.to_vec()).unwrap_or_default();
        for v in args.iter().skip(1) {
            if let Value::Array(id) = v {
                if let Some(elems) = heap.array_elements(*id) {
                    to_push.extend(elems.iter().cloned());
                }
            } else {
                to_push.push(v.clone());
            }
        }
        let new_id = heap.alloc_array();
        for v in to_push {
            heap.array_push(new_id, v);
        }
        Value::Array(new_id)
    } else {
        Value::Undefined
    }
}

pub fn index_of(args: &[Value], heap: &Heap) -> Value {
    let receiver = match args.first() {
        Some(v) => v,
        None => return Value::Int(-1),
    };
    let search = args.get(1).cloned().unwrap_or(Value::Undefined);
    let from_val = args.get(2);
    let idx = if let Value::String(s) = receiver {
        let search_str = search.to_string();
        let from = from_val
            .map(|v| {
                let n = to_number(&v) as i32;
                if n < 0 { ((s.len() as i32) + n).max(0) as usize }
                else { n.min(s.len() as i32) as usize }
            })
            .unwrap_or(0);
        s[from..].find(&search_str).map(|i| (from + i) as i32).unwrap_or(-1)
    } else if let Value::Array(id) = receiver {
        let elements: Vec<Value> = heap.array_elements(*id).map(|s| s.to_vec()).unwrap_or_default();
        let from = from_val
            .map(|v| {
                let n = to_number(v) as i32;
                if n < 0 { ((elements.len() as i32) + n).max(0) as usize }
                else { n.min(elements.len() as i32) as usize }
            })
            .unwrap_or(0);
        let mut found = -1i32;
        for (i, v) in elements.iter().skip(from).enumerate() {
            if strict_eq(v, &search) {
                found = (from + i) as i32;
                break;
            }
        }
        found
    } else {
        -1
    };
    Value::Int(idx)
}

pub fn join(args: &[Value], heap: &Heap) -> Value {
    let arr = match args.first() {
        Some(v) => v,
        None => return Value::Undefined,
    };
    let sep = args.get(1).map(|v| v.to_string()).unwrap_or_else(|| ",".to_string());
    let elements: Vec<Value> = match arr {
        Value::Array(id) => heap.array_elements(*id).map(|s| s.to_vec()).unwrap_or_default(),
        _ => return Value::Undefined,
    };
    let parts: Vec<String> = elements.iter().map(|v| v.to_string()).collect();
    Value::String(parts.join(&sep))
}
