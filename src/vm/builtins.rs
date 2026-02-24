//! Builtin function dispatch.
//!
//! IDs 0-32 map to BuiltinId in src/ir/hir.rs:
//! 0=Print, 1-2=Array(push,pop), 3-6=Math(floor,abs,min,max), 7-8=JSON, 9=ObjectCreate,
//! 10=ArrayIsArray, 11=ObjectKeys, 12=String, 13=Error, 14=Number, 15=Boolean,
//! 16=slice, 17=concat, 18=ObjectAssign, 19=indexOf, 20=join, 21=MathPow,
//! 22-23=Array(shift,unshift), 24=split, 25-27=String(trim,toLowerCase,toUpperCase),
//! 28-31=Math(ceil,round,sqrt,random), 32=ObjectHasOwnProperty.

use crate::runtime::{Heap, Value};
use std::io::Write;
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug)]
pub enum BuiltinError {
    Throw(Value),
}

static RNG_STATE: AtomicU64 = AtomicU64::new(0);

fn random_f64() -> f64 {
    let mut state = RNG_STATE.load(Ordering::Relaxed);
    if state == 0 {
        state = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos() as u64)
            .unwrap_or(1);
        if state == 0 {
            state = 1;
        }
    }
    state ^= state << 13;
    state ^= state >> 7;
    state ^= state << 17;
    RNG_STATE.store(state, Ordering::Relaxed);
    (state as f64) / (u64::MAX as f64)
}

fn to_number(v: &Value) -> f64 {
    match v {
        Value::Int(n) => *n as f64,
        Value::Number(n) => *n,
        Value::Bool(b) => if *b { 1.0 } else { 0.0 },
        Value::Null => 0.0,
        Value::Undefined => f64::NAN,
        Value::String(s) => s.parse().unwrap_or_else(|_| f64::NAN),
        Value::Object(_) | Value::Array(_) | Value::Function(_) => f64::NAN,
    }
}

fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Undefined | Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(n) => *n != 0,
        Value::Number(n) => *n != 0.0 && !n.is_nan(),
        Value::String(_) | Value::Object(_) | Value::Array(_) | Value::Function(_) => true,
    }
}

fn to_prop_key(v: &Value) -> String {
    match v {
        Value::String(s) => s.clone(),
        Value::Int(n) => n.to_string(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Null => "null".to_string(),
        Value::Undefined => "undefined".to_string(),
        Value::Object(_) | Value::Array(_) => "[object Object]".to_string(),
        Value::Function(_) => "function".to_string(),
    }
}

fn strict_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Undefined, Value::Undefined) => true,
        (Value::Null, Value::Null) => true,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Number(x), Value::Number(y)) => !x.is_nan() && !y.is_nan() && x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Object(x), Value::Object(y)) => x == y,
        (Value::Array(x), Value::Array(y)) => x == y,
        (Value::Function(x), Value::Function(y)) => x == y,
        _ => false,
    }
}

fn number_to_value(n: f64) -> Value {
    if n.is_finite() && n.fract() == 0.0 && n >= i32::MIN as f64 && n <= i32::MAX as f64 {
        Value::Int(n as i32)
    } else {
        Value::Number(n)
    }
}

pub fn dispatch(id: u8, args: &[Value], heap: &mut Heap) -> Result<Value, BuiltinError> {
    let result = match id {
        // --- Host ---
        0 => {
            let mut out = std::io::stdout();
            for (i, v) in args.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " ");
                }
                let _ = write!(out, "{}", v);
            }
            let _ = writeln!(out);
            let _ = out.flush();
            Value::Undefined
        }
        // --- Array ---
        1 => {
            let (arr, vals) = match args.split_first() {
                Some(p) => p,
                None => return Ok(Value::Undefined),
            };
            let arr_id = match arr {
                Value::Array(id) => *id,
                _ => return Ok(Value::Undefined),
            };
            let new_len = heap.array_push_values(arr_id, vals);
            Value::Int(new_len)
        }
        2 => {
            let arr = match args.first() {
                Some(v) => v,
                None => return Ok(Value::Undefined),
            };
            let val = match arr {
                Value::Array(id) => heap.array_pop(*id),
                _ => Value::Undefined,
            };
            val
        }
        // --- Math (floor, abs, min, max) ---
        3 => {
            let n = args.first().map(|x| match x {
                Value::Int(i) => *i as f64,
                Value::Number(n) => *n,
                _ => f64::NAN,
            }).unwrap_or(f64::NAN);
            number_to_value(n.floor())
        }
        4 => {
            let result = args.first().map(|x| match x {
                Value::Int(i) => Value::Int(if *i < 0 { -(*i) } else { *i }),
                Value::Number(n) => Value::Number(n.abs()),
                _ => Value::Number(f64::NAN),
            }).unwrap_or(Value::Number(f64::NAN));
            result
        }
        5 => {
            let nums: Vec<f64> = args.iter().map(to_number).collect();
            let m = if nums.is_empty() {
                f64::INFINITY
            } else {
                nums.iter().fold(f64::INFINITY, |a, &b| a.min(b))
            };
            number_to_value(m)
        }
        6 => {
            let nums: Vec<f64> = args.iter().map(to_number).collect();
            let m = if nums.is_empty() {
                f64::NEG_INFINITY
            } else {
                nums.iter().fold(f64::NEG_INFINITY, |a, &b| a.max(b))
            };
            number_to_value(m)
        }
        // --- JSON ---
        7 => {
            let s = match args.first() {
                Some(Value::String(s)) => s.clone(),
                _ => return Err(BuiltinError::Throw(Value::String("JSON.parse requires a string".to_string()))),
            };
            match crate::runtime::json_parse(&s, heap) {
                Ok(v) => v,
                Err(e) => return Err(BuiltinError::Throw(Value::String(e.message))),
            }
        }
        8 => {
            let arg = args.first().unwrap_or(&Value::Undefined);
            match crate::runtime::json_stringify(arg, heap) {
                Some(s) => Value::String(s),
                None => Value::Undefined,
            }
        }
        // --- Object ---
        9 => {
            let prototype = args.first().and_then(|p| match p {
                Value::Null | Value::Undefined => None,
                Value::Object(id) => Some(*id),
                _ => None,
            });
            let id = heap.alloc_object_with_prototype(prototype);
            Value::Object(id)
        }
        // --- Array (isArray, keys) ---
        10 => {
            Value::Bool(matches!(args.first(), Some(Value::Array(_))))
        }
        11 => {
            let arr_id = heap.alloc_array();
            if let Some(Value::Object(obj_id)) = args.first() {
                for key in heap.object_keys(*obj_id) {
                    heap.array_push(arr_id, Value::String(key));
                }
            }
            Value::Array(arr_id)
        }
        // --- String, Error, Number, Boolean ---
        12 => {
            let arg = args.first().map(|v| v.to_string()).unwrap_or_default();
            Value::String(arg)
        }
        13 => {
            let msg = args.first().map(|v| v.to_string()).unwrap_or_default();
            let obj_id = heap.alloc_object();
            heap.set_prop(obj_id, "message", Value::String(msg));
            Value::Object(obj_id)
        }
        14 => {
            let n = args.first().map(to_number).unwrap_or(f64::NAN);
            number_to_value(n)
        }
        15 => {
            let b = args.first().map(is_truthy).unwrap_or(false);
            Value::Bool(b)
        }
        // --- Array/String (slice, concat, indexOf, join) ---
        16 => builtin_slice(args, heap),
        17 => builtin_concat(args, heap),
        18 => builtin_object_assign(args, heap),
        19 => builtin_index_of(args, heap),
        20 => builtin_join(args, heap),
        // --- Math (pow) ---
        21 => {
            let base = args.get(0).map(to_number).unwrap_or(f64::NAN);
            let exp = args.get(1).map(to_number).unwrap_or(f64::NAN);
            number_to_value(base.powf(exp))
        }
        22 => {
            let val = match args.first() {
                Some(Value::Array(id)) => heap.array_shift(*id),
                _ => Value::Undefined,
            };
            val
        }
        23 => {
            let new_len = match args.first() {
                Some(Value::Array(id)) => heap.array_unshift(*id, &args[1..]),
                _ => 0,
            };
            Value::Int(new_len)
        }
        // --- String (split, trim, toLowerCase, toUpperCase) ---
        24 => builtin_split(args, heap),
        25 => {
            let s = match args.first() {
                Some(Value::String(x)) => x.clone(),
                Some(v) => v.to_string(),
                None => String::new(),
            };
            Value::String(s.trim().to_string())
        }
        26 => {
            let s = match args.first() {
                Some(Value::String(x)) => x.clone(),
                Some(v) => v.to_string(),
                None => String::new(),
            };
            Value::String(s.to_lowercase())
        }
        27 => {
            let s = match args.first() {
                Some(Value::String(x)) => x.clone(),
                Some(v) => v.to_string(),
                None => String::new(),
            };
            Value::String(s.to_uppercase())
        }
        // --- Math (ceil, round, sqrt, random) ---
        28 => {
            let n = args.first().map(to_number).unwrap_or(f64::NAN);
            number_to_value(n.ceil())
        }
        29 => {
            let n = args.first().map(to_number).unwrap_or(f64::NAN);
            number_to_value(n.round())
        }
        30 => {
            let n = args.first().map(to_number).unwrap_or(f64::NAN);
            number_to_value(n.sqrt())
        }
        31 =>             Value::Number(random_f64()),
        // --- Object (hasOwnProperty) ---
        32 => {
            let key = args.get(1).map(to_prop_key).unwrap_or_default();
            let result = match args.first() {
                Some(Value::Object(id)) => heap.object_has_own_property(*id, &key),
                _ => false,
            };
            Value::Bool(result)
        }
        _ => unreachable!("invalid builtin id checked by caller"),
    };
    Ok(result)
}

fn builtin_slice(args: &[Value], heap: &mut Heap) -> Value {
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

fn builtin_concat(args: &[Value], heap: &mut Heap) -> Value {
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

fn builtin_object_assign(args: &[Value], heap: &mut Heap) -> Value {
    let target = match args.first() {
        Some(v) => v,
        None => return Value::Undefined,
    };
    let target_id = match target {
        Value::Object(id) => *id,
        _ => return target.clone(),
    };
    for source in args.iter().skip(1) {
        if let Value::Object(src_id) = source {
            for key in heap.object_keys(*src_id) {
                let val = heap.get_prop(*src_id, &key);
                heap.set_prop(target_id, &key, val);
            }
        }
    }
    Value::Object(target_id)
}

fn builtin_index_of(args: &[Value], heap: &mut Heap) -> Value {
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

fn builtin_join(args: &[Value], heap: &mut Heap) -> Value {
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

fn builtin_split(args: &[Value], heap: &mut Heap) -> Value {
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
