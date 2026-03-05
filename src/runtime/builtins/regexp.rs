use crate::runtime::builtins;
use crate::runtime::builtins::regex_engine;
use crate::runtime::{Heap, Value};

fn reg_exp_test_id() -> u8 {
    builtins::resolve("RegExp", "test").expect("RegExp.test builtin")
}

pub fn create(args: &[Value], heap: &mut Heap) -> Value {
    let pattern = match args.get(0) {
        Some(Value::String(s)) => s.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    let flags = match args.get(1) {
        Some(Value::String(s)) => s.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    let obj_id = heap.alloc_regexp();
    heap.set_prop(obj_id, "source", Value::String(pattern.clone()));
    heap.set_prop(obj_id, "flags", Value::String(flags.clone()));
    heap.set_prop(obj_id, "test", Value::Builtin(reg_exp_test_id()));
    heap.set_prop(obj_id, "__regexp_pattern", Value::String(pattern));
    heap.set_prop(obj_id, "__regexp_flags", Value::String(flags));
    Value::Object(obj_id)
}

pub fn exec(args: &[Value], heap: &mut Heap) -> Value {
    let obj_id = match args.first().and_then(|v| v.as_object_id()) {
        Some(id) => id,
        None => return Value::Null,
    };
    let pattern = match heap.get_prop(obj_id, "__regexp_pattern") {
        Value::String(s) => s.clone(),
        _ => return Value::Null,
    };
    let s = match args.get(1) {
        Some(Value::String(x)) => x.clone(),
        Some(v) => v.to_string(),
        None => return Value::Null,
    };
    let flags = match heap.get_prop(obj_id, "__regexp_flags") {
        Value::String(f) => f.clone(),
        _ => String::new(),
    };
    let (start, full_match_str) = match regex_engine::regex_find(pattern.as_str(), &flags, &s) {
        Some((i, m)) => (i, m.to_string()),
        None => return Value::Null,
    };
    let arr_id = heap.alloc_array();
    heap.array_push(arr_id, Value::String(full_match_str));
    heap.set_array_prop(arr_id, "index", Value::Int(start as i32));
    heap.set_array_prop(arr_id, "input", Value::String(s));
    Value::Array(arr_id)
}

pub fn test(args: &[Value], heap: &mut Heap) -> Value {
    let obj_id = match args.first().and_then(|v| v.as_object_id()) {
        Some(id) => id,
        None => return Value::Bool(false),
    };
    let pattern = match heap.get_prop(obj_id, "__regexp_pattern") {
        Value::String(s) => s.clone(),
        _ => return Value::Bool(false),
    };
    let s = match args.get(1) {
        Some(Value::String(x)) => x.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    let flags = match heap.get_prop(obj_id, "__regexp_flags") {
        Value::String(f) => f.clone(),
        _ => String::new(),
    };
    let found = regex_engine::regex_is_match(pattern.as_str(), &flags, &s);
    Value::Bool(found)
}

fn escape_char(c: char) -> Option<&'static str> {
    match c {
        '\\' => Some("\\\\"),
        '^' => Some("\\^"),
        '$' => Some("\\$"),
        '.' => Some("\\."),
        '*' => Some("\\*"),
        '+' => Some("\\+"),
        '?' => Some("\\?"),
        '(' => Some("\\("),
        ')' => Some("\\)"),
        '[' => Some("\\["),
        ']' => Some("\\]"),
        '{' => Some("\\{"),
        '}' => Some("\\}"),
        '|' => Some("\\|"),
        '/' => Some("\\/"),
        _ => None,
    }
}

pub fn compile(args: &[Value], heap: &mut Heap) -> Value {
    let obj_id = match args.first().and_then(|v| v.as_object_id()) {
        Some(id) => id,
        None => return Value::Undefined,
    };
    let (pattern, flags) = match args.get(1) {
        Some(Value::Object(pid)) => {
            let p = heap.get_prop(*pid, "__regexp_pattern");
            let f = heap.get_prop(*pid, "__regexp_flags");
            match (p, f) {
                (Value::String(ps), Value::String(fs)) => (ps.clone(), fs.clone()),
                (Value::String(ps), _) => (ps.clone(), String::new()),
                _ => (String::new(), String::new()),
            }
        }
        Some(Value::String(s)) => (
            s.clone(),
            args.get(2).map(|v| v.to_string()).unwrap_or_default(),
        ),
        Some(v) => (
            v.to_string(),
            args.get(2).map(|v| v.to_string()).unwrap_or_default(),
        ),
        None => {
            let existing = heap.get_prop(obj_id, "__regexp_pattern");
            let pattern = match existing {
                Value::String(s) => s,
                _ => String::new(),
            };
            let flags = match heap.get_prop(obj_id, "__regexp_flags") {
                Value::String(s) => s,
                _ => String::new(),
            };
            (pattern, flags)
        }
    };
    heap.set_prop(obj_id, "source", Value::String(pattern.clone()));
    heap.set_prop(obj_id, "flags", Value::String(flags.clone()));
    heap.set_prop(obj_id, "__regexp_pattern", Value::String(pattern));
    heap.set_prop(obj_id, "__regexp_flags", Value::String(flags));
    heap.set_prop(obj_id, "test", Value::Builtin(reg_exp_test_id()));
    Value::Object(obj_id)
}

pub fn symbol_match(args: &[Value], heap: &mut Heap) -> Value {
    exec(args, heap)
}

pub fn symbol_search(args: &[Value], heap: &mut Heap) -> Value {
    let result = exec(args, heap);
    match result {
        Value::Null => Value::Int(-1),
        Value::Array(id) => {
            let idx = heap.get_array_prop(id, "index");
            match idx {
                Value::Int(n) => Value::Int(n),
                _ => Value::Int(-1),
            }
        }
        _ => Value::Int(-1),
    }
}

pub fn symbol_replace(args: &[Value], heap: &mut Heap) -> Value {
    let obj_id = match args.first().and_then(|v| v.as_object_id()) {
        Some(id) => id,
        None => return Value::Undefined,
    };
    let pattern = match heap.get_prop(obj_id, "__regexp_pattern") {
        Value::String(s) => s.clone(),
        _ => return args.get(1).cloned().unwrap_or(Value::Undefined),
    };
    let flags = match heap.get_prop(obj_id, "__regexp_flags") {
        Value::String(f) => f.clone(),
        _ => String::new(),
    };
    let s = match args.get(1) {
        Some(Value::String(x)) => x.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    let repl = match args.get(2) {
        Some(Value::String(x)) => x.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    let result = if flags.contains('g') {
        regex_engine::regex_replace_all(pattern.as_str(), &flags, &s, &repl)
    } else {
        regex_engine::regex_replace_first(pattern.as_str(), &flags, &s, &repl)
    };
    Value::String(result)
}

pub fn symbol_split(args: &[Value], heap: &mut Heap) -> Value {
    let obj_id = match args.first().and_then(|v| v.as_object_id()) {
        Some(id) => id,
        None => return Value::Undefined,
    };
    let pattern = match heap.get_prop(obj_id, "__regexp_pattern") {
        Value::String(s) => s.clone(),
        _ => return Value::Undefined,
    };
    let s = match args.get(1) {
        Some(Value::String(x)) => x.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    let limit = args
        .get(2)
        .map(|v| super::to_number(v) as i32)
        .unwrap_or(i32::MAX);
    let flags = match heap.get_prop(obj_id, "__regexp_flags") {
        Value::String(f) => f.clone(),
        _ => String::new(),
    };
    let parts: Vec<String> = regex_engine::regex_split(pattern.as_str(), &flags, &s);
    let take = if limit >= 0 {
        parts.len().min(limit as usize)
    } else {
        parts.len()
    };
    let arr_id = heap.alloc_array();
    for p in parts.into_iter().take(take) {
        heap.array_push(arr_id, Value::String(p));
    }
    Value::Array(arr_id)
}

pub fn escape(args: &[Value], _heap: &mut Heap) -> Value {
    let s = match args.first() {
        Some(Value::String(x)) => x.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    let mut out = String::with_capacity(s.len() * 2);
    for c in s.chars() {
        if let Some(esc) = escape_char(c) {
            out.push_str(esc);
        } else {
            out.push(c);
        }
    }
    Value::String(out)
}
