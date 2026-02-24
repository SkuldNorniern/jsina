use crate::runtime::{Heap, Value};

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
    let obj_id = heap.alloc_object();
    heap.set_prop(obj_id, "source", Value::String(pattern.clone()));
    heap.set_prop(obj_id, "flags", Value::String(flags.clone()));
    heap.set_prop(obj_id, "test", Value::Builtin(0x82));
    heap.set_prop(obj_id, "__regexp_pattern", Value::String(pattern));
    heap.set_prop(obj_id, "__regexp_flags", Value::String(flags));
    Value::Object(obj_id)
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
    let found = s.contains(pattern.as_str());
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
