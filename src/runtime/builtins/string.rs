use super::{to_number, to_prop_key};
use crate::runtime::{Heap, Value};

fn html_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("&quot;"),
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            c => out.push(c),
        }
    }
    out
}

fn string_html_receiver(args: &[Value]) -> String {
    args.first().map(|v| to_prop_key(v)).unwrap_or_default()
}

pub fn string(args: &[Value], _heap: &mut Heap) -> Value {
    let arg = args.first().map(|v| v.to_string()).unwrap_or_default();
    Value::String(arg)
}

pub fn trim(args: &[Value], _heap: &mut Heap) -> Value {
    let s = match args.first() {
        Some(Value::String(x)) => x.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    Value::String(s.trim().to_string())
}

pub fn to_lower_case(args: &[Value], _heap: &mut Heap) -> Value {
    let s = match args.first() {
        Some(Value::String(x)) => x.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    Value::String(s.to_lowercase())
}

pub fn to_upper_case(args: &[Value], _heap: &mut Heap) -> Value {
    let s = match args.first() {
        Some(Value::String(x)) => x.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    Value::String(s.to_uppercase())
}

pub fn repeat(args: &[Value], _heap: &mut Heap) -> Value {
    let s = match args.first() {
        Some(Value::String(x)) => x.clone(),
        Some(v) => v.to_string(),
        None => String::new(),
    };
    let count = match args.get(1) {
        Some(v) => super::to_number(v),
        None => 0.0,
    };
    let n = if count.is_nan() || count < 0.0 || count.is_infinite() {
        0
    } else {
        count as i32
    };
    let n = n.max(0) as usize;
    Value::String(s.repeat(n))
}

pub fn from_char_code(args: &[Value], _heap: &mut Heap) -> Value {
    let mut s = String::new();
    for v in args {
        let n = super::to_number(v);
        let code = if n.is_nan() || n.is_infinite() {
            0
        } else {
            n as i32
        };
        if let Some(c) = char::from_u32(code as u32 & 0xFFFF) {
            s.push(c);
        }
    }
    Value::String(s)
}

pub fn char_at(args: &[Value], _heap: &mut Heap) -> Value {
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

pub fn anchor(args: &[Value], _heap: &mut Heap) -> Value {
    let s = string_html_receiver(args);
    let name = args.get(1).map(|v| html_escape(&to_prop_key(v))).unwrap_or_default();
    Value::String(format!(r#"<a name="{}">{}</a>"#, name, s))
}

pub fn big(args: &[Value], _heap: &mut Heap) -> Value {
    let s = string_html_receiver(args);
    Value::String(format!("<big>{}</big>", s))
}

pub fn blink(args: &[Value], _heap: &mut Heap) -> Value {
    let s = string_html_receiver(args);
    Value::String(format!("<blink>{}</blink>", s))
}

pub fn bold(args: &[Value], _heap: &mut Heap) -> Value {
    let s = string_html_receiver(args);
    Value::String(format!("<b>{}</b>", s))
}

pub fn fixed(args: &[Value], _heap: &mut Heap) -> Value {
    let s = string_html_receiver(args);
    Value::String(format!("<tt>{}</tt>", s))
}

pub fn fontcolor(args: &[Value], _heap: &mut Heap) -> Value {
    let s = string_html_receiver(args);
    let color = args.get(1).map(|v| html_escape(&to_prop_key(v))).unwrap_or_default();
    Value::String(format!(r#"<font color="{}">{}</font>"#, color, s))
}

pub fn fontsize(args: &[Value], _heap: &mut Heap) -> Value {
    let s = string_html_receiver(args);
    let size = args.get(1).map(|v| html_escape(&to_prop_key(v))).unwrap_or_default();
    Value::String(format!(r#"<font size="{}">{}</font>"#, size, s))
}

pub fn italics(args: &[Value], _heap: &mut Heap) -> Value {
    let s = string_html_receiver(args);
    Value::String(format!("<i>{}</i>", s))
}

pub fn link(args: &[Value], _heap: &mut Heap) -> Value {
    let s = string_html_receiver(args);
    let url = args.get(1).map(|v| html_escape(&to_prop_key(v))).unwrap_or_default();
    Value::String(format!(r#"<a href="{}">{}</a>"#, url, s))
}

pub fn small(args: &[Value], _heap: &mut Heap) -> Value {
    let s = string_html_receiver(args);
    Value::String(format!("<small>{}</small>", s))
}

pub fn strike(args: &[Value], _heap: &mut Heap) -> Value {
    let s = string_html_receiver(args);
    Value::String(format!("<strike>{}</strike>", s))
}

pub fn sub(args: &[Value], _heap: &mut Heap) -> Value {
    let s = string_html_receiver(args);
    Value::String(format!("<sub>{}</sub>", s))
}

pub fn sup(args: &[Value], _heap: &mut Heap) -> Value {
    let s = string_html_receiver(args);
    Value::String(format!("<sup>{}</sup>", s))
}
