use crate::runtime::Value;

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

pub fn escape(args: &[Value]) -> Value {
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
