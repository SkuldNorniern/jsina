//! encodeURI and encodeURIComponent - percent-encode URI components.
use crate::runtime::Value;
use super::to_prop_key;

fn encode_uri_component(s: &str) -> String {
    let mut out = String::with_capacity(s.len() * 3);
    for c in s.chars() {
        match c {
            'A'..='Z' | 'a'..='z' | '0'..='9' | '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' => {
                out.push(c);
            }
            _ => {
                for b in c.to_string().as_bytes() {
                    out.push_str(&format!("%{:02X}", b));
                }
            }
        }
    }
    out
}

fn encode_uri(s: &str) -> String {
    let mut out = String::with_capacity(s.len() * 3);
    for c in s.chars() {
        match c {
            'A'..='Z' | 'a'..='z' | '0'..='9' | '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' |
            ';' | ',' | '/' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | '#' => {
                out.push(c);
            }
            _ => {
                for b in c.to_string().as_bytes() {
                    out.push_str(&format!("%{:02X}", b));
                }
            }
        }
    }
    out
}

pub fn encode_uri_builtin(args: &[Value], _heap: &mut crate::runtime::Heap) -> Value {
    let s = args.first().map(|v| to_prop_key(v)).unwrap_or_default();
    Value::String(encode_uri(&s))
}

pub fn encode_uri_component_builtin(args: &[Value], _heap: &mut crate::runtime::Heap) -> Value {
    let s = args.first().map(|v| to_prop_key(v)).unwrap_or_default();
    Value::String(encode_uri_component(&s))
}
