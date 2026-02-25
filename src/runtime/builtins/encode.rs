//! encodeURI, encodeURIComponent, decodeURI, decodeURIComponent - percent-encode/decode URI.
use super::{BuiltinError, to_prop_key};
use crate::runtime::Value;

const DECODE_URI_RESERVED: &[u8] = b";,/?:@&=+$#";
const HEX: &[u8; 16] = b"0123456789ABCDEF";

fn push_percent_encoded(out: &mut String, b: u8) {
    out.push('%');
    out.push(HEX[(b >> 4) as usize] as char);
    out.push(HEX[(b & 0xf) as usize] as char);
}

fn encode_uri_component(s: &str) -> String {
    let mut out = String::with_capacity(s.len() * 3);
    let mut buf = [0u8; 4];
    for c in s.chars() {
        match c {
            'A'..='Z'
            | 'a'..='z'
            | '0'..='9'
            | '-'
            | '_'
            | '.'
            | '!'
            | '~'
            | '*'
            | '\''
            | '('
            | ')' => {
                out.push(c);
            }
            _ => {
                for b in c.encode_utf8(&mut buf).as_bytes() {
                    push_percent_encoded(&mut out, *b);
                }
            }
        }
    }
    out
}

fn encode_uri(s: &str) -> String {
    let mut out = String::with_capacity(s.len() * 3);
    let mut buf = [0u8; 4];
    for c in s.chars() {
        match c {
            'A'..='Z'
            | 'a'..='z'
            | '0'..='9'
            | '-'
            | '_'
            | '.'
            | '!'
            | '~'
            | '*'
            | '\''
            | '('
            | ')'
            | ';'
            | ','
            | '/'
            | '?'
            | ':'
            | '@'
            | '&'
            | '='
            | '+'
            | '$'
            | '#' => {
                out.push(c);
            }
            _ => {
                for b in c.encode_utf8(&mut buf).as_bytes() {
                    push_percent_encoded(&mut out, *b);
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

fn hex_digit(b: u8) -> Option<u8> {
    match b {
        b'0'..=b'9' => Some(b - b'0'),
        b'A'..=b'F' => Some(b - b'A' + 10),
        b'a'..=b'f' => Some(b - b'a' + 10),
        _ => None,
    }
}

fn decode_uri_impl(s: &str, decode_reserved: bool) -> Result<String, ()> {
    let bytes = s.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' {
            if i + 2 >= bytes.len() {
                return Err(());
            }
            let hi = hex_digit(bytes[i + 1]).ok_or(())?;
            let lo = hex_digit(bytes[i + 2]).ok_or(())?;
            let b = (hi << 4) | lo;
            if !decode_reserved && DECODE_URI_RESERVED.contains(&b) {
                out.push(bytes[i]);
                out.push(bytes[i + 1]);
                out.push(bytes[i + 2]);
            } else {
                out.push(b);
            }
            i += 3;
        } else {
            out.push(bytes[i]);
            i += 1;
        }
    }
    String::from_utf8(out).map_err(|_| ())
}

pub fn decode_uri_builtin(
    args: &[Value],
    ctx: &mut super::BuiltinContext,
) -> Result<Value, BuiltinError> {
    let s = args.first().map(|v| to_prop_key(v)).unwrap_or_default();
    decode_uri_impl(&s, false).map(Value::String).map_err(|_| {
        BuiltinError::Throw(super::error::uri_error(
            &[Value::String("URI malformed".to_string())],
            ctx.heap,
        ))
    })
}

pub fn decode_uri_component_builtin(
    args: &[Value],
    ctx: &mut super::BuiltinContext,
) -> Result<Value, BuiltinError> {
    let s = args.first().map(|v| to_prop_key(v)).unwrap_or_default();
    decode_uri_impl(&s, true).map(Value::String).map_err(|_| {
        BuiltinError::Throw(super::error::uri_error(
            &[Value::String("URI malformed".to_string())],
            ctx.heap,
        ))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decode_percent_throws() {
        assert!(decode_uri_impl("%", true).is_err());
        assert!(decode_uri_impl("%A", true).is_err());
        assert!(decode_uri_impl("%1", true).is_err());
    }

    #[test]
    fn decode_valid() {
        assert_eq!(decode_uri_impl("%41", true).unwrap(), "A");
        assert_eq!(decode_uri_impl("a%42c", true).unwrap(), "aBc");
    }
}
