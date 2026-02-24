use super::{number_to_value, to_number, to_prop_key};
use crate::runtime::{Heap, Value};

pub fn number(args: &[Value], _heap: &mut Heap) -> Value {
    let n = args.first().map(to_number).unwrap_or(f64::NAN);
    number_to_value(n)
}

pub fn parse_int(args: &[Value], _heap: &mut Heap) -> Value {
    let s = args.first().map(|v| to_prop_key(v)).unwrap_or_default();
    let radix = args.get(1).map(|v| to_number(v) as i32).unwrap_or(10);
    let s = s.trim_start();
    let (s, sign) = if s.starts_with('-') {
        (&s[1..], -1)
    } else if s.starts_with('+') {
        (&s[1..], 1)
    } else {
        (s, 1)
    };
    let radix = if radix == 0 { 10 } else { radix.clamp(2, 36) };
    let s = if s.len() >= 2 && s.starts_with("0x") && radix == 16 {
        &s[2..]
    } else if s.len() >= 2 && s.starts_with("0X") && radix == 16 {
        &s[2..]
    } else {
        s
    };
    let mut n: i64 = 0;
    for c in s.chars() {
        let d = match c {
            '0'..='9' => c as u32 - '0' as u32,
            'a'..='z' => c as u32 - 'a' as u32 + 10,
            'A'..='Z' => c as u32 - 'A' as u32 + 10,
            _ => break,
        };
        if d >= radix as u32 {
            break;
        }
        n = n.saturating_mul(radix as i64).saturating_add(d as i64);
    }
    number_to_value((n * sign) as f64)
}

pub fn parse_float(args: &[Value], _heap: &mut Heap) -> Value {
    let s = args.first().map(|v| to_prop_key(v)).unwrap_or_default();
    let s = s.trim_start();
    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;
    if i < chars.len() && (chars[i] == '-' || chars[i] == '+') {
        i += 1;
    }
    while i < chars.len() && chars[i].is_ascii_digit() {
        i += 1;
    }
    if i < chars.len() && chars[i] == '.' {
        i += 1;
        while i < chars.len() && chars[i].is_ascii_digit() {
            i += 1;
        }
    }
    if i < chars.len() && (chars[i] == 'e' || chars[i] == 'E') {
        i += 1;
        if i < chars.len() && (chars[i] == '-' || chars[i] == '+') {
            i += 1;
        }
        while i < chars.len() && chars[i].is_ascii_digit() {
            i += 1;
        }
    }
    let buf: String = chars[..i].iter().collect();
    let n: f64 = buf.parse().unwrap_or(f64::NAN);
    number_to_value(n)
}
