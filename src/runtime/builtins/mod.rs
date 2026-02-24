//! Builtin function dispatch.
//!
//! IDs 0-34 map to BuiltinId in src/ir/hir.rs.
//! See submodules: host, object, array, string, number, boolean, error, math, json.

mod array;
mod boolean;
mod error;
mod map;
mod host;
mod json;
mod math;
mod number;
mod object;
mod regexp;
mod string;

use crate::runtime::{Heap, Value};
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug)]
pub enum BuiltinError {
    Throw(Value),
}

pub(crate) fn to_number(v: &Value) -> f64 {
    match v {
        Value::Int(n) => *n as f64,
        Value::Number(n) => *n,
        Value::Bool(b) => if *b { 1.0 } else { 0.0 },
        Value::Null => 0.0,
        Value::Undefined => f64::NAN,
        Value::String(s) => s.parse().unwrap_or_else(|_| f64::NAN),
        Value::Object(_) | Value::Array(_) | Value::Map(_) | Value::Function(_) => f64::NAN,
    }
}

pub(crate) fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Undefined | Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(n) => *n != 0,
        Value::Number(n) => *n != 0.0 && !n.is_nan(),
        Value::String(_) | Value::Object(_) | Value::Array(_) | Value::Map(_) | Value::Function(_) => true,
    }
}

pub(crate) fn to_prop_key(v: &Value) -> String {
    match v {
        Value::String(s) => s.clone(),
        Value::Int(n) => n.to_string(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Null => "null".to_string(),
        Value::Undefined => "undefined".to_string(),
        Value::Object(_) | Value::Array(_) | Value::Map(_) => "[object Object]".to_string(),
        Value::Function(_) => "function".to_string(),
    }
}

pub(crate) fn strict_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Undefined, Value::Undefined) => true,
        (Value::Null, Value::Null) => true,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Number(x), Value::Number(y)) => !x.is_nan() && !y.is_nan() && x == y,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Object(x), Value::Object(y)) => x == y,
        (Value::Array(x), Value::Array(y)) => x == y,
        (Value::Map(x), Value::Map(y)) => x == y,
        (Value::Function(x), Value::Function(y)) => x == y,
        _ => false,
    }
}

pub(crate) fn number_to_value(n: f64) -> Value {
    if n.is_finite() && n.fract() == 0.0 && n >= i32::MIN as f64 && n <= i32::MAX as f64 {
        Value::Int(n as i32)
    } else {
        Value::Number(n)
    }
}

static RNG_STATE: AtomicU64 = AtomicU64::new(0);

pub(crate) fn random_f64() -> f64 {
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

pub fn seed_random(seed: u64) {
    RNG_STATE.store(if seed == 0 { 1 } else { seed }, Ordering::Relaxed);
}

pub fn dispatch(id: u8, args: &[Value], heap: &mut Heap) -> Result<Value, BuiltinError> {
    let result = match id {
        0 => host::print(args),
        1 => array::push(args, heap),
        2 => array::pop(args, heap),
        3 => math::floor(args),
        4 => math::abs(args),
        5 => math::min(args),
        6 => math::max(args),
        7 => json::parse(args, heap)?,
        8 => json::stringify(args, heap),
        9 => object::create(args, heap),
        10 => array::is_array(args),
        11 => object::keys(args, heap),
        12 => string::string(args),
        13 => error::error(args, heap),
        14 => number::number(args),
        15 => boolean::boolean(args),
        16 => array::slice(args, heap),
        17 => array::concat(args, heap),
        18 => object::assign(args, heap),
        19 => array::index_of(args, heap),
        20 => array::join(args, heap),
        21 => math::pow(args),
        22 => array::shift(args, heap),
        23 => array::unshift(args, heap),
        24 => string::split(args, heap),
        25 => string::trim(args),
        26 => string::to_lower_case(args),
        27 => string::to_upper_case(args),
        28 => math::ceil(args),
        29 => math::round(args),
        30 => math::sqrt(args),
        31 => math::random(),
        32 => object::has_own_property(args, heap),
        33 => array::reverse(args, heap),
        34 => string::char_at(args),
        35 => error::is_error(args, heap),
        36 => regexp::escape(args),
        37 => array::includes(args, heap),
        38 => string::repeat(args),
        39 => array::fill(args, heap),
        40 => map::create(heap),
        41 => map::set(args, heap),
        42 => map::get(args, heap),
        43 => map::has(args, heap),
        _ => unreachable!("invalid builtin id checked by caller"),
    };
    Ok(result)
}
