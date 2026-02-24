//! Builtin function dispatch.
//!
//! IDs 0..=MAX_BUILTIN_ID map to BuiltinId in src/ir/hir.rs.
//! See submodules: host, object, array, string, number, boolean, error, math, json, map, set.

mod array;
mod boolean;
mod date;
mod error;
mod map;
mod set;
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
        Value::Object(_) | Value::Array(_) | Value::Map(_) | Value::Set(_) | Value::Date(_) | Value::Function(_) => f64::NAN,
    }
}

pub(crate) fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Undefined | Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(n) => *n != 0,
        Value::Number(n) => *n != 0.0 && !n.is_nan(),
        Value::String(_) | Value::Object(_) | Value::Array(_) | Value::Map(_) | Value::Set(_) | Value::Date(_) | Value::Function(_) => true,
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
        Value::Object(_) | Value::Array(_) | Value::Map(_) | Value::Set(_) | Value::Date(_) => "[object Object]".to_string(),
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
        (Value::Set(x), Value::Set(y)) => x == y,
        (Value::Date(x), Value::Date(y)) => x == y,
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

fn collection_has(args: &[Value], heap: &mut Heap) -> Value {
    let (receiver, value) = match args {
        [r, v] => (r, v),
        _ => return Value::Bool(false),
    };
    let key = to_prop_key(value);
    if let Some(id) = receiver.as_map_id() {
        return Value::Bool(heap.map_has(id, &key));
    }
    if let Some(id) = receiver.as_set_id() {
        return Value::Bool(heap.set_has(id, &key));
    }
    Value::Bool(false)
}

pub fn seed_random(seed: u64) {
    RNG_STATE.store(if seed == 0 { 1 } else { seed }, Ordering::Relaxed);
}

pub type BuiltinFn = fn(&[Value], &mut Heap) -> Value;
type ThrowingBuiltinFn = fn(&[Value], &mut Heap) -> Result<Value, BuiltinError>;

pub trait Builtin {
    fn call(&self, args: &[Value], heap: &mut Heap) -> Result<Value, BuiltinError>;
}

enum BuiltinEntry {
    Normal(BuiltinFn),
    Throwing(ThrowingBuiltinFn),
}

impl Builtin for BuiltinEntry {
    fn call(&self, args: &[Value], heap: &mut Heap) -> Result<Value, BuiltinError> {
        match self {
            Self::Normal(f) => Ok(f(args, heap)),
            Self::Throwing(f) => f(args, heap),
        }
    }
}

pub struct BuiltinDef {
    pub category: &'static str,
    pub name: &'static str,
    entry: BuiltinEntry,
}

const BUILTINS: &[BuiltinDef] = &[
    // Host
    BuiltinDef { category: "Host", name: "print", entry: BuiltinEntry::Normal(host::print) },
    // Array 0..11
    BuiltinDef { category: "Array", name: "push", entry: BuiltinEntry::Normal(array::push) },
    BuiltinDef { category: "Array", name: "pop", entry: BuiltinEntry::Normal(array::pop) },
    BuiltinDef { category: "Array", name: "isArray", entry: BuiltinEntry::Normal(array::is_array) },
    BuiltinDef { category: "Array", name: "slice", entry: BuiltinEntry::Normal(array::slice) },
    BuiltinDef { category: "Array", name: "concat", entry: BuiltinEntry::Normal(array::concat) },
    BuiltinDef { category: "Array", name: "indexOf", entry: BuiltinEntry::Normal(array::index_of) },
    BuiltinDef { category: "Array", name: "join", entry: BuiltinEntry::Normal(array::join) },
    BuiltinDef { category: "Array", name: "shift", entry: BuiltinEntry::Normal(array::shift) },
    BuiltinDef { category: "Array", name: "unshift", entry: BuiltinEntry::Normal(array::unshift) },
    BuiltinDef { category: "Array", name: "reverse", entry: BuiltinEntry::Normal(array::reverse) },
    BuiltinDef { category: "Array", name: "includes", entry: BuiltinEntry::Normal(array::includes) },
    BuiltinDef { category: "Array", name: "fill", entry: BuiltinEntry::Normal(array::fill) },
    // Math 0..8
    BuiltinDef { category: "Math", name: "floor", entry: BuiltinEntry::Normal(math::floor) },
    BuiltinDef { category: "Math", name: "abs", entry: BuiltinEntry::Normal(math::abs) },
    BuiltinDef { category: "Math", name: "min", entry: BuiltinEntry::Normal(math::min) },
    BuiltinDef { category: "Math", name: "max", entry: BuiltinEntry::Normal(math::max) },
    BuiltinDef { category: "Math", name: "pow", entry: BuiltinEntry::Normal(math::pow) },
    BuiltinDef { category: "Math", name: "ceil", entry: BuiltinEntry::Normal(math::ceil) },
    BuiltinDef { category: "Math", name: "round", entry: BuiltinEntry::Normal(math::round) },
    BuiltinDef { category: "Math", name: "sqrt", entry: BuiltinEntry::Normal(math::sqrt) },
    BuiltinDef { category: "Math", name: "random", entry: BuiltinEntry::Normal(math::random) },
    // Json 0..1
    BuiltinDef { category: "Json", name: "parse", entry: BuiltinEntry::Throwing(json::parse) },
    BuiltinDef { category: "Json", name: "stringify", entry: BuiltinEntry::Normal(json::stringify) },
    // Object 0..3
    BuiltinDef { category: "Object", name: "create", entry: BuiltinEntry::Normal(object::create) },
    BuiltinDef { category: "Object", name: "keys", entry: BuiltinEntry::Normal(object::keys) },
    BuiltinDef { category: "Object", name: "assign", entry: BuiltinEntry::Normal(object::assign) },
    BuiltinDef { category: "Object", name: "hasOwnProperty", entry: BuiltinEntry::Normal(object::has_own_property) },
    // Type 0..3 (String, Error, Number, Boolean constructors)
    BuiltinDef { category: "Type", name: "String", entry: BuiltinEntry::Normal(string::string) },
    BuiltinDef { category: "Type", name: "Error", entry: BuiltinEntry::Normal(error::error) },
    BuiltinDef { category: "Type", name: "Number", entry: BuiltinEntry::Normal(number::number) },
    BuiltinDef { category: "Type", name: "Boolean", entry: BuiltinEntry::Normal(boolean::boolean) },
    // String 0..5 (methods)
    BuiltinDef { category: "String", name: "split", entry: BuiltinEntry::Normal(string::split) },
    BuiltinDef { category: "String", name: "trim", entry: BuiltinEntry::Normal(string::trim) },
    BuiltinDef { category: "String", name: "toLowerCase", entry: BuiltinEntry::Normal(string::to_lower_case) },
    BuiltinDef { category: "String", name: "toUpperCase", entry: BuiltinEntry::Normal(string::to_upper_case) },
    BuiltinDef { category: "String", name: "charAt", entry: BuiltinEntry::Normal(string::char_at) },
    BuiltinDef { category: "String", name: "repeat", entry: BuiltinEntry::Normal(string::repeat) },
    // Error 0 (isError)
    BuiltinDef { category: "Error", name: "isError", entry: BuiltinEntry::Normal(error::is_error) },
    // RegExp 0
    BuiltinDef { category: "RegExp", name: "escape", entry: BuiltinEntry::Normal(regexp::escape) },
    // Map 0..3
    BuiltinDef { category: "Map", name: "create", entry: BuiltinEntry::Normal(map::create) },
    BuiltinDef { category: "Map", name: "set", entry: BuiltinEntry::Normal(map::set) },
    BuiltinDef { category: "Map", name: "get", entry: BuiltinEntry::Normal(map::get) },
    BuiltinDef { category: "Map", name: "has", entry: BuiltinEntry::Normal(map::has) },
    // Set 0..3
    BuiltinDef { category: "Set", name: "create", entry: BuiltinEntry::Normal(set::create) },
    BuiltinDef { category: "Set", name: "add", entry: BuiltinEntry::Normal(set::add) },
    BuiltinDef { category: "Set", name: "has", entry: BuiltinEntry::Normal(set::has) },
    BuiltinDef { category: "Set", name: "size", entry: BuiltinEntry::Normal(set::size) },
    // Collection 0 (Map/Set .has shared)
    BuiltinDef { category: "Collection", name: "has", entry: BuiltinEntry::Normal(collection_has) },
    // Date 0..4
    BuiltinDef { category: "Date", name: "create", entry: BuiltinEntry::Normal(date::create) },
    BuiltinDef { category: "Date", name: "now", entry: BuiltinEntry::Normal(date::now) },
    BuiltinDef { category: "Date", name: "getTime", entry: BuiltinEntry::Normal(date::get_time) },
    BuiltinDef { category: "Date", name: "toString", entry: BuiltinEntry::Normal(date::to_string) },
    BuiltinDef { category: "Date", name: "toISOString", entry: BuiltinEntry::Normal(date::to_iso_string) },
];

const INVALID: u8 = 0xFF;

static ENCODED_TO_INDEX: [u8; 256] = {
    let mut t = [INVALID; 256];
    t[0x00] = 0;
    t[0x10] = 1;
    t[0x11] = 2;
    t[0x12] = 3;
    t[0x13] = 4;
    t[0x14] = 5;
    t[0x15] = 6;
    t[0x16] = 7;
    t[0x17] = 8;
    t[0x18] = 9;
    t[0x19] = 10;
    t[0x1A] = 11;
    t[0x1B] = 12;
    t[0x20] = 13;
    t[0x21] = 14;
    t[0x22] = 15;
    t[0x23] = 16;
    t[0x24] = 17;
    t[0x25] = 18;
    t[0x26] = 19;
    t[0x27] = 20;
    t[0x28] = 21;
    t[0x30] = 22;
    t[0x31] = 23;
    t[0x40] = 24;
    t[0x41] = 25;
    t[0x42] = 26;
    t[0x43] = 27;
    t[0x50] = 28;
    t[0x51] = 29;
    t[0x52] = 30;
    t[0x53] = 31;
    t[0x60] = 32;
    t[0x61] = 33;
    t[0x62] = 34;
    t[0x63] = 35;
    t[0x64] = 36;
    t[0x65] = 37;
    t[0x70] = 38;
    t[0x80] = 39;
    t[0x90] = 40;
    t[0x91] = 41;
    t[0x92] = 42;
    t[0x93] = 43;
    t[0xA0] = 44;
    t[0xA1] = 45;
    t[0xA2] = 46;
    t[0xA3] = 47;
    t[0xB0] = 48;
    t[0xC0] = 49;
    t[0xC1] = 50;
    t[0xC2] = 51;
    t[0xC3] = 52;
    t[0xC4] = 53;
    t
};

pub const MAX_BUILTIN_ID: u8 = 0xC4;

fn index_for(id: u8) -> Option<usize> {
    let idx = ENCODED_TO_INDEX[id as usize];
    if idx == INVALID {
        None
    } else {
        Some(idx as usize)
    }
}

pub fn name(id: u8) -> &'static str {
    index_for(id)
        .and_then(|i| BUILTINS.get(i))
        .map(|b| b.name)
        .unwrap_or("?")
}

pub fn category(id: u8) -> &'static str {
    index_for(id)
        .and_then(|i| BUILTINS.get(i))
        .map(|b| b.category)
        .unwrap_or("?")
}

pub fn get(id: u8) -> Option<&'static BuiltinDef> {
    index_for(id).and_then(|i| BUILTINS.get(i))
}

pub fn all() -> &'static [BuiltinDef] {
    BUILTINS
}

const INDEX_TO_ENCODED: [u8; 54] = [
    0x00, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B,
    0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
    0x30, 0x31,
    0x40, 0x41, 0x42, 0x43,
    0x50, 0x51, 0x52, 0x53,
    0x60, 0x61, 0x62, 0x63, 0x64, 0x65,
    0x70,
    0x80,
    0x90, 0x91, 0x92, 0x93,
    0xA0, 0xA1, 0xA2, 0xA3,
    0xB0,
    0xC0, 0xC1, 0xC2, 0xC3, 0xC4,
];

pub fn by_category(cat: &str) -> impl Iterator<Item = (u8, &'static BuiltinDef)> {
    BUILTINS
        .iter()
        .enumerate()
        .filter(move |(_, b)| b.category == cat)
        .filter_map(|(i, b)| INDEX_TO_ENCODED.get(i).map(|&id| (id, b)))
}

pub fn dispatch(id: u8, args: &[Value], heap: &mut Heap) -> Result<Value, BuiltinError> {
    let idx = match index_for(id) {
        Some(i) => i,
        None => {
            return Err(BuiltinError::Throw(Value::String(
                "invalid builtin id".to_string(),
            )));
        }
    };
    BUILTINS[idx].entry.call(args, heap)
}
