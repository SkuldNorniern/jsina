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
mod symbol;
mod typed_array;
mod dollar262;
mod encode;
mod eval;
mod function_ctor;
mod reflect;

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
        Value::Symbol(_) | Value::Object(_) | Value::Array(_) | Value::Map(_) | Value::Set(_) | Value::Date(_) | Value::Function(_) | Value::Builtin(_) => f64::NAN,
    }
}

pub(crate) fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Undefined | Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(n) => *n != 0,
        Value::Number(n) => *n != 0.0 && !n.is_nan(),
        Value::String(_) | Value::Symbol(_) | Value::Object(_) | Value::Array(_) | Value::Map(_) | Value::Set(_) | Value::Date(_) | Value::Function(_) | Value::Builtin(_) => true,
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
        Value::Symbol(_) => "Symbol()".to_string(),
        Value::Object(_) | Value::Array(_) | Value::Map(_) | Value::Set(_) | Value::Date(_) => "[object Object]".to_string(),
        Value::Function(_) | Value::Builtin(_) => "function".to_string(),
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
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Object(x), Value::Object(y)) => x == y,
        (Value::Array(x), Value::Array(y)) => x == y,
        (Value::Map(x), Value::Map(y)) => x == y,
        (Value::Set(x), Value::Set(y)) => x == y,
        (Value::Date(x), Value::Date(y)) => x == y,
        (Value::Function(x), Value::Function(y)) => x == y,
        (Value::Builtin(x), Value::Builtin(y)) => x == y,
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
    // Object 0..7
    BuiltinDef { category: "Object", name: "create", entry: BuiltinEntry::Normal(object::create) },
    BuiltinDef { category: "Object", name: "keys", entry: BuiltinEntry::Normal(object::keys) },
    BuiltinDef { category: "Object", name: "assign", entry: BuiltinEntry::Normal(object::assign) },
    BuiltinDef { category: "Object", name: "hasOwnProperty", entry: BuiltinEntry::Normal(object::has_own_property) },
    BuiltinDef { category: "Object", name: "preventExtensions", entry: BuiltinEntry::Normal(object::prevent_extensions) },
    BuiltinDef { category: "Object", name: "seal", entry: BuiltinEntry::Normal(object::seal) },
    BuiltinDef { category: "Object", name: "setPrototypeOf", entry: BuiltinEntry::Normal(object::set_prototype_of) },
    BuiltinDef { category: "Object", name: "propertyIsEnumerable", entry: BuiltinEntry::Normal(object::property_is_enumerable) },
    BuiltinDef { category: "Object", name: "getPrototypeOf", entry: BuiltinEntry::Normal(object::get_prototype_of) },
    BuiltinDef { category: "Object", name: "freeze", entry: BuiltinEntry::Normal(object::freeze) },
    BuiltinDef { category: "Object", name: "isExtensible", entry: BuiltinEntry::Normal(object::is_extensible) },
    BuiltinDef { category: "Object", name: "isFrozen", entry: BuiltinEntry::Normal(object::is_frozen) },
    BuiltinDef { category: "Object", name: "isSealed", entry: BuiltinEntry::Normal(object::is_sealed) },
    BuiltinDef { category: "Object", name: "hasOwn", entry: BuiltinEntry::Normal(object::has_own) },
    BuiltinDef { category: "Object", name: "is", entry: BuiltinEntry::Normal(object::is_same_value) },
    // Type 0..3 (String, Error, Number, Boolean constructors)
    BuiltinDef { category: "Type", name: "String", entry: BuiltinEntry::Normal(string::string) },
    BuiltinDef { category: "Type", name: "Error", entry: BuiltinEntry::Normal(error::error) },
    BuiltinDef { category: "Type", name: "Number", entry: BuiltinEntry::Normal(number::number) },
    BuiltinDef { category: "Type", name: "Boolean", entry: BuiltinEntry::Normal(boolean::boolean) },
    BuiltinDef { category: "Number", name: "isSafeInteger", entry: BuiltinEntry::Normal(number::is_safe_integer) },
    BuiltinDef { category: "Number", name: "primitiveToString", entry: BuiltinEntry::Normal(number::primitive_to_string) },
    BuiltinDef { category: "Number", name: "primitiveValueOf", entry: BuiltinEntry::Normal(number::primitive_value_of) },
    // String 0..6 (methods)
    BuiltinDef { category: "String", name: "split", entry: BuiltinEntry::Normal(string::split) },
    BuiltinDef { category: "String", name: "trim", entry: BuiltinEntry::Normal(string::trim) },
    BuiltinDef { category: "String", name: "toLowerCase", entry: BuiltinEntry::Normal(string::to_lower_case) },
    BuiltinDef { category: "String", name: "toUpperCase", entry: BuiltinEntry::Normal(string::to_upper_case) },
    BuiltinDef { category: "String", name: "charAt", entry: BuiltinEntry::Normal(string::char_at) },
    BuiltinDef { category: "String", name: "repeat", entry: BuiltinEntry::Normal(string::repeat) },
    BuiltinDef { category: "String", name: "fromCharCode", entry: BuiltinEntry::Normal(string::from_char_code) },
    BuiltinDef { category: "String", name: "anchor", entry: BuiltinEntry::Normal(string::anchor) },
    BuiltinDef { category: "String", name: "big", entry: BuiltinEntry::Normal(string::big) },
    BuiltinDef { category: "String", name: "blink", entry: BuiltinEntry::Normal(string::blink) },
    BuiltinDef { category: "String", name: "bold", entry: BuiltinEntry::Normal(string::bold) },
    BuiltinDef { category: "String", name: "fixed", entry: BuiltinEntry::Normal(string::fixed) },
    BuiltinDef { category: "String", name: "fontcolor", entry: BuiltinEntry::Normal(string::fontcolor) },
    BuiltinDef { category: "String", name: "fontsize", entry: BuiltinEntry::Normal(string::fontsize) },
    BuiltinDef { category: "String", name: "italics", entry: BuiltinEntry::Normal(string::italics) },
    BuiltinDef { category: "String", name: "link", entry: BuiltinEntry::Normal(string::link) },
    // Error 0 (isError)
    BuiltinDef { category: "Error", name: "isError", entry: BuiltinEntry::Normal(error::is_error) },
    // RegExp 0..2
    BuiltinDef { category: "RegExp", name: "escape", entry: BuiltinEntry::Normal(regexp::escape) },
    BuiltinDef { category: "RegExp", name: "create", entry: BuiltinEntry::Normal(regexp::create) },
    BuiltinDef { category: "RegExp", name: "test", entry: BuiltinEntry::Normal(regexp::test) },
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
    // String annex B HTML (small, strike, sub, sup) 0xB1..0xB4
    BuiltinDef { category: "String", name: "small", entry: BuiltinEntry::Normal(string::small) },
    BuiltinDef { category: "String", name: "strike", entry: BuiltinEntry::Normal(string::strike) },
    BuiltinDef { category: "String", name: "sub", entry: BuiltinEntry::Normal(string::sub) },
    BuiltinDef { category: "String", name: "sup", entry: BuiltinEntry::Normal(string::sup) },
    // Date 0..4
    BuiltinDef { category: "Date", name: "create", entry: BuiltinEntry::Normal(date::create) },
    BuiltinDef { category: "Date", name: "now", entry: BuiltinEntry::Normal(date::now) },
    BuiltinDef { category: "Date", name: "getTime", entry: BuiltinEntry::Normal(date::get_time) },
    BuiltinDef { category: "Date", name: "toString", entry: BuiltinEntry::Normal(date::to_string) },
    BuiltinDef { category: "Date", name: "toISOString", entry: BuiltinEntry::Normal(date::to_iso_string) },
    BuiltinDef { category: "Date", name: "getYear", entry: BuiltinEntry::Normal(date::get_year) },
    BuiltinDef { category: "Date", name: "setYear", entry: BuiltinEntry::Normal(date::set_year) },
    BuiltinDef { category: "Date", name: "toGMTString", entry: BuiltinEntry::Normal(date::to_gmt_string) },
    BuiltinDef { category: "Symbol", name: "create", entry: BuiltinEntry::Normal(symbol::symbol) },
    BuiltinDef { category: "Error", name: "ReferenceError", entry: BuiltinEntry::Normal(error::reference_error) },
    BuiltinDef { category: "Error", name: "TypeError", entry: BuiltinEntry::Normal(error::type_error) },
    BuiltinDef { category: "Error", name: "RangeError", entry: BuiltinEntry::Normal(error::range_error) },
    BuiltinDef { category: "Error", name: "SyntaxError", entry: BuiltinEntry::Normal(error::syntax_error) },
    BuiltinDef { category: "$262", name: "createRealm", entry: BuiltinEntry::Throwing(dollar262::create_realm) },
    BuiltinDef { category: "$262", name: "evalScript", entry: BuiltinEntry::Throwing(dollar262::eval_script) },
    BuiltinDef { category: "$262", name: "gc", entry: BuiltinEntry::Throwing(dollar262::gc) },
    BuiltinDef { category: "$262", name: "detachArrayBuffer", entry: BuiltinEntry::Throwing(dollar262::detach_array_buffer) },
    BuiltinDef { category: "Global", name: "eval", entry: BuiltinEntry::Throwing(eval::eval) },
    BuiltinDef { category: "Global", name: "encodeURI", entry: BuiltinEntry::Normal(encode::encode_uri_builtin) },
    BuiltinDef { category: "Global", name: "encodeURIComponent", entry: BuiltinEntry::Normal(encode::encode_uri_component_builtin) },
    BuiltinDef { category: "Global", name: "parseInt", entry: BuiltinEntry::Normal(number::parse_int) },
    BuiltinDef { category: "Global", name: "parseFloat", entry: BuiltinEntry::Normal(number::parse_float) },
    BuiltinDef { category: "Global", name: "isNaN", entry: BuiltinEntry::Normal(number::is_nan) },
    BuiltinDef { category: "Global", name: "isFinite", entry: BuiltinEntry::Normal(number::is_finite) },
    BuiltinDef { category: "Global", name: "decodeURI", entry: BuiltinEntry::Throwing(encode::decode_uri_builtin) },
    BuiltinDef { category: "Global", name: "decodeURIComponent", entry: BuiltinEntry::Throwing(encode::decode_uri_component_builtin) },
    BuiltinDef { category: "TypedArray", name: "Int32Array", entry: BuiltinEntry::Normal(typed_array::int32_array) },
    BuiltinDef { category: "TypedArray", name: "Uint8Array", entry: BuiltinEntry::Normal(typed_array::uint8_array) },
    BuiltinDef { category: "TypedArray", name: "Uint8ClampedArray", entry: BuiltinEntry::Normal(typed_array::uint8_clamped_array) },
    BuiltinDef { category: "TypedArray", name: "ArrayBuffer", entry: BuiltinEntry::Normal(typed_array::array_buffer) },
    BuiltinDef { category: "Global", name: "Function", entry: BuiltinEntry::Throwing(function_ctor::function_constructor) },
    BuiltinDef { category: "Reflect", name: "apply", entry: BuiltinEntry::Throwing(reflect::reflect_apply) },
    BuiltinDef { category: "Reflect", name: "construct", entry: BuiltinEntry::Throwing(reflect::reflect_construct) },
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
    t[0x44] = 28;
    t[0x45] = 29;
    t[0x46] = 30;
    t[0x47] = 31;
    t[0x48] = 32;
    t[0x49] = 33;
    t[0x4A] = 34;
    t[0x4B] = 35;
    t[0x4C] = 36;
    t[0x4D] = 37;
    t[0x4E] = 38;
    t[0x50] = 39;
    t[0x51] = 40;
    t[0x52] = 41;
    t[0x53] = 42;
    t[0x54] = 43;
    t[0x55] = 44;
    t[0x56] = 45;
    t[0x60] = 46;
    t[0x61] = 47;
    t[0x62] = 48;
    t[0x63] = 49;
    t[0x64] = 50;
    t[0x65] = 51;
    t[0x66] = 52;
    t[0x67] = 53;
    t[0x68] = 54;
    t[0x69] = 55;
    t[0x6A] = 56;
    t[0x6B] = 57;
    t[0x6C] = 58;
    t[0x6D] = 59;
    t[0x6E] = 60;
    t[0x6F] = 61;
    t[0x70] = 62;
    t[0x80] = 63;
    t[0x81] = 64;
    t[0x82] = 65;
    t[0x90] = 66;
    t[0x91] = 67;
    t[0x92] = 68;
    t[0x93] = 69;
    t[0xA0] = 70;
    t[0xA1] = 71;
    t[0xA2] = 72;
    t[0xA3] = 73;
    t[0xB0] = 74;
    t[0xB1] = 75;
    t[0xB2] = 76;
    t[0xB3] = 77;
    t[0xB4] = 78;
    t[0xC0] = 79;
    t[0xC1] = 80;
    t[0xC2] = 81;
    t[0xC3] = 82;
    t[0xC4] = 83;
    t[0xC5] = 84;
    t[0xC6] = 85;
    t[0xC7] = 86;
    t[0xD0] = 87;
    t[0xD1] = 88;
    t[0xD2] = 89;
    t[0xD3] = 90;
    t[0xD4] = 91;
    t[0xD5] = 92;
    t[0xD6] = 93;
    t[0xD7] = 94;
    t[0xD8] = 95;
    t[0xD9] = 96;
    t[0xDA] = 97;
    t[0xDB] = 98;
    t[0xDC] = 99;
    t[0xDD] = 100;
    t[0xDE] = 101;
    t[0xDF] = 102;
    t[0xE0] = 103;
    t[0xE1] = 104;
    t[0xE2] = 105;
    t[0xE3] = 106;
    t[0xE4] = 107;
    t[0xE5] = 108;
    t[0xE6] = 109;
    t[0xE7] = 110;
    t[0xE8] = 111;
    t
};

pub const MAX_BUILTIN_ID: u8 = 0xE8;

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

const INDEX_TO_ENCODED: [u8; 112] = [
    0x00, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B,
    0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
    0x30, 0x31,
    0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
    0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E,
    0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56,
    0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66,
    0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
    0x70,
    0x80, 0x81, 0x82,
    0x90, 0x91, 0x92, 0x93,
    0xA0, 0xA1, 0xA2, 0xA3,
    0xB0, 0xB1, 0xB2, 0xB3, 0xB4,
    0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
    0xD0, 0xD1, 0xD2, 0xD3, 0xD4,
    0xD5, 0xD6, 0xD7, 0xD8,
    0xD9, 0xDA, 0xDB,
    0xDC, 0xDD,
    0xDE, 0xDF,
    0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8,
];

pub fn by_category(cat: &str) -> impl Iterator<Item = (u8, &'static BuiltinDef)> {
    BUILTINS
        .iter()
        .enumerate()
        .filter(move |(_, b)| b.category == cat)
        .filter_map(|(i, b)| INDEX_TO_ENCODED.get(i).map(|&id| (id, b)))
}

/// Resolve (category, name) to builtin id. For lower/compile consistency.
pub fn resolve(category: &str, name: &str) -> Option<u8> {
    BUILTINS
        .iter()
        .enumerate()
        .find(|(_, b)| b.category == category && b.name == name)
        .and_then(|(i, _)| INDEX_TO_ENCODED.get(i).copied())
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Heap;

    #[test]
    fn dispatch_regexp_escape() {
        let mut heap = Heap::new();
        let args = [crate::runtime::Value::String(".".to_string())];
        let r = dispatch(0x80, &args, &mut heap);
        assert!(r.is_ok(), "dispatch failed: {:?}", r);
        let v = r.unwrap();
        let expected = crate::runtime::Value::String("\\.".to_string());
        assert_eq!(v, expected, "RegExp.escape(\".\") should return \"\\.\"");
    }

    #[test]
    fn resolve_known_builtins() {
        assert_eq!(resolve("Host", "print"), Some(0x00));
        assert_eq!(resolve("Array", "push"), Some(0x10));
        assert_eq!(resolve("Math", "floor"), Some(0x20));
        assert_eq!(resolve("Json", "parse"), Some(0x30));
        assert_eq!(resolve("Object", "preventExtensions"), Some(0x44));
        assert_eq!(resolve("Object", "setPrototypeOf"), Some(0x46));
        assert_eq!(resolve("String", "fromCharCode"), Some(0x66));
        assert_eq!(resolve("Date", "now"), Some(0xC1));
        assert_eq!(resolve("Unknown", "foo"), None);
    }
}
