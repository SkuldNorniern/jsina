use super::{number_to_value, random_f64, to_number};
use crate::runtime::{Heap, Value};

pub fn floor(args: &[Value], _heap: &mut Heap) -> Value {
    let n = args.first().map(|x| match x {
        Value::Int(i) => *i as f64,
        Value::Number(n) => *n,
        _ => f64::NAN,
    }).unwrap_or(f64::NAN);
    number_to_value(n.floor())
}

pub fn abs(args: &[Value], _heap: &mut Heap) -> Value {
    args.first().map(|x| match x {
        Value::Int(i) => Value::Int(if *i < 0 { -(*i) } else { *i }),
        Value::Number(n) => Value::Number(n.abs()),
        _ => Value::Number(f64::NAN),
    }).unwrap_or(Value::Number(f64::NAN))
}

pub fn min(args: &[Value], _heap: &mut Heap) -> Value {
    let nums: Vec<f64> = args.iter().map(to_number).collect();
    let m = if nums.is_empty() {
        f64::INFINITY
    } else {
        nums.iter().fold(f64::INFINITY, |a, &b| a.min(b))
    };
    number_to_value(m)
}

pub fn max(args: &[Value], _heap: &mut Heap) -> Value {
    let nums: Vec<f64> = args.iter().map(to_number).collect();
    let m = if nums.is_empty() {
        f64::NEG_INFINITY
    } else {
        nums.iter().fold(f64::NEG_INFINITY, |a, &b| a.max(b))
    };
    number_to_value(m)
}

pub fn pow(args: &[Value], _heap: &mut Heap) -> Value {
    let base = args.get(0).map(to_number).unwrap_or(f64::NAN);
    let exp = args.get(1).map(to_number).unwrap_or(f64::NAN);
    number_to_value(base.powf(exp))
}

pub fn ceil(args: &[Value], _heap: &mut Heap) -> Value {
    let n = args.first().map(to_number).unwrap_or(f64::NAN);
    number_to_value(n.ceil())
}

pub fn round(args: &[Value], _heap: &mut Heap) -> Value {
    let n = args.first().map(to_number).unwrap_or(f64::NAN);
    number_to_value(n.round())
}

pub fn sqrt(args: &[Value], _heap: &mut Heap) -> Value {
    let n = args.first().map(to_number).unwrap_or(f64::NAN);
    number_to_value(n.sqrt())
}

pub fn random(_args: &[Value], _heap: &mut Heap) -> Value {
    Value::Number(random_f64())
}
