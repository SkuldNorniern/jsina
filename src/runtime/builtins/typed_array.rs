//! TypedArray and ArrayBuffer constructor stubs.
//! Minimal implementation: returns array-like objects for test262 compatibility.

use crate::runtime::{Heap, Value};

fn typed_array_len(args: &[Value]) -> usize {
    let n = match args.first() {
        Some(Value::Int(x)) => *x as usize,
        Some(Value::Number(x)) if x.is_finite() && *x >= 0.0 => *x as usize,
        Some(Value::Number(x)) if x.is_nan() => 0,
        _ => 0,
    };
    n.min(1_000_000)
}

pub fn int32_array(args: &[Value], heap: &mut Heap) -> Value {
    let len = typed_array_len(args);
    let id = heap.alloc_array();
    for _ in 0..len {
        heap.array_push(id, Value::Int(0));
    }
    Value::Array(id)
}

pub fn uint8_array(args: &[Value], heap: &mut Heap) -> Value {
    let len = typed_array_len(args);
    let id = heap.alloc_array();
    for _ in 0..len {
        heap.array_push(id, Value::Int(0));
    }
    Value::Array(id)
}

pub fn uint8_clamped_array(args: &[Value], heap: &mut Heap) -> Value {
    let len = typed_array_len(args);
    let id = heap.alloc_array();
    for _ in 0..len {
        heap.array_push(id, Value::Int(0));
    }
    Value::Array(id)
}

pub fn array_buffer(args: &[Value], heap: &mut Heap) -> Value {
    let len = typed_array_len(args);
    let id = heap.alloc_object();
    heap.set_prop(id, "byteLength", Value::Int(len as i32));
    Value::Object(id)
}

pub fn data_view(args: &[Value], heap: &mut Heap) -> Result<Value, super::BuiltinError> {
    let _ = (args, heap);
    Err(super::BuiltinError::Throw(Value::String(
        "DataView is not implemented".to_string(),
    )))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn data_view_throws_not_implemented() {
        let mut heap = Heap::new();
        let r = data_view(&[], &mut heap);
        assert!(r.is_err());
        if let Err(super::super::BuiltinError::Throw(Value::String(s))) = r {
            assert!(s.contains("DataView"));
        } else {
            panic!("expected Throw with string");
        }
    }
}
