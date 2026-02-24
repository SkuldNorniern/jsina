use super::{number_to_value, to_number};
use crate::runtime::{Heap, Value};

pub fn number(args: &[Value], _heap: &mut Heap) -> Value {
    let n = args.first().map(to_number).unwrap_or(f64::NAN);
    number_to_value(n)
}
