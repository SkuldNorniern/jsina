use crate::runtime::{Heap, Value};
use std::io::Write;

pub fn print(args: &[Value], _heap: &mut Heap) -> Value {
    let mut out = std::io::stdout();
    for (i, v) in args.iter().enumerate() {
        if i > 0 {
            let _ = write!(out, " ");
        }
        let _ = write!(out, "{}", v);
    }
    let _ = writeln!(out);
    let _ = out.flush();
    Value::Undefined
}
