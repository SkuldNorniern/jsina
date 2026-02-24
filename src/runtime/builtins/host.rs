use crate::host;
use crate::runtime::{Heap, Value};
use std::io::Write;

pub fn print(args: &[Value], _heap: &mut Heap) -> Value {
    let strings: Vec<String> = args.iter().map(|v| v.to_string()).collect();
    let refs: Vec<&str> = strings.iter().map(|s| s.as_str()).collect();
    if !host::print_via_host(&refs) {
        let mut out = std::io::stdout();
        for (i, s) in refs.iter().enumerate() {
            if i > 0 {
                let _ = write!(out, " ");
            }
            let _ = write!(out, "{}", s);
        }
        let _ = writeln!(out);
        let _ = out.flush();
    }
    Value::Undefined
}
