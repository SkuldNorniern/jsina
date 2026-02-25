//! Function constructor: new Function(arg1, arg2, ..., body) - creates function from string.
//! Uses eval internally. Last argument is body, preceding are param names.

use crate::runtime::{Heap, Value};
use crate::frontend::{check_early_errors, Parser};
use crate::ir::{hir_to_bytecode, script_to_hir};
use crate::vm::{interpret_program_with_heap, Completion, Program};
use super::{to_prop_key, BuiltinError};

pub fn function_constructor(args: &[Value], heap: &mut Heap) -> Result<Value, BuiltinError> {
    if args.is_empty() {
        return Ok(Value::Undefined);
    }
    let body = to_prop_key(args.last().unwrap());
    let params: Vec<String> = args[..args.len().saturating_sub(1)]
        .iter()
        .map(to_prop_key)
        .collect();
    let param_list = params.join(", ");
    let wrapped = format!(
        "function main() {{ return (function({}) {{\n{}\n}}); }}\n",
        param_list, body
    );
    let script = match Parser::new(&wrapped).parse() {
        Ok(s) => s,
        Err(_) => return Err(BuiltinError::Throw(Value::String(
            "SyntaxError: Invalid function body".to_string(),
        ))),
    };
    if check_early_errors(&script).is_err() {
        return Err(BuiltinError::Throw(Value::String(
            "SyntaxError: Invalid function body".to_string(),
        )));
    }
    let funcs = match script_to_hir(&script) {
        Ok(f) => f,
        Err(_) => return Err(BuiltinError::Throw(Value::String(
            "SyntaxError: Invalid function body".to_string(),
        ))),
    };
    let entry = funcs.iter().position(|f| f.name.as_deref() == Some("main"));
    let entry = match entry {
        Some(i) => i,
        None => return Ok(Value::Undefined),
    };
    let chunks: Vec<_> = funcs.iter().map(|f| hir_to_bytecode(f).chunk).collect();
    let global_funcs: Vec<(String, usize)> = funcs
        .iter()
        .enumerate()
        .filter_map(|(i, f)| f.name.as_ref().filter(|n| *n != "__init__").map(|n| (n.clone(), i)))
        .collect();
    let program = Program {
        chunks,
        entry,
        init_entry: None,
        global_funcs,
    };
    match interpret_program_with_heap(&program, heap, false, None, None) {
        Ok(Completion::Return(v)) => Ok(v),
        Ok(Completion::Throw(v)) => Err(BuiltinError::Throw(v)),
        Ok(Completion::Normal(v)) => Ok(v),
        Err(e) => Err(BuiltinError::Throw(Value::String(e.to_string()))),
    }
}
