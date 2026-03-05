//! Function constructor: new Function(arg1, arg2, ..., body) - creates function from string.
//! Uses eval internally. Last argument is body, preceding are param names.
//! Returns Value::DynamicFunction so the created function can be invoked in the caller's context.

use super::{BuiltinContext, BuiltinError, error, html_comments, to_prop_key};
use crate::frontend::{Parser, check_early_errors};
use crate::ir::{hir_to_bytecode, script_to_hir};
use crate::runtime::Value;
use crate::vm::{Completion, Program, interpret_program_with_heap};

fn invalid_function_syntax_error(heap: &mut crate::runtime::Heap) -> BuiltinError {
    BuiltinError::Throw(error::syntax_error(
        &[Value::String("Invalid function body".to_string())],
        heap,
    ))
}

pub fn function_constructor(
    args: &[Value],
    ctx: &mut BuiltinContext,
) -> Result<Value, BuiltinError> {
    let actual: &[Value] = if !args.is_empty() && args.last().is_some_and(|v| v.is_object()) {
        &args[..args.len() - 1]
    } else {
        args
    };
    if actual.is_empty() {
        let wrapped = "function main() { return (function() {}); }\n";
        let script = Parser::new(wrapped)
            .parse()
            .map_err(|_| invalid_function_syntax_error(ctx.heap))?;
        let funcs = script_to_hir(&script).map_err(|_| invalid_function_syntax_error(ctx.heap))?;
        let entry = funcs.iter().position(|f| f.name.as_deref() == Some("main"));
        let entry = match entry {
            Some(i) => i,
            None => return Ok(Value::Undefined),
        };
        let chunks: Vec<_> = funcs.iter().map(|f| hir_to_bytecode(f).chunk).collect();
        let global_funcs: Vec<(String, usize)> = funcs
            .iter()
            .enumerate()
            .filter_map(|(i, f)| {
                f.name
                    .as_ref()
                    .filter(|n| *n != "__init__")
                    .map(|n| (n.clone(), i))
            })
            .collect();
        let program = Program {
            chunks: chunks.clone(),
            entry,
            init_entry: None,
            global_funcs,
        };
        return match interpret_program_with_heap(
            &program, ctx.heap, false, None, false, false, None,
        ) {
            Ok(Completion::Return(v)) => {
                if let Value::Function(inner_idx) = v
                    && let Some(inner_chunk) = program.chunks.get(inner_idx)
                {
                    ctx.heap.dynamic_chunks.push(inner_chunk.clone());
                    ctx.heap.dynamic_captures.push(Vec::new());
                    return Ok(Value::DynamicFunction(ctx.heap.dynamic_chunks.len() - 1));
                }
                Ok(v)
            }
            Ok(Completion::Throw(v)) => Err(BuiltinError::Throw(v)),
            Ok(Completion::Normal(v)) => Ok(v),
            Err(e) => Err(BuiltinError::Throw(Value::String(e.to_string()))),
        };
    }
    let body = html_comments::normalize_function_constructor_source(
        &to_prop_key(actual.last().unwrap()),
        true,
        "//",
    );
    let params: Vec<String> = actual[..actual.len().saturating_sub(1)]
        .iter()
        .map(|v| {
            html_comments::normalize_function_constructor_source(&to_prop_key(v), false, "/**/")
        })
        .collect();
    let param_list = params.join(", ");
    let wrapped = format!(
        "function main() {{ return (function({}) {{\n{}\n}}); }}\n",
        param_list, body
    );
    let script = match Parser::new(&wrapped).parse() {
        Ok(s) => s,
        Err(_) => return Err(invalid_function_syntax_error(ctx.heap)),
    };
    if check_early_errors(&script).is_err() {
        return Err(invalid_function_syntax_error(ctx.heap));
    }
    let funcs = match script_to_hir(&script) {
        Ok(f) => f,
        Err(_) => return Err(invalid_function_syntax_error(ctx.heap)),
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
        .filter_map(|(i, f)| {
            f.name
                .as_ref()
                .filter(|n| *n != "__init__")
                .map(|n| (n.clone(), i))
        })
        .collect();
    let program = Program {
        chunks,
        entry,
        init_entry: None,
        global_funcs,
    };
    match interpret_program_with_heap(&program, ctx.heap, false, None, false, false, None) {
        Ok(Completion::Return(v)) => {
            if let Value::Function(inner_idx) = v
                && let Some(inner_chunk) = program.chunks.get(inner_idx)
            {
                ctx.heap.dynamic_chunks.push(inner_chunk.clone());
                ctx.heap.dynamic_captures.push(Vec::new());
                return Ok(Value::DynamicFunction(ctx.heap.dynamic_chunks.len() - 1));
            }
            Ok(v)
        }
        Ok(Completion::Throw(v)) => Err(BuiltinError::Throw(v)),
        Ok(Completion::Normal(v)) => Ok(v),
        Err(e) => Err(BuiltinError::Throw(Value::String(e.to_string()))),
    }
}
