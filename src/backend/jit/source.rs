use crate::frontend::{Lexer, TokenType};

use super::error::BackendError;
use super::runtime::{CompiledChunk, build_constant_main_module};

pub fn extract_constant_return(js_source: &str) -> Result<i64, BackendError> {
    let mut lexer = Lexer::new(js_source.to_string());
    let tokens = lexer.tokenize();

    let mut in_main_function = false;
    let mut return_value: Option<i64> = None;

    for (i, token) in tokens.iter().enumerate() {
        match &token.token_type {
            TokenType::Function => {
                if i + 1 < tokens.len()
                    && matches!(tokens[i + 1].token_type, TokenType::Identifier)
                    && tokens[i + 1].lexeme == "main"
                {
                    in_main_function = true;
                }
            }
            TokenType::Return => {
                if in_main_function {
                    for token in tokens.iter().skip(i + 1) {
                        if matches!(token.token_type, TokenType::Number)
                            && let Ok(val) = token.lexeme.parse::<i64>()
                        {
                            return_value = Some(val);
                            break;
                        }
                    }
                }
            }
            TokenType::RightBrace => {
                in_main_function = false;
            }
            _ => {}
        }
    }

    return_value.ok_or_else(|| {
        BackendError::Parse("Could not find main function with numeric return".to_string())
    })
}

pub fn translate_to_lamina_ir(js_source: &str) -> Result<String, BackendError> {
    let value = extract_constant_return(js_source)?;
    Ok(format!(
        concat!(
            "@export\n",
            "fn @main() -> i64 {{\n",
            "  entry:\n",
            "    ret.i64 {}\n",
            "}}\n"
        ),
        value
    ))
}

pub fn run_via_jit(js_source: &str) -> Result<i64, BackendError> {
    let value = extract_constant_return(js_source)?;
    let module = build_constant_main_module(value);
    let compiled = CompiledChunk::from_module(&module)?;
    Ok(compiled.invoke())
}
