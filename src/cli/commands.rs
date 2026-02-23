use crate::driver::Driver;
use crate::frontend::TokenType;

pub fn tokens(source: &str) {
    let tokens = Driver::tokens(source);
    for (i, t) in tokens.iter().enumerate() {
        let tt = match &t.token_type {
            TokenType::Eof => "EOF".to_string(),
            TokenType::Error(msg) => format!("Error({})", msg),
            _ => format!("{:?}", t.token_type),
        };
        println!("{:4}  {}  {:?}", i, tt, t.lexeme);
    }
}
