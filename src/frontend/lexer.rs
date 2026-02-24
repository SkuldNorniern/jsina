use crate::diagnostics::{Position, Span};
use super::token_type::{Token, TokenType};
use super::trie::Trie;

pub struct Lexer<'a> {
    source: String,
    position: Position,
    current_char: Option<char>,
    keywords_trie: &'a Trie,
    prev_token_type: Option<TokenType>,
}

impl Lexer<'_> {
    pub fn new(source: String) -> Self {
        let position = Position::start();
        let first_char = source.chars().next();

        Self {
            source,
            position,
            current_char: first_char,
            keywords_trie: Trie::js_trie(),
            prev_token_type: None,
        }
    }

    pub fn position(&self) -> Position {
        self.position
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.position.byte_offset + 1..)
            .and_then(|s| s.chars().next())
    }

    fn advance(&mut self) {
        if let Some(ch) = self.current_char {
            self.position.advance(ch);
            self.current_char = self.source.get(self.position.byte_offset..)
                .and_then(|s| s.chars().next());
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char {
            if ch.is_ascii_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn scan_identifier(&mut self) -> Token {
        let start_pos = self.position;
        let mut lexeme = String::new();

        while let Some(ch) = self.current_char {
            if ch.is_alphanumeric() || ch == '_' || ch == '$' {
                lexeme.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        let span = Span::from_text(start_pos, &lexeme);
        let token_type = if self.keywords_trie.is_keyword(&lexeme) {
            match lexeme.as_str() {
                "break" => TokenType::Break,
                "case" => TokenType::Case,
                "catch" => TokenType::Catch,
                "class" => TokenType::Class,
                "const" => TokenType::Const,
                "continue" => TokenType::Continue,
                "debugger" => TokenType::Debugger,
                "default" => TokenType::Default,
                "delete" => TokenType::Delete,
                "do" => TokenType::Do,
                "else" => TokenType::Else,
                "export" => TokenType::Export,
                "extends" => TokenType::Extends,
                "finally" => TokenType::Finally,
                "for" => TokenType::For,
                "function" => TokenType::Function,
                "if" => TokenType::If,
                "import" => TokenType::Import,
                "in" => TokenType::In,
                "instanceof" => TokenType::Instanceof,
                "of" => TokenType::Of,
                "let" => TokenType::Let,
                "new" => TokenType::New,
                "return" => TokenType::Return,
                "super" => TokenType::Super,
                "switch" => TokenType::Switch,
                "this" => TokenType::This,
                "throw" => TokenType::Throw,
                "try" => TokenType::Try,
                "typeof" => TokenType::Typeof,
                "var" => TokenType::Var,
                "void" => TokenType::Void,
                "while" => TokenType::While,
                "with" => TokenType::With,
                "yield" => TokenType::Yield,
                "null" => TokenType::Null,
                "true" => TokenType::True,
                "false" => TokenType::False,
                _ => TokenType::Identifier,
            }
        } else {
            TokenType::Identifier
        };

        Token::new(token_type, lexeme, span)
    }

    fn scan_number(&mut self) -> Token {
        let start_pos = self.position;
        let mut lexeme = String::new();

        if self.current_char == Some('0') {
            lexeme.push('0');
            self.advance();

            if let Some(ch) = self.current_char {
                match ch {
                    'x' | 'X' => {
                        lexeme.push(ch);
                        self.advance();
                        while let Some(ch) = self.current_char {
                            if ch.is_ascii_hexdigit() {
                                lexeme.push(ch);
                                self.advance();
                            } else { break; }
                        }
                    }
                    'b' | 'B' => {
                        lexeme.push(ch);
                        self.advance();
                        while let Some(ch) = self.current_char {
                            if ch == '0' || ch == '1' {
                                lexeme.push(ch);
                                self.advance();
                            } else { break; }
                        }
                    }
                    'o' | 'O' => {
                        lexeme.push(ch);
                        self.advance();
                        while let Some(ch) = self.current_char {
                            if ch.is_ascii_digit() && ch < '8' {
                                lexeme.push(ch);
                                self.advance();
                            } else { break; }
                        }
                    }
                    _ => {
                        while let Some(ch) = self.current_char {
                            if ch.is_ascii_digit() {
                                lexeme.push(ch);
                                self.advance();
                            } else { break; }
                        }
                    }
                }
            }
        } else {
            while let Some(ch) = self.current_char {
                if ch.is_ascii_digit() {
                    lexeme.push(ch);
                    self.advance();
                } else { break; }
            }
        }

        if self.current_char == Some('.') {
            lexeme.push('.');
            self.advance();
            while let Some(ch) = self.current_char {
                if ch.is_ascii_digit() {
                    lexeme.push(ch);
                    self.advance();
                } else { break; }
            }
        }

        if let Some(ch) = self.current_char && (ch == 'e' || ch == 'E') {
            lexeme.push(ch);
            self.advance();
            if let Some(ch) = self.current_char && (ch == '+' || ch == '-') {
                lexeme.push(ch);
                self.advance();
            }
            while let Some(ch) = self.current_char {
                if ch.is_ascii_digit() {
                    lexeme.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
        }

        let span = Span::from_text(start_pos, &lexeme);
        Token::new(TokenType::Number, lexeme, span)
    }

    fn scan_string(&mut self) -> Token {
        let start_pos = self.position;
        let quote = self.current_char.unwrap();
        let mut lexeme = String::new();
        lexeme.push(quote);
        self.advance();

        while let Some(ch) = self.current_char {
            if ch == quote {
                lexeme.push(ch);
                self.advance();
                break;
            } else if ch == '\\' {
                lexeme.push(ch);
                self.advance();
                if let Some(ch) = self.current_char {
                    lexeme.push(ch);
                    self.advance();
                }
            } else if ch == '\n' {
                break;
            } else {
                lexeme.push(ch);
                self.advance();
            }
        }

        let span = Span::from_text(start_pos, &lexeme);
        Token::new(TokenType::String, lexeme, span)
    }

    fn scan_template_literal(&mut self) -> Token {
        let start_pos = self.position;
        let mut lexeme = String::new();
        lexeme.push(self.current_char.unwrap());
        self.advance();

        while let Some(ch) = self.current_char {
            if ch == '`' {
                lexeme.push(ch);
                self.advance();
                break;
            } else if ch == '\\' {
                lexeme.push(ch);
                self.advance();
                if let Some(ch) = self.current_char {
                    lexeme.push(ch);
                    self.advance();
                }
            } else if ch == '$' && self.peek() == Some('{') {
                lexeme.push(ch);
                self.advance();
                lexeme.push(self.current_char.unwrap());
                self.advance();
                let mut brace_count = 1;
                while let Some(ch) = self.current_char {
                    lexeme.push(ch);
                    self.advance();
                    if ch == '{' { brace_count += 1; } else if ch == '}' {
                        brace_count -= 1;
                        if brace_count == 0 { break; }
                    }
                }
            } else {
                lexeme.push(ch);
                self.advance();
            }
        }

        let span = Span::from_text(start_pos, &lexeme);
        Token::new(TokenType::TemplateLiteral, lexeme, span)
    }

    fn prev_could_end_expression(&self) -> bool {
        use TokenType::*;
        matches!(
            self.prev_token_type.as_ref(),
            Some(RightParen) | Some(RightBracket) | Some(RightBrace)
                | Some(Identifier) | Some(Number) | Some(String)
                | Some(RegExpLiteral { .. }) | Some(True) | Some(False)
                | Some(Null) | Some(This) | Some(TemplateLiteral)
        )
    }

    fn scan_regex_literal(&mut self) -> Token {
        let start_pos = self.position;
        self.advance();
        let mut pattern = String::new();
        let mut in_class = false;
        while let Some(ch) = self.current_char {
            if ch == '\\' {
                pattern.push(ch);
                self.advance();
                if let Some(esc) = self.current_char {
                    pattern.push(esc);
                    self.advance();
                }
            } else if ch == '[' {
                in_class = true;
                pattern.push(ch);
                self.advance();
            } else if ch == ']' {
                in_class = false;
                pattern.push(ch);
                self.advance();
            } else if ch == '/' && !in_class {
                self.advance();
                break;
            } else {
                pattern.push(ch);
                self.advance();
            }
        }
        let mut flags = String::new();
        while let Some(ch) = self.current_char {
            if matches!(ch, 'g' | 'i' | 'm' | 's' | 'u' | 'y') {
                flags.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        let lexeme = format!("/{}/{}", pattern, flags);
        let span = Span::from_text(start_pos, &lexeme);
        Token::new(
            TokenType::RegExpLiteral {
                pattern,
                flags,
            },
            lexeme,
            span,
        )
    }

    fn scan_comment(&mut self) -> bool {
        if self.current_char != Some('/') { return false; }
        let start_pos = self.position;
        self.advance();

        if self.current_char == Some('/') {
            self.advance();
            while let Some(ch) = self.current_char {
                if ch == '\n' { break; }
                self.advance();
            }
            true
        } else if self.current_char == Some('*') {
            self.advance();
            while let Some(ch) = self.current_char {
                if ch == '*' && self.peek() == Some('/') {
                    self.advance();
                    self.advance();
                    break;
                }
                self.advance();
            }
            true
        } else {
            self.position = start_pos;
            self.current_char = Some('/');
            false
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if self.current_char.is_none() {
            let span = Span::point(self.position);
            return Token::new(TokenType::Eof, String::new(), span);
        }

        if self.scan_comment() {
            return self.next_token();
        }

        let start_pos = self.position;
        let ch = self.current_char.unwrap();

        match ch {
            '(' => {
                self.advance();
                Token::new(TokenType::LeftParen, "(".to_string(), Span::from_text(start_pos, "("))
            }
            ')' => {
                self.advance();
                Token::new(TokenType::RightParen, ")".to_string(), Span::from_text(start_pos, ")"))
            }
            '[' => {
                self.advance();
                Token::new(TokenType::LeftBracket, "[".to_string(), Span::from_text(start_pos, "["))
            }
            ']' => {
                self.advance();
                Token::new(TokenType::RightBracket, "]".to_string(), Span::from_text(start_pos, "]"))
            }
            '{' => {
                self.advance();
                Token::new(TokenType::LeftBrace, "{".to_string(), Span::from_text(start_pos, "{"))
            }
            '}' => {
                self.advance();
                Token::new(TokenType::RightBrace, "}".to_string(), Span::from_text(start_pos, "}"))
            }
            ';' => {
                self.advance();
                Token::new(TokenType::Semicolon, ";".to_string(), Span::from_text(start_pos, ";"))
            }
            ',' => {
                self.advance();
                Token::new(TokenType::Comma, ",".to_string(), Span::from_text(start_pos, ","))
            }
            '.' => {
                self.advance();
                Token::new(TokenType::Dot, ".".to_string(), Span::from_text(start_pos, "."))
            }
            '~' => {
                self.advance();
                Token::new(TokenType::BitwiseNot, "~".to_string(), Span::from_text(start_pos, "~"))
            }
            '"' | '\'' => self.scan_string(),
            '`' => self.scan_template_literal(),
            '0'..='9' => self.scan_number(),
            '/' => {
                if let Some((token_type, length)) = self.keywords_trie.find_longest_match(&self.source, self.position.byte_offset) {
                    if matches!(token_type, TokenType::DivideAssign) {
                        let lexeme = self.source[self.position.byte_offset..self.position.byte_offset + length].to_string();
                        let mut end_pos = start_pos;
                        end_pos.advance_by(&lexeme);
                        let span = Span::new(start_pos, end_pos);
                        for _ in 0..length {
                            self.advance();
                        }
                        Token::new(token_type, lexeme, span)
                    } else if self.prev_could_end_expression() {
                        self.advance();
                        Token::new(TokenType::Divide, "/".to_string(), Span::from_text(start_pos, "/"))
                    } else {
                        self.scan_regex_literal()
                    }
                } else if self.prev_could_end_expression() {
                    self.advance();
                    Token::new(TokenType::Divide, "/".to_string(), Span::from_text(start_pos, "/"))
                } else {
                    self.scan_regex_literal()
                }
            }
            'a'..='z' | 'A'..='Z' | '_' | '$' => self.scan_identifier(),
            _ => {
                if let Some((token_type, length)) = self.keywords_trie.find_longest_match(&self.source, self.position.byte_offset) {
                    let lexeme = self.source[self.position.byte_offset..self.position.byte_offset + length].to_string();
                    let mut end_pos = start_pos;
                    end_pos.advance_by(&lexeme);
                    let span = Span::new(start_pos, end_pos);
                    for _ in 0..length {
                        self.advance();
                    }
                    Token::new(token_type, lexeme, span)
                } else {
                    self.advance();
                    let span = Span::from_text(start_pos, &ch.to_string());
                    Token::new(TokenType::Error(format!("Unexpected character: {}", ch)), ch.to_string(), span)
                }
            }
        }
    }

    pub fn peek_token(&mut self) -> Token {
        let saved_position = self.position;
        let saved_char = self.current_char;
        let token = self.next_token();
        self.position = saved_position;
        self.current_char = saved_char;
        token
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            if token.token_type == TokenType::Eof { break; }
            self.prev_token_type = Some(token.token_type.clone());
            tokens.push(token);
        }
        tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_keywords() {
        let source = "function var const if else return".to_string();
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].token_type, TokenType::Function);
        assert_eq!(tokens[1].token_type, TokenType::Var);
        assert_eq!(tokens[2].token_type, TokenType::Const);
        assert_eq!(tokens[3].token_type, TokenType::If);
        assert_eq!(tokens[4].token_type, TokenType::Else);
        assert_eq!(tokens[5].token_type, TokenType::Return);
    }

    #[test]
    fn lexer_operators() {
        let source = "=== !== == != && || ??".to_string();
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 7);
        assert_eq!(tokens[0].token_type, TokenType::StrictEqual);
        assert_eq!(tokens[1].token_type, TokenType::StrictNotEqual);
        assert_eq!(tokens[2].token_type, TokenType::Equal);
        assert_eq!(tokens[3].token_type, TokenType::NotEqual);
        assert_eq!(tokens[4].token_type, TokenType::LogicalAnd);
        assert_eq!(tokens[5].token_type, TokenType::LogicalOr);
        assert_eq!(tokens[6].token_type, TokenType::NullishCoalescing);
    }

    #[test]
    fn lexer_numbers() {
        let source = "42 3.14 0x10 0b1010 0o755".to_string();
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].token_type, TokenType::Number);
        assert_eq!(tokens[1].token_type, TokenType::Number);
        assert_eq!(tokens[2].token_type, TokenType::Number);
        assert_eq!(tokens[3].token_type, TokenType::Number);
        assert_eq!(tokens[4].token_type, TokenType::Number);
    }

    #[test]
    fn lexer_strings() {
        let source = r#""hello" 'world' `template ${value}`"#.to_string();
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].token_type, TokenType::String);
        assert_eq!(tokens[1].token_type, TokenType::String);
        assert_eq!(tokens[2].token_type, TokenType::TemplateLiteral);
    }

    #[test]
    fn lexer_complex_expression() {
        let source = "function add(a, b) { return a + b; }".to_string();
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 14);
        assert_eq!(tokens[0].token_type, TokenType::Function);
        assert_eq!(tokens[1].token_type, TokenType::Identifier);
        assert_eq!(tokens[2].token_type, TokenType::LeftParen);
        assert_eq!(tokens[3].token_type, TokenType::Identifier);
        assert_eq!(tokens[4].token_type, TokenType::Comma);
        assert_eq!(tokens[5].token_type, TokenType::Identifier);
        assert_eq!(tokens[6].token_type, TokenType::RightParen);
        assert_eq!(tokens[7].token_type, TokenType::LeftBrace);
        assert_eq!(tokens[8].token_type, TokenType::Return);
        assert_eq!(tokens[9].token_type, TokenType::Identifier);
        assert_eq!(tokens[10].token_type, TokenType::Plus);
        assert_eq!(tokens[11].token_type, TokenType::Identifier);
        assert_eq!(tokens[12].token_type, TokenType::Semicolon);
        assert_eq!(tokens[13].token_type, TokenType::RightBrace);
    }

    #[test]
    fn lexer_position_tracking() {
        let source = "function\n  test".to_string();
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].span.start.line, 1);
        assert_eq!(tokens[1].span.start.line, 2);
    }
}
