use crate::diagnostics::Span;
use crate::frontend::ast::*;
use crate::frontend::token_type::{Token, TokenType};
use crate::frontend::Lexer;
use std::iter::Peekable;
use std::vec::IntoIter;

const MAX_RECURSION: u32 = 256;

#[derive(Debug)]
pub struct ParseError {
    pub code: String,
    pub message: String,
    pub span: Option<Span>,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let loc = self.span.map(|s| format!(" at {}", s)).unwrap_or_default();
        write!(f, "{} ({}){}", self.message, self.code, loc)
    }
}

impl std::error::Error for ParseError {}

fn binary_op_precedence(op: BinaryOp) -> u8 {
    match op {
        BinaryOp::Pow => 14,
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 12,
        BinaryOp::Add | BinaryOp::Sub => 11,
        BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => 9,
        BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::StrictEq | BinaryOp::StrictNotEq => 8,
        BinaryOp::LogicalAnd => 4,
        BinaryOp::LogicalOr => 3,
    }
}

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
    current: Option<Token>,
    next_id: u32,
    recursion_depth: u32,
}

impl Parser {
    pub fn new(source: &str) -> Self {
        let mut lexer = Lexer::new(source.to_string());
        let tokens: Vec<Token> = lexer.tokenize();
        let mut parser = Self {
            tokens: tokens.into_iter().peekable(),
            current: None,
            next_id: 0,
            recursion_depth: 0,
        };
        parser.advance();
        parser
    }

    fn advance(&mut self) {
        self.current = self.tokens.next();
    }

    fn peek(&mut self) -> Option<&TokenType> {
        self.tokens.peek().map(|t| &t.token_type)
    }

    fn current(&self) -> Option<&Token> {
        self.current.as_ref()
    }

    fn next_id(&mut self) -> NodeId {
        let id = NodeId(self.next_id);
        self.next_id += 1;
        id
    }

    fn check_recursion(&mut self) -> Result<(), ParseError> {
        if self.recursion_depth >= MAX_RECURSION {
            return Err(ParseError {
                code: "JSINA-PARSE-001".to_string(),
                message: "parser recursion limit exceeded".to_string(),
                span: self.current().map(|t| t.span),
            });
        }
        self.recursion_depth += 1;
        Ok(())
    }

    fn end_recursion(&mut self) {
        self.recursion_depth = self.recursion_depth.saturating_sub(1);
    }

    fn expect(&mut self, tt: TokenType) -> Result<Token, ParseError> {
        let token = self.current().ok_or_else(|| ParseError {
            code: "JSINA-PARSE-002".to_string(),
            message: format!("unexpected end of input, expected {:?}", tt),
            span: None,
        })?.clone();

        if std::mem::discriminant(&token.token_type) != std::mem::discriminant(&tt) {
            return Err(ParseError {
                code: "JSINA-PARSE-003".to_string(),
                message: format!("unexpected token {:?}, expected {:?}", token.token_type, tt),
                span: Some(token.span),
            });
        }
        self.advance();
        Ok(token)
    }

    fn optional(&mut self, tt: TokenType) -> bool {
        if self.current().map(|t| std::mem::discriminant(&t.token_type) == std::mem::discriminant(&tt)).unwrap_or(false) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn parse(&mut self) -> Result<Script, ParseError> {
        let start_span = self.current().map(|t| t.span).unwrap_or(Span::point(crate::diagnostics::Position::start()));
        let id = self.next_id();

        let mut body = Vec::new();
        while !matches!(self.peek(), Some(TokenType::Eof) | None) {
            body.push(self.parse_statement()?);
        }

        let end_span = self.current().map(|t| t.span).unwrap_or(start_span);
        let span = start_span.merge(end_span);

        Ok(Script {
            id,
            span,
            body,
        })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        self.check_recursion()?;
        let stmt = self.parse_statement_inner()?;
        self.end_recursion();
        Ok(stmt)
    }

    fn parse_statement_inner(&mut self) -> Result<Statement, ParseError> {
        let token = self.current().ok_or_else(|| ParseError {
            code: "JSINA-PARSE-004".to_string(),
            message: "unexpected end of input".to_string(),
            span: None,
        })?.clone();

        match &token.token_type {
            TokenType::Function => self.parse_function_decl(),
            TokenType::Return => self.parse_return(),
            TokenType::If => self.parse_if(),
            TokenType::While => self.parse_while(),
            TokenType::For => self.parse_for(),
            TokenType::Var => self.parse_var_decl(),
            TokenType::Let => self.parse_let_decl(),
            TokenType::Const => self.parse_const_decl(),
            TokenType::LeftBrace => self.parse_block(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_block(&mut self) -> Result<Statement, ParseError> {
        let start_span = self.expect(TokenType::LeftBrace)?.span;
        let id = self.next_id();

        let mut body = Vec::new();
        while !matches!(self.peek(), Some(TokenType::RightBrace) | Some(TokenType::Eof) | None) {
            body.push(self.parse_statement()?);
        }

        let end_token = self.expect(TokenType::RightBrace)?;
        let span = start_span.merge(end_token.span);

        Ok(Statement::Block(BlockStmt { id, span, body }))
    }

    fn parse_function_decl(&mut self) -> Result<Statement, ParseError> {
        let start_span = self.expect(TokenType::Function)?.span;
        let id = self.next_id();

        let name_tok = self.expect(TokenType::Identifier)?;
        let name = name_tok.lexeme.clone();

        self.expect(TokenType::LeftParen)?;
        let params = self.parse_params()?;
        self.expect(TokenType::RightParen)?;

        let body = Box::new(self.parse_block()?);
        let span = start_span.merge(body.span());

        Ok(Statement::FunctionDecl(FunctionDeclStmt {
            id,
            span,
            name,
            params,
            body,
        }))
    }

    fn parse_params(&mut self) -> Result<Vec<String>, ParseError> {
        let mut params = Vec::new();
        while matches!(self.peek(), Some(TokenType::Identifier)) {
            let token = self.expect(TokenType::Identifier)?;
            params.push(token.lexeme);
            if !self.optional(TokenType::Comma) {
                break;
            }
        }
        Ok(params)
    }

    fn parse_return(&mut self) -> Result<Statement, ParseError> {
        let start_span = self.expect(TokenType::Return)?.span;
        let id = self.next_id();

        let argument = if matches!(self.current().map(|t| &t.token_type), Some(TokenType::Semicolon) | Some(TokenType::RightBrace))
            || self.current().is_none()
        {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.optional(TokenType::Semicolon);

        let end_span = argument.as_ref().map(|e| e.span()).unwrap_or(start_span);
        let span = start_span.merge(end_span);

        Ok(Statement::Return(ReturnStmt {
            id,
            span,
            argument,
        }))
    }

    fn parse_if(&mut self) -> Result<Statement, ParseError> {
        let start_span = self.expect(TokenType::If)?.span;
        let id = self.next_id();

        self.expect(TokenType::LeftParen)?;
        let condition = Box::new(self.parse_expression()?);
        self.expect(TokenType::RightParen)?;

        let then_branch = Box::new(self.parse_statement()?);

        let else_branch = if self.optional(TokenType::Else) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        let end_span = else_branch.as_ref().map(|s| s.span()).unwrap_or_else(|| then_branch.span());
        let span = start_span.merge(end_span);

        Ok(Statement::If(IfStmt {
            id,
            span,
            condition,
            then_branch,
            else_branch,
        }))
    }

    fn parse_while(&mut self) -> Result<Statement, ParseError> {
        let start_span = self.expect(TokenType::While)?.span;
        let id = self.next_id();

        self.expect(TokenType::LeftParen)?;
        let condition = Box::new(self.parse_expression()?);
        self.expect(TokenType::RightParen)?;

        let body = Box::new(self.parse_statement()?);
        let span = start_span.merge(body.span());

        Ok(Statement::While(WhileStmt {
            id,
            span,
            condition,
            body,
        }))
    }

    fn parse_for(&mut self) -> Result<Statement, ParseError> {
        let start_span = self.expect(TokenType::For)?.span;
        let id = self.next_id();

        self.expect(TokenType::LeftParen)?;

        let init = if matches!(self.peek(), Some(TokenType::Semicolon)) {
            self.advance();
            None
        } else if matches!(self.peek(), Some(TokenType::Var) | Some(TokenType::Let) | Some(TokenType::Const)) {
            let stmt = self.parse_statement()?;
            self.optional(TokenType::Semicolon);
            Some(Box::new(stmt))
        } else {
            let expr = self.parse_expression()?;
            let span = expr.span();
            self.expect(TokenType::Semicolon)?;
            Some(Box::new(Statement::Expression(ExpressionStmt {
                id: self.next_id(),
                span,
                expression: Box::new(expr),
            })))
        };

        let condition = if matches!(self.peek(), Some(TokenType::Semicolon)) {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.expect(TokenType::Semicolon)?;

        let update = if matches!(self.peek(), Some(TokenType::RightParen)) {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.expect(TokenType::RightParen)?;

        let body = Box::new(self.parse_statement()?);
        let span = start_span.merge(body.span());

        Ok(Statement::For(ForStmt {
            id,
            span,
            init,
            condition,
            update,
            body,
        }))
    }

    fn parse_var_decl(&mut self) -> Result<Statement, ParseError> {
        let start_span = self.expect(TokenType::Var)?.span;
        let id = self.next_id();
        let declarations = self.parse_declarators()?;
        self.optional(TokenType::Semicolon);
        let span = declarations.last().map(|d| start_span.merge(d.span)).unwrap_or(start_span);
        Ok(Statement::VarDecl(VarDeclStmt { id, span, declarations }))
    }

    fn parse_let_decl(&mut self) -> Result<Statement, ParseError> {
        let start_span = self.expect(TokenType::Let)?.span;
        let id = self.next_id();
        let declarations = self.parse_declarators()?;
        self.optional(TokenType::Semicolon);
        let span = declarations.last().map(|d| start_span.merge(d.span)).unwrap_or(start_span);
        Ok(Statement::LetDecl(LetDeclStmt { id, span, declarations }))
    }

    fn parse_const_decl(&mut self) -> Result<Statement, ParseError> {
        let start_span = self.expect(TokenType::Const)?.span;
        let id = self.next_id();
        let declarations = self.parse_declarators()?;
        self.expect(TokenType::Semicolon)?;
        let span = declarations.last().map(|d| start_span.merge(d.span)).unwrap_or(start_span);
        Ok(Statement::ConstDecl(ConstDeclStmt { id, span, declarations }))
    }

    fn parse_declarators(&mut self) -> Result<Vec<VarDeclarator>, ParseError> {
        let mut decls = Vec::new();
        loop {
            let name_tok = self.expect(TokenType::Identifier)?;
            let name = name_tok.lexeme.clone();
            let decl_span = name_tok.span;

            let init = if self.optional(TokenType::Assign) {
                Some(Box::new(self.parse_expression()?))
            } else {
                None
            };

            let decl_span = init.as_ref().map(|e| decl_span.merge(e.span())).unwrap_or(decl_span);

            decls.push(VarDeclarator {
                id: self.next_id(),
                span: decl_span,
                name,
                init,
            });

            if !self.optional(TokenType::Comma) {
                break;
            }
        }
        Ok(decls)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.parse_expression()?;
        self.optional(TokenType::Semicolon);
        Ok(Statement::Expression(ExpressionStmt {
            id: self.next_id(),
            span: expr.span(),
            expression: Box::new(expr),
        }))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_expression_prec(0)
    }

    fn parse_expression_prec(&mut self, min_prec: u8) -> Result<Expression, ParseError> {
        self.check_recursion()?;

        let mut left = self.parse_unary()?;

        loop {
            let op = match self.peek() {
                Some(TokenType::Plus) => BinaryOp::Add,
                Some(TokenType::Minus) => BinaryOp::Sub,
                Some(TokenType::Multiply) => BinaryOp::Mul,
                Some(TokenType::Divide) => BinaryOp::Div,
                Some(TokenType::Modulo) => BinaryOp::Mod,
                Some(TokenType::Exponent) => BinaryOp::Pow,
                Some(TokenType::Equal) => BinaryOp::Eq,
                Some(TokenType::NotEqual) => BinaryOp::NotEq,
                Some(TokenType::StrictEqual) => BinaryOp::StrictEq,
                Some(TokenType::StrictNotEqual) => BinaryOp::StrictNotEq,
                Some(TokenType::LessThan) => BinaryOp::Lt,
                Some(TokenType::LessEqual) => BinaryOp::Lte,
                Some(TokenType::GreaterThan) => BinaryOp::Gt,
                Some(TokenType::GreaterEqual) => BinaryOp::Gte,
                Some(TokenType::LogicalAnd) => BinaryOp::LogicalAnd,
                Some(TokenType::LogicalOr) => BinaryOp::LogicalOr,
                Some(TokenType::Assign) => {
                    self.end_recursion();
                    let left_span = left.span();
                    self.advance();
                    let right = self.parse_expression_prec(0)?;
                    let span = left_span.merge(right.span());
                    return Ok(Expression::Assign(AssignExpr {
                        id: self.next_id(),
                        span,
                        left: Box::new(left),
                        right: Box::new(right),
                    }));
                }
                _ => break,
            };

            let prec = binary_op_precedence(op);

            if prec < min_prec {
                break;
            }

            let next_min = if matches!(op, BinaryOp::Pow) { prec } else { prec + 1 };
            self.advance();
            let right = self.parse_expression_prec(next_min)?;
            let left_span = left.span();
            let right_span = right.span();
            let span = left_span.merge(right_span);

            left = Expression::Binary(BinaryExpr {
                id: self.next_id(),
                span,
                op,
                left: Box::new(left),
                right: Box::new(right),
            });
        }

        self.end_recursion();
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expression, ParseError> {
        self.check_recursion()?;

        let token = self.current().cloned();
        if let Some(ref t) = token {
            let (op, span) = match &t.token_type {
                TokenType::Minus => {
                    self.advance();
                    (UnaryOp::Minus, t.span)
                }
                TokenType::Plus => {
                    self.advance();
                    (UnaryOp::Plus, t.span)
                }
                TokenType::LogicalNot => {
                    self.advance();
                    (UnaryOp::LogicalNot, t.span)
                }
                _ => {
                    self.end_recursion();
                    return self.parse_postfix();
                }
            };

            let arg = self.parse_unary()?;
            let full_span = span.merge(arg.span());
            self.end_recursion();
            return Ok(Expression::Unary(UnaryExpr {
                id: self.next_id(),
                span: full_span,
                op,
                argument: Box::new(arg),
            }));
        }

        self.end_recursion();
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            if matches!(self.peek(), Some(TokenType::LeftParen)) {
                let start_span = expr.span();
                self.advance();
                let mut args = Vec::new();
                while !matches!(self.peek(), Some(TokenType::RightParen) | Some(TokenType::Eof) | None) {
                    args.push(self.parse_expression()?);
                    if !self.optional(TokenType::Comma) {
                        break;
                    }
                }
                let end_token = self.expect(TokenType::RightParen)?;
                let span = start_span.merge(end_token.span);
                expr = Expression::Call(CallExpr {
                    id: self.next_id(),
                    span,
                    callee: Box::new(expr),
                    args,
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        let token = self.current().ok_or_else(|| ParseError {
            code: "JSINA-PARSE-005".to_string(),
            message: "unexpected end of input in expression".to_string(),
            span: None,
        })?.clone();

        let (expr, _span) = match &token.token_type {
            TokenType::Number => {
                let span = token.span;
                self.advance();
                let val = if token.lexeme.contains('.') || token.lexeme.contains('e') || token.lexeme.contains('E') {
                    LiteralValue::Number(token.lexeme.parse().unwrap_or(0.0))
                } else {
                    LiteralValue::Int(token.lexeme.parse().unwrap_or(0))
                };
                (Expression::Literal(LiteralExpr { id: self.next_id(), span, value: val }), span)
            }
            TokenType::String => {
                let span = token.span;
                self.advance();
                let s = token.lexeme;
                let s = s.strip_prefix('"').and_then(|s| s.strip_suffix('"'))
                    .or_else(|| s.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')))
                    .unwrap_or(&s)
                    .to_string();
                (Expression::Literal(LiteralExpr { id: self.next_id(), span, value: LiteralValue::String(s) }), span)
            }
            TokenType::True => {
                let span = token.span;
                self.advance();
                (Expression::Literal(LiteralExpr { id: self.next_id(), span, value: LiteralValue::True }), span)
            }
            TokenType::False => {
                let span = token.span;
                self.advance();
                (Expression::Literal(LiteralExpr { id: self.next_id(), span, value: LiteralValue::False }), span)
            }
            TokenType::Null => {
                let span = token.span;
                self.advance();
                (Expression::Literal(LiteralExpr { id: self.next_id(), span, value: LiteralValue::Null }), span)
            }
            TokenType::Identifier => {
                let span = token.span;
                let name = token.lexeme.clone();
                self.advance();
                (Expression::Identifier(IdentifierExpr { id: self.next_id(), span, name }), span)
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.parse_expression()?;
                let end_tok = self.expect(TokenType::RightParen)?;
                let span = token.span.merge(end_tok.span);
                (expr, span)
            }
            _ => {
                return Err(ParseError {
                    code: "JSINA-PARSE-006".to_string(),
                    message: format!("unexpected token in expression: {:?}", token.token_type),
                    span: Some(token.span),
                });
            }
        };

        Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_function_return() {
        let mut parser = Parser::new("function main() { return 50; }");
        let script = parser.parse().expect("parse");
        assert_eq!(script.body.len(), 1);
        if let Statement::FunctionDecl(f) = &script.body[0] {
            if let Statement::Block(b) = &*f.body {
                assert_eq!(b.body.len(), 1, "block should have 1 stmt, got {:?}", b.body);
                if let Statement::Return(r) = &b.body[0] {
                    assert!(r.argument.is_some(), "return should have argument");
                }
            }
        }
    }

    #[test]
    fn parse_empty_block() {
        let mut parser = Parser::new("function f() {}");
        let script = parser.parse().expect("parse");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_default_source() {
        let source = "function main() { return 50; }";
        let mut parser = Parser::new(source);
        let script = parser.parse().expect("parse");
        assert_eq!(script.body.len(), 1);
        if let Statement::FunctionDecl(f) = &script.body[0] {
            if let Statement::Block(b) = &*f.body {
                assert_eq!(b.body.len(), 1, "block body len={} {:?}", b.body.len(), b.body);
            }
        }
    }
}
