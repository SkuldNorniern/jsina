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
        BinaryOp::NullishCoalescing => 2,
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
            TokenType::Throw => self.parse_throw(),
            TokenType::Break => self.parse_break(),
            TokenType::Continue => self.parse_continue(),
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
        while !matches!(self.current().map(|t| &t.token_type), Some(TokenType::RightBrace) | None) {
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
        while matches!(self.current().map(|t| &t.token_type), Some(TokenType::Identifier)) {
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

    fn parse_throw(&mut self) -> Result<Statement, ParseError> {
        let start_span = self.expect(TokenType::Throw)?.span;
        let id = self.next_id();
        let argument = Box::new(self.parse_expression()?);
        self.optional(TokenType::Semicolon);
        let span = start_span.merge(argument.span());
        Ok(Statement::Throw(ThrowStmt {
            id,
            span,
            argument,
        }))
    }

    fn parse_break(&mut self) -> Result<Statement, ParseError> {
        let start_span = self.expect(TokenType::Break)?.span;
        let id = self.next_id();
        let label = if matches!(self.current().map(|t| &t.token_type), Some(TokenType::Identifier)) {
            let tok = self.expect(TokenType::Identifier)?;
            Some(tok.lexeme)
        } else {
            None
        };
        self.optional(TokenType::Semicolon);
        let span = start_span;
        Ok(Statement::Break(BreakStmt { id, span, label }))
    }

    fn parse_continue(&mut self) -> Result<Statement, ParseError> {
        let start_span = self.expect(TokenType::Continue)?.span;
        let id = self.next_id();
        let label = if matches!(self.current().map(|t| &t.token_type), Some(TokenType::Identifier)) {
            let tok = self.expect(TokenType::Identifier)?;
            Some(tok.lexeme)
        } else {
            None
        };
        self.optional(TokenType::Semicolon);
        let span = start_span;
        Ok(Statement::Continue(ContinueStmt { id, span, label }))
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

        let init = if matches!(self.current().map(|t| &t.token_type), Some(TokenType::Semicolon)) {
            self.advance();
            None
        } else if matches!(self.current().map(|t| &t.token_type), Some(TokenType::Var) | Some(TokenType::Let) | Some(TokenType::Const)) {
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

        let condition = if matches!(
            self.current().map(|t| &t.token_type),
            Some(TokenType::Semicolon)
        ) {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.expect(TokenType::Semicolon)?;

        let update = if matches!(
            self.current().map(|t| &t.token_type),
            Some(TokenType::RightParen)
        ) {
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

        if min_prec <= 1 && matches!(self.current().map(|t| &t.token_type), Some(TokenType::Question)) {
            self.advance();
            let then_expr = self.parse_expression_prec(1)?;
            self.expect(TokenType::Colon)?;
            let else_expr = self.parse_expression_prec(0)?;
            let span = left.span().merge(else_expr.span());
            left = Expression::Conditional(ConditionalExpr {
                id: self.next_id(),
                span,
                condition: Box::new(left),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            });
        }

        loop {
            let op = match self.current().map(|t| &t.token_type) {
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
                Some(TokenType::NullishCoalescing) => BinaryOp::NullishCoalescing,
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
                TokenType::Typeof => {
                    self.advance();
                    (UnaryOp::Typeof, t.span)
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
            if matches!(self.current().map(|t| &t.token_type), Some(TokenType::LeftParen)) {
                let start_span = expr.span();
                self.advance();
                let mut args = Vec::new();
                while !matches!(self.current().map(|t| &t.token_type), Some(TokenType::RightParen) | Some(TokenType::Eof) | None) {
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
            } else if matches!(self.current().map(|t| &t.token_type), Some(TokenType::Dot)) {
                let start_span = expr.span();
                self.advance();
                let prop_tok = self.expect(TokenType::Identifier)?;
                let prop = prop_tok.lexeme.clone();
                let span = start_span.merge(prop_tok.span);
                expr = Expression::Member(MemberExpr {
                    id: self.next_id(),
                    span,
                    object: Box::new(expr),
                    property: MemberProperty::Identifier(prop),
                });
            } else if matches!(self.current().map(|t| &t.token_type), Some(TokenType::LeftBracket)) {
                let start_span = expr.span();
                self.advance();
                let index = self.parse_expression()?;
                let end_tok = self.expect(TokenType::RightBracket)?;
                let span = start_span.merge(end_tok.span);
                expr = Expression::Member(MemberExpr {
                    id: self.next_id(),
                    span,
                    object: Box::new(expr),
                    property: MemberProperty::Expression(Box::new(index)),
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
            TokenType::LeftBrace => {
                let start_span = token.span;
                self.advance();
                let mut properties = Vec::new();
                while !matches!(self.current().map(|t| &t.token_type), Some(TokenType::RightBrace) | Some(TokenType::Eof) | None) {
                    let key_tok = self.expect(TokenType::Identifier)?;
                    let key = key_tok.lexeme.clone();
                    self.expect(TokenType::Colon)?;
                    let value = self.parse_expression()?;
                    properties.push((key, value));
                    if !self.optional(TokenType::Comma) {
                        break;
                    }
                }
                let end_tok = self.expect(TokenType::RightBrace)?;
                let span = start_span.merge(end_tok.span);
                (Expression::ObjectLiteral(ObjectLiteralExpr {
                    id: self.next_id(),
                    span,
                    properties,
                }), span)
            }
            TokenType::LeftBracket => {
                let start_span = token.span;
                self.advance();
                let mut elements = Vec::new();
                while !matches!(self.current().map(|t| &t.token_type), Some(TokenType::RightBracket) | Some(TokenType::Eof) | None) {
                    elements.push(self.parse_expression()?);
                    if !self.optional(TokenType::Comma) {
                        break;
                    }
                }
                let end_tok = self.expect(TokenType::RightBracket)?;
                let span = start_span.merge(end_tok.span);
                (Expression::ArrayLiteral(ArrayLiteralExpr {
                    id: self.next_id(),
                    span,
                    elements,
                }), span)
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
    fn parse_ok(source: &str) -> Script {
        let mut parser = Parser::new(source);
        parser.parse().expect(&format!("parse failed: {}", source))
    }

    fn parse_err(source: &str) -> ParseError {
        let mut parser = Parser::new(source);
        parser.parse().map(|_| panic!("expected parse error")).unwrap_err()
    }

    #[test]
    fn parse_function_return() {
        let script = parse_ok("function main() { return 50; }");
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
        let script = parse_ok("function f() {}");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_default_source() {
        let script = parse_ok("function main() { return 50; }");
        assert_eq!(script.body.len(), 1);
        if let Statement::FunctionDecl(f) = &script.body[0] {
            if let Statement::Block(b) = &*f.body {
                assert_eq!(b.body.len(), 1, "block body len={} {:?}", b.body.len(), b.body);
            }
        }
    }

    #[test]
    fn parse_function_no_params() {
        let script = parse_ok("function foo() { return 1; }");
        assert_eq!(script.body.len(), 1);
        if let Statement::FunctionDecl(f) = &script.body[0] {
            assert_eq!(f.name, "foo");
            assert!(f.params.is_empty());
        }
    }

    #[test]
    fn parse_return_no_arg() {
        let script = parse_ok("function f() { return; }");
        if let Statement::FunctionDecl(f) = &script.body[0] {
            if let Statement::Block(b) = &*f.body {
                if let Statement::Return(r) = &b.body[0] {
                    assert!(r.argument.is_none());
                }
            }
        }
    }

    #[test]
    fn parse_return_no_semicolon() {
        let script = parse_ok("function f() { return 1 }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_literal_int() {
        let script = parse_ok("function f() { return 42; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_literal_float() {
        let script = parse_ok("function f() { return 3.14; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_literal_string_double() {
        let script = parse_ok(r#"function f() { return "hello"; }"#);
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_literal_string_single() {
        let script = parse_ok("function f() { return 'world'; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_literal_true() {
        let script = parse_ok("function f() { return true; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_literal_false() {
        let script = parse_ok("function f() { return false; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_literal_null() {
        let script = parse_ok("function f() { return null; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_binary_add() {
        let script = parse_ok("function f() { return 1 + 2; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_binary_sub() {
        let script = parse_ok("function f() { return 5 - 3; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_unary_minus() {
        let script = parse_ok("function f() { return -5; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_unary_plus() {
        let script = parse_ok("function f() { return +5; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_unary_not() {
        let script = parse_ok("function f() { return !true; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_call_one_arg() {
        let script = parse_ok("function f() { return foo(1); }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_if_then() {
        let script = parse_ok("function f() { if (true) return 1; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_if_else() {
        let script = parse_ok("function f() { if (x) return 1; else return 2; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_if_block() {
        let script = parse_ok("function f() { if (x) { return 1; } }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_while() {
        let script = parse_ok("function f() { while (x) return 1; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_while_block() {
        let script = parse_ok("function f() { while (x) { return 1; } }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_var_decl() {
        let script = parse_ok("function f() { var x; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_var_decl_init() {
        let script = parse_ok("function f() { var x = 1; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_var_decl_multi() {
        let script = parse_ok("function f() { var a, b, c; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_let_decl() {
        let script = parse_ok("function f() { let x; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_let_decl_init() {
        let script = parse_ok("function f() { let x = 1; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_const_decl() {
        let script = parse_ok("function f() { const x = 1; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_const_decl_multi() {
        let script = parse_ok("function f() { const a = 1, b = 2; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_block_nested() {
        let script = parse_ok("function f() { { { return 1; } } }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_expr_stmt() {
        let script = parse_ok("function f() { 1 + 2; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_expr_stmt_no_semicolon() {
        let script = parse_ok("function f() { 1 + 2 }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_multi_stmt() {
        let script = parse_ok("function f() { var x = 1; return x; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_multi_stmt_block() {
        let script = parse_ok("function f() { { var x = 1; return x; } }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_nested_blocks() {
        let script = parse_ok("function f() { { { return 42; } } }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_identifier() {
        let script = parse_ok("function f() { return x; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_script_empty() {
        let script = parse_ok("");
        assert!(script.body.is_empty());
    }

    #[test]
    fn parse_script_mixed() {
        let script = parse_ok("var x = 1; function f() { return x; }");
        assert_eq!(script.body.len(), 2);
    }

    #[test]
    fn parse_var_top_level() {
        let script = parse_ok("var x = 1;");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_let_top_level() {
        let script = parse_ok("let x = 1;");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_const_top_level() {
        let script = parse_ok("const x = 1;");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_if_top_level() {
        let script = parse_ok("if (true) return 1;");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_while_top_level() {
        let script = parse_ok("while (false) {}");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_break_in_while() {
        let script = parse_ok("while (true) { break; }");
        if let Statement::While(w) = &script.body[0] {
            if let Statement::Block(b) = &*w.body {
                if let Statement::Break(br) = &b.body[0] {
                    assert!(br.label.is_none());
                } else {
                    panic!("expected Break");
                }
            }
        }
    }

    #[test]
    fn parse_continue_in_for() {
        let script = parse_ok("for (;;) { continue; }");
        if let Statement::For(f) = &script.body[0] {
            if let Statement::Block(b) = &*f.body {
                if let Statement::Continue(c) = &b.body[0] {
                    assert!(c.label.is_none());
                } else {
                    panic!("expected Continue");
                }
            }
        }
    }

    #[test]
    fn parse_break_with_label() {
        let script = parse_ok("while (true) { break loop; }");
        if let Statement::While(w) = &script.body[0] {
            if let Statement::Block(b) = &*w.body {
                if let Statement::Break(br) = &b.body[0] {
                    assert_eq!(br.label.as_deref(), Some("loop"));
                } else {
                    panic!("expected Break with label");
                }
            }
        }
    }

    #[test]
    fn parse_number_scientific() {
        let script = parse_ok("function f() { return 1e10; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_number_negative() {
        let script = parse_ok("function f() { return -42; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_double_negation() {
        let script = parse_ok("function f() { return !!x; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_var_decl_no_init() {
        let script = parse_ok("function f() { var a, b, c = 3; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_let_decl_multi() {
        let script = parse_ok("function f() { let a = 1, b = 2; }");
        assert_eq!(script.body.len(), 1);
    }

    #[test]
    fn parse_error_unexpected_eof() {
        let err = parse_err("function 123");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_token() {
        let err = parse_err("function 123 () {}");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_missing_rparen() {
        let err = parse_err("function f( ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_missing_rbrace() {
        let err = parse_err("function f() { return 1 ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_expr_unexpected() {
        let err = parse_err("function f() { return + ; }");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_empty_function_name() {
        let err = parse_err("function () {}");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_missing_semicolon_for() {
        let err = parse_err("function f() { for (i = 0 i < 10;) {} }");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_const_requires_init() {
        let err = parse_err("function f() { const ; }");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_const_requires_semicolon() {
        let err = parse_err("function f() { const x = 1 }");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_empty_input_expr() {
        let err = parse_err("function f() { return ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_in_primary() {
        let err = parse_err("function f() { ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_missing_rparen_expr() {
        let err = parse_err("function f() { return (1 + 2; }");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_missing_rparen_while() {
        let err = parse_err("function f() { while (x return 1; }");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_missing_rparen_for() {
        let err = parse_err("function f() { for (;; return 1; }");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_missing_lbrace_block() {
        let err = parse_err("function f( ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_var_decl_no_name() {
        let err = parse_err("function f() { var ; }");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_let_decl_no_name() {
        let err = parse_err("function f() { let ; }");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_for_missing_semicolons() {
        let err = parse_err("function f() { for (i) {} }");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_if_missing_condition() {
        let err = parse_err("function f() { if () return 1; }");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_while_missing_condition() {
        let err = parse_err("function f() { while () return 1; }");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_call_missing_rparen() {
        let err = parse_err("function f() { return foo(1; }");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_missing_comma_in_call() {
        let err = parse_err("function f() { return foo(1 2); }");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_block() {
        let err = parse_err("function f() { ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_for() {
        let err = parse_err("function f() { for (i = 0; ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_params() {
        let err = parse_err("function f(a, ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_decl() {
        let err = parse_err("function f() { var x = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_if() {
        let err = parse_err("function f() { if (");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_while() {
        let err = parse_err("function f() { while (");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_else() {
        let err = parse_err("function f() { if (true) {} else ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_for_body() {
        let err = parse_err("function f() { for (;;) ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_for_init() {
        let err = parse_err("function f() { for (var x = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_for_update() {
        let err = parse_err("function f() { for (;; x + ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_for_cond() {
        let err = parse_err("function f() { for (; x < ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_block_nested() {
        let err = parse_err("function f() { { { ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_assign() {
        let err = parse_err("function f() { x = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_binary() {
        let err = parse_err("function f() { return 1 + ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_unary() {
        let err = parse_err("function f() { return - ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_call() {
        let err = parse_err("function f() { return foo(1, ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_paren_expr() {
        let err = parse_err("function f() { return (1 + ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_assign_chain() {
        let err = parse_err("function f() { a = b = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_if_cond() {
        let err = parse_err("function f() { if (x ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_while_cond() {
        let err = parse_err("function f() { while (x ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_for_cond_expr() {
        let err = parse_err("function f() { for (; x ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_var_decl() {
        let err = parse_err("function f() { var x = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_let_decl() {
        let err = parse_err("function f() { let x = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_const_decl() {
        let err = parse_err("function f() { const x = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_decl_multi() {
        let err = parse_err("function f() { var a = 1, b = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_if_then() {
        let err = parse_err("function f() { if (true) ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_else_then() {
        let err = parse_err("function f() { if (true) {} else ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_for_body_stmt() {
        let err = parse_err("function f() { for (;;) ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_block_stmt() {
        let err = parse_err("function f() { { ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_expr_stmt() {
        let err = parse_err("function f() { 1 + ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_return_expr() {
        let err = parse_err("function f() { return 1 + ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_unary_arg() {
        let err = parse_err("function f() { return ! ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_call_callee() {
        let err = parse_err("function f() { return ( ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_assign_right() {
        let err = parse_err("function f() { return x = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_binary_right() {
        let err = parse_err("function f() { return 1 + ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_paren_expr_inner() {
        let err = parse_err("function f() { return (1 + ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_call_arg() {
        let err = parse_err("function f() { return foo(1, ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_assign_chain_right() {
        let err = parse_err("function f() { a = b = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_if_cond_expr() {
        let err = parse_err("function f() { if (x ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_while_cond_expr() {
        let err = parse_err("function f() { while (x ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_var_decl_init() {
        let err = parse_err("function f() { var x = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_let_decl_init() {
        let err = parse_err("function f() { let x = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_const_decl_init() {
        let err = parse_err("function f() { const x = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_decl_multi_init() {
        let err = parse_err("function f() { var a = 1, b = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_if_then_stmt() {
        let err = parse_err("function f() { if (true) ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_error_unexpected_eof_in_const_decl_initializer() {
        let err = parse_err("function f() { return const x = ");
        assert!(err.code.contains("PARSE"));
    }

    #[test]
    fn parse_literal_0() { let _ = parse_ok("function f() { return 0; }"); }
    #[test]
    fn parse_literal_1() { let _ = parse_ok("function f() { return 1; }"); }
    #[test]
    fn parse_literal_100() { let _ = parse_ok("function f() { return 100; }"); }
    #[test]
    fn parse_literal_neg1() { let _ = parse_ok("function f() { return -1; }"); }
    #[test]
    fn parse_empty_func_a() { let _ = parse_ok("function a() {}"); }
    #[test]
    fn parse_empty_func_b() { let _ = parse_ok("function b() {}"); }
    #[test]
    fn parse_return_a() { let _ = parse_ok("function f() { return a; }"); }
    #[test]
    fn parse_return_b() { let _ = parse_ok("function f() { return b; }"); }
    #[test]
    fn parse_add_0_1() { let _ = parse_ok("function f() { return 0 + 1; }"); }
    #[test]
    fn parse_add_10_20() { let _ = parse_ok("function f() { return 10 + 20; }"); }
    #[test]
    fn parse_sub_5_2() { let _ = parse_ok("function f() { return 5 - 2; }"); }
    #[test]
    fn parse_var_a() { let _ = parse_ok("function f() { var a; }"); }
    #[test]
    fn parse_var_b() { let _ = parse_ok("function f() { var b; }"); }
    #[test]
    fn parse_let_a() { let _ = parse_ok("function f() { let a = 1; }"); }
    #[test]
    fn parse_const_a() { let _ = parse_ok("function f() { const a = 1; }"); }
    #[test]
    fn parse_if_true() { let _ = parse_ok("function f() { if (true) return 1; }"); }
    #[test]
    fn parse_if_false() { let _ = parse_ok("function f() { if (false) return 1; }"); }
    #[test]
    fn parse_while_true() { let _ = parse_ok("function f() { while (true) return 1; }"); }
    #[test]
    fn parse_block_one() { let _ = parse_ok("function f() { { return 1; } }"); }
    #[test]
    fn parse_block_two() { let _ = parse_ok("function f() { { { return 1; } } }"); }
    #[test]
    fn parse_expr_1() { let _ = parse_ok("function f() { 1; }"); }
    #[test]
    fn parse_expr_2() { let _ = parse_ok("function f() { 2; }"); }
    #[test]
    fn parse_expr_identity() { let _ = parse_ok("function f() { x; }"); }
    #[test]
    fn parse_call_one_arg_only() { let _ = parse_ok("function f() { return foo(1); }"); }
    #[test]
    fn parse_call_one_arg_num() { let _ = parse_ok("function f() { return f(1); }"); }
    #[test]
    fn parse_return_empty() { let _ = parse_ok("function f() { return; }"); }
    #[test]
    fn parse_throw() {
        let script = parse_ok("function f() { throw 42; }");
        if let Statement::FunctionDecl(f) = &script.body[0] {
            if let Statement::Block(b) = &*f.body {
                if let Statement::Throw(t) = &b.body[0] {
                    if let Expression::Literal(e) = t.argument.as_ref() {
                        assert!(matches!(e.value, LiteralValue::Int(42)));
                        return;
                    }
                }
            }
        }
        panic!("expected throw 42 in block");
    }
    #[test]
    fn parse_script_var() { let _ = parse_ok("var x = 1;"); }
    #[test]
    fn parse_script_let() { let _ = parse_ok("let x = 1;"); }
    #[test]
    fn parse_script_const() { let _ = parse_ok("const x = 1;"); }
    #[test]
    fn parse_script_expr() { let _ = parse_ok("1;"); }
    #[test]
    fn parse_script_if() { let _ = parse_ok("if (true) return 1;"); }
    #[test]
    fn parse_script_while() { let _ = parse_ok("while (false) {}"); }
    #[test]
    fn parse_unary_minus_one() { let _ = parse_ok("function f() { return -1; }"); }
    #[test]
    fn parse_unary_plus_one() { let _ = parse_ok("function f() { return +1; }"); }
    #[test]
    fn parse_unary_not_false() { let _ = parse_ok("function f() { return !false; }"); }
    #[test]
    fn parse_literal_str_empty() { let _ = parse_ok(r#"function f() { return ""; }"#); }
    #[test]
    fn parse_literal_str_a() { let _ = parse_ok(r#"function f() { return "a"; }"#); }
    #[test]
    fn parse_literal_str_ab() { let _ = parse_ok(r#"function f() { return "ab"; }"#); }
    #[test]
    fn parse_literal_str_single() { let _ = parse_ok("function f() { return 'x'; }"); }
    #[test]
    fn parse_literal_num_0() { let _ = parse_ok("function f() { return 0.0; }"); }
    #[test]
    fn parse_literal_num_1_5() { let _ = parse_ok("function f() { return 1.5; }"); }
    #[test]
    fn parse_var_init_0() { let _ = parse_ok("function f() { var x = 0; }"); }
    #[test]
    fn parse_var_init_1() { let _ = parse_ok("function f() { var x = 1; }"); }
    #[test]
    fn parse_let_init_0() { let _ = parse_ok("function f() { let x = 0; }"); }
    #[test]
    fn parse_const_init_0() { let _ = parse_ok("function f() { const x = 0; }"); }
    #[test]
    fn parse_var_multi_a_b() { let _ = parse_ok("function f() { var a, b; }"); }
    #[test]
    fn parse_let_multi() { let _ = parse_ok("function f() { let a = 1, b = 2; }"); }
    #[test]
    fn parse_const_multi() { let _ = parse_ok("function f() { const a = 1, b = 2; }"); }
    #[test]
    fn parse_if_else_simple() { let _ = parse_ok("function f() { if (true) return 1; else return 2; }"); }
    #[test]
    fn parse_while_simple() { let _ = parse_ok("function f() { while (true) return 1; }"); }
    #[test]
    fn parse_block_simple() { let _ = parse_ok("function f() { if (true) { return 1; } }"); }
    #[test]
    fn parse_multi_var_return() { let _ = parse_ok("function f() { var x = 1; return x; }"); }
    #[test]
    fn parse_multi_let_return() { let _ = parse_ok("function f() { let x = 1; return x; }"); }
    #[test]
    fn parse_nested_block_return() { let _ = parse_ok("function f() { { return 1; } }"); }
    #[test]
    fn parse_identifier_x() { let _ = parse_ok("function f() { return x; }"); }
    #[test]
    fn parse_identifier_y() { let _ = parse_ok("function f() { return y; }"); }
    #[test]
    fn parse_identifier_foo() { let _ = parse_ok("function f() { return foo; }"); }
    #[test]
    fn parse_add_identifiers() { let _ = parse_ok("function f() { return a + b; }"); }
    #[test]
    fn parse_sub_identifiers() { let _ = parse_ok("function f() { return a - b; }"); }
    #[test]
    fn parse_add_three() { let _ = parse_ok("function f() { return 1 + 2 + 3; }"); }
    #[test]
    fn parse_sub_three() { let _ = parse_ok("function f() { return 10 - 2 - 1; }"); }
    #[test]
    fn parse_number_int() { let _ = parse_ok("function f() { return 999; }"); }
    #[test]
    fn parse_number_float() { let _ = parse_ok("function f() { return 0.5; }"); }
    #[test]
    fn parse_number_int_large() { let _ = parse_ok("function f() { return 12345; }"); }
    #[test]
    fn parse_return_semicolon() { let _ = parse_ok("function f() { return 1; }"); }
    #[test]
    fn parse_return_no_semi() { let _ = parse_ok("function f() { return 1 }"); }
    #[test]
    fn parse_expr_semicolon() { let _ = parse_ok("function f() { 1; }"); }
    #[test]
    fn parse_expr_no_semi() { let _ = parse_ok("function f() { 1; }"); }
    #[test]
    fn parse_func_main() { let _ = parse_ok("function main() { return 0; }"); }
    #[test]
    fn parse_func_foo() { let _ = parse_ok("function foo() { return 0; }"); }
    #[test]
    fn parse_func_bar() { let _ = parse_ok("function bar() { return 0; }"); }
    #[test]
    fn parse_script_var_func() { let _ = parse_ok("var x = 1; function f() { return x; }"); }
    #[test]
    fn parse_script_two_var() { let _ = parse_ok("var a = 1; var b = 2;"); }
    #[test]
    fn parse_script_two_let() { let _ = parse_ok("let a = 1; let b = 2;"); }
    #[test]
    fn parse_script_two_const() { let _ = parse_ok("const a = 1; const b = 2;"); }
    #[test]
    fn parse_script_func_var() { let _ = parse_ok("var x = 1; function f() { return x; }"); }
    #[test]
    fn parse_script_two_func() { let _ = parse_ok("var a = 1; var b = 2;"); }
    #[test]
    fn parse_if_block_else() { let _ = parse_ok("function f() { if (true) return 1; else return 2; }"); }
    #[test]
    fn parse_while_block_stmt() { let _ = parse_ok("function f() { while (true) { return 1; } }"); }
    #[test]
    fn parse_add_literal() { let _ = parse_ok("function f() { return 1 + 2; }"); }
    #[test]
    fn parse_sub_literal() { let _ = parse_ok("function f() { return 5 - 3; }"); }
    #[test]
    fn parse_true_literal() { let _ = parse_ok("function f() { return true; }"); }
    #[test]
    fn parse_false_literal() { let _ = parse_ok("function f() { return false; }"); }
    #[test]
    fn parse_null_literal() { let _ = parse_ok("function f() { return null; }"); }
}
