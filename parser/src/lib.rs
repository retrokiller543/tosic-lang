pub mod error;

use crate::error::ParserError;
use statement::{Expr, Literal, Operator, Stmt, UnaryOperator};
use std::iter::Peekable;
use std::str::FromStr;
use std::vec::IntoIter;
use tokens::{Reserved, Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    tokens: Peekable<IntoIter<Token<'a>>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    fn match_token(&mut self, kind: TokenKind<'a>) -> bool {
        if self.check(kind.clone()) {
            self.tokens.next();
            true
        } else {
            false
        }
    }

    fn consume(
        &mut self,
        kind: TokenKind<'a>,
        message: &'static str,
    ) -> Result<(), ParserError<'a>> {
        if self.check(kind.clone()) {
            self.tokens.next();
            Ok(())
        } else {
            let token = self.peek_token().cloned().unwrap_or(Token {
                kind: TokenKind::EOF,
                line: self.current_line(),
            });

            Err(ParserError::ExpectedToken(
                token.line,
                token.clone(),
                message,
            ))
        }
    }

    fn check(&mut self, kind: TokenKind<'a>) -> bool {
        if let Some(token) = self.tokens.peek() {
            token.kind == kind
        } else {
            false
        }
    }

    fn at_end(&mut self) -> bool {
        self.tokens
            .peek()
            .map_or(true, |t| t.kind == TokenKind::EOF)
    }

    fn peek_token(&mut self) -> Option<&Token<'a>> {
        self.tokens.peek()
    }

    fn current_line(&mut self) -> usize {
        self.peek_token().map_or(0, |t| t.line)
    }

    fn parse_declaration(&mut self) -> Result<Stmt, ParserError<'a>> {
        if self.match_token(TokenKind::Reserved(Reserved::Var)) {
            self.parse_var_declaration()
        } else {
            self.parse_statement()
        }
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError<'a>> {
        if self.match_token(TokenKind::Reserved(Reserved::If)) {
            self.parse_if_statement()
        } else if self.match_token(TokenKind::Reserved(Reserved::While)) {
            self.parse_while_statement()
        } else if self.match_token(TokenKind::Reserved(Reserved::Print)) {
            self.parse_print_statement()
        } else if self.match_token(TokenKind::LeftBrace) {
            self.parse_block()
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_var_declaration(&mut self) -> Result<Stmt, ParserError<'a>> {
        let name = if let Some(TokenKind::Ident(name)) = self.tokens.next().map(|t| t.kind) {
            name.to_string()
        } else {
            let token = self.peek_token().cloned().unwrap_or(Token {
                kind: TokenKind::EOF,
                line: self.current_line(),
            });

            return Err(ParserError::ExpectedToken(
                token.line,
                token.clone(),
                "variable name",
            ));
        };

        let initializer = if self.match_token(TokenKind::Equal) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume(TokenKind::Semicolon, "';' after variable declaration.")?;
        Ok(Stmt::Var(name, initializer))
    }

    fn parse_if_statement(&mut self) -> Result<Stmt, ParserError<'a>> {
        self.consume(TokenKind::LeftParen, "'(' after 'if'.")?;
        let condition = self.parse_expression()?;
        self.consume(TokenKind::RightParen, "')' after if condition.")?;

        let then_branch = Box::new(self.parse_statement()?);
        let else_branch = if self.match_token(TokenKind::Reserved(Reserved::Else)) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        Ok(Stmt::If(condition, then_branch, else_branch))
    }

    fn parse_while_statement(&mut self) -> Result<Stmt, ParserError<'a>> {
        self.consume(TokenKind::LeftParen, "'(' after 'while'.")?;
        let condition = self.parse_expression()?;
        self.consume(TokenKind::RightParen, "')' after condition.")?;
        let body = Box::new(self.parse_statement()?);

        Ok(Stmt::While(condition, body))
    }

    fn parse_print_statement(&mut self) -> Result<Stmt, ParserError<'a>> {
        let value = self.parse_expression()?;
        self.consume(TokenKind::Semicolon, "';' after value.")?;
        Ok(Stmt::Print(value))
    }

    fn parse_block(&mut self) -> Result<Stmt, ParserError<'a>> {
        let mut statements = Vec::new();

        while !self.check(TokenKind::RightBrace) && !self.at_end() {
            statements.push(self.parse_declaration()?);
        }

        self.consume(TokenKind::RightBrace, "'}' after block.")?;
        Ok(Stmt::Block(statements))
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt, ParserError<'a>> {
        let expr = self.parse_expression()?;
        self.consume(TokenKind::Semicolon, "';' after expression.")?;
        Ok(Stmt::Expression(expr))
    }

    // Entry point for parsing statements
    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParserError<'a>> {
        let mut statements = Vec::new();

        while let Some(token) = self.tokens.peek() {
            if token.kind == TokenKind::EOF {
                break;
            }
            let stmt = self.parse_declaration()?;
            statements.push(stmt);
        }

        Ok(statements)
    }

    // Parses a single expression, useful for REPL or evaluating expressions directly
    pub fn parse_single_expression(&mut self) -> Result<Expr, ParserError<'a>> {
        let expr = self.parse_expression()?;
        if !self.at_end() {
            let next_token = self.tokens.next().unwrap();
            return Err(ParserError::UnexpectedToken(
                next_token.line,
                next_token,
                "Expected end of input after expression",
            ));
        }
        Ok(expr)
    }

    pub fn parse_expressions(&mut self) -> Result<Vec<(Expr, usize)>, ParserError<'a>> {
        let mut expressions = Vec::new();

        while let Some(token) = self.tokens.peek() {
            if token.kind == TokenKind::EOF {
                break;
            }
            let expr = self.parse_expression()?;
            expressions.push((expr, self.current_line()));
        }

        Ok(expressions)
    }

    fn parse_expression(&mut self) -> Result<Expr, ParserError<'a>> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParserError<'a>> {
        let expr = self.parse_equality()?;

        if self.match_token(TokenKind::Equal) {
            let equals_token = self.peek_token().unwrap().clone();
            let value = self.parse_assignment()?;

            if let Expr::Variable(name) = expr {
                Ok(Expr::Assign(name, Box::new(value)))
            } else {
                Err(ParserError::InvalidAssignmentTarget(
                    equals_token.line,
                    equals_token,
                ))
            }
        } else {
            Ok(expr)
        }
    }

    // Parses equality expressions (handles == and !=)
    fn parse_equality(&mut self) -> Result<Expr, ParserError<'a>> {
        let mut expr = self.parse_comparison()?;

        while let Some(token) = self.tokens.peek() {
            match &token.kind {
                TokenKind::EqualEqual | TokenKind::BangEqual => {
                    let operator =
                        self.parse_operator(&[TokenKind::EqualEqual, TokenKind::BangEqual])?;
                    let right = self.parse_comparison()?;
                    expr = Expr::BinaryOp(Box::new(expr), operator, Box::new(right));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    // Parses comparison expressions (handles >, >=, <, <=)
    fn parse_comparison(&mut self) -> Result<Expr, ParserError<'a>> {
        let mut expr = self.parse_term()?;

        while let Some(token) = self.tokens.peek() {
            match &token.kind {
                TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::Less
                | TokenKind::LessEqual => {
                    let operator = self.parse_operator(&[
                        TokenKind::Greater,
                        TokenKind::GreaterEqual,
                        TokenKind::Less,
                        TokenKind::LessEqual,
                    ])?;
                    let right = self.parse_term()?;
                    expr = Expr::BinaryOp(Box::new(expr), operator, Box::new(right));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    // Parses terms (handles addition and subtraction)
    fn parse_term(&mut self) -> Result<Expr, ParserError<'a>> {
        let mut expr = self.parse_factor()?;

        while let Some(token) = self.tokens.peek() {
            match &token.kind {
                TokenKind::Plus | TokenKind::Minus => {
                    let operator = self.parse_operator(&[TokenKind::Plus, TokenKind::Minus])?;
                    let right = self.parse_factor()?;
                    expr = Expr::BinaryOp(Box::new(expr), operator, Box::new(right));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    // Parses factors (handles multiplication and division)
    fn parse_factor(&mut self) -> Result<Expr, ParserError<'a>> {
        let mut expr = self.parse_unary()?;

        while let Some(token) = self.tokens.peek() {
            match &token.kind {
                TokenKind::Star | TokenKind::Slash => {
                    let operator = self.parse_operator(&[TokenKind::Star, TokenKind::Slash])?;
                    let right = self.parse_unary()?;
                    expr = Expr::BinaryOp(Box::new(expr), operator, Box::new(right));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    // Parses unary expressions (handles ! and -)
    fn parse_unary(&mut self) -> Result<Expr, ParserError<'a>> {
        if let Some(token) = self.tokens.peek() {
            match &token.kind {
                TokenKind::Bang => {
                    let _token = self.tokens.next().unwrap(); // Consume '!'
                    let right = self.parse_unary()?;
                    return Ok(Expr::UnaryOp(UnaryOperator::Not, Box::new(right)));
                }
                TokenKind::Minus => {
                    let _token = self.tokens.next().unwrap(); // Consume '-'
                    let right = self.parse_unary()?;
                    return Ok(Expr::UnaryOp(UnaryOperator::Negate, Box::new(right)));
                }
                _ => {}
            }
        }

        self.parse_primary()
    }

    // Parses primary expressions (literals, identifiers, and grouped expressions)
    fn parse_primary(&mut self) -> Result<Expr, ParserError<'a>> {
        if let Some(token) = self.tokens.next() {
            match &token.kind {
                TokenKind::LitNum(s) => {
                    if let Ok(value) = f64::from_str(s) {
                        Ok(Expr::Literal(Literal::Number(value)))
                    } else {
                        Err(ParserError::InvalidNumber(token.line, token))
                    }
                }
                TokenKind::LitStr(s) => Ok(Expr::Literal(Literal::String(s.to_string()))),
                TokenKind::Reserved(Reserved::True) => Ok(Expr::Literal(Literal::True)),
                TokenKind::Reserved(Reserved::False) => Ok(Expr::Literal(Literal::False)),
                TokenKind::Reserved(Reserved::Nil) => Ok(Expr::Literal(Literal::Nil)),
                TokenKind::Ident(name) => Ok(Expr::Variable(name.to_string())),
                TokenKind::LeftParen => {
                    let expr = self.parse_expression()?;
                    self.consume(TokenKind::RightParen, "Expect ')' after expression.")?;
                    Ok(Expr::Group(Box::new(expr)))
                }
                TokenKind::EOF => Err(ParserError::UnexpectedEOF(token.line)),
                _ => Err(ParserError::ExpectedExpression(token.line, token)),
            }
        } else {
            Err(ParserError::UnexpectedEOF(self.current_line()))
        }
    }

    // Parses an operator and returns the corresponding Operator enum
    fn parse_operator(&mut self, expected: &[TokenKind<'a>]) -> Result<Operator, ParserError<'a>> {
        if let Some(token) = self.tokens.next() {
            if expected.contains(&token.kind) {
                match token.kind {
                    TokenKind::Plus => Ok(Operator::Plus),
                    TokenKind::Minus => Ok(Operator::Minus),
                    TokenKind::Star => Ok(Operator::Star),
                    TokenKind::Slash => Ok(Operator::Slash),
                    TokenKind::Less => Ok(Operator::Less),
                    TokenKind::LessEqual => Ok(Operator::LessEqual),
                    TokenKind::Greater => Ok(Operator::Greater),
                    TokenKind::GreaterEqual => Ok(Operator::GreaterEqual),
                    TokenKind::EqualEqual => Ok(Operator::Equal),
                    TokenKind::BangEqual => Ok(Operator::NotEqual),
                    _ => Err(ParserError::UnexpectedToken(
                        token.line,
                        token,
                        "Invalid operator",
                    )),
                }
            } else {
                Err(ParserError::UnexpectedToken(
                    token.line,
                    token,
                    "Expected operator",
                ))
            }
        } else {
            Err(ParserError::UnexpectedEOF(self.current_line()))
        }
    }
}
