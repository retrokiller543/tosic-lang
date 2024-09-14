use crate::error::{EvalError, ParserError};
use crate::token::{trim_trailing_zeroes, Reserved, Token, TokenKind};
use crate::value::Value;
use std::fmt::Display;
use std::iter::Peekable;
use std::str::FromStr;
use std::vec::IntoIter;

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Variable(String),
    UnaryOp(UnaryOperator, Box<Expr>),
    BinaryOp(Box<Expr>, Operator, Box<Expr>),
    Group(Box<Expr>),
}

impl Expr {
    pub fn evaluate(&self, line: usize) -> Result<Value, EvalError> {
        match self {
            Expr::Literal(literal) => match literal {
                Literal::Number(n) => Ok(Value::Number(*n)),
                Literal::String(s) => Ok(Value::String(s.clone())),
                Literal::True => Ok(Value::Boolean(true)),
                Literal::False => Ok(Value::Boolean(false)),
                Literal::Nil => Ok(Value::Nil),
            },
            Expr::Variable(name) => {
                // Variable handling is not implemented yet
                Err(EvalError::UndefinedVariable {
                    name: name.clone(),
                    line,
                })
            }
            Expr::UnaryOp(op, expr) => {
                let value = expr.evaluate(line)?;
                match (op, value) {
                    (UnaryOperator::Negate, Value::Number(n)) => Ok(Value::Number(-n)),
                    (UnaryOperator::Not, Value::Boolean(b)) => Ok(Value::Boolean(!b)),
                    (UnaryOperator::Not, Value::Nil) => Ok(Value::Boolean(true)),
                    (UnaryOperator::Not, _) => Ok(Value::Boolean(false)),
                    (UnaryOperator::Negate, _) => Err(EvalError::TypeError {
                        message: "Operand must be a number.".to_string(),
                        line,
                    }),
                }
            }
            Expr::BinaryOp(left, op, right) => {
                let left_value = left.evaluate(line)?;
                let right_value = right.evaluate(line)?;
                match (op, left_value, right_value) {
                    // Arithmetic operations
                    (Operator::Plus, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l + r))
                    }
                    (Operator::Minus, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l - r))
                    }
                    (Operator::Star, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l * r))
                    }
                    (Operator::Slash, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l / r))
                    }
                    // String concatenation
                    (Operator::Plus, Value::String(l), Value::String(r)) => {
                        Ok(Value::String(l + &r))
                    }
                    // Comparison operators
                    (Operator::Greater, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Boolean(l > r))
                    }
                    (Operator::GreaterEqual, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Boolean(l >= r))
                    }
                    (Operator::Less, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Boolean(l < r))
                    }
                    (Operator::LessEqual, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Boolean(l <= r))
                    }
                    // Equality operators
                    (Operator::Equal, l, r) => Ok(Value::Boolean(l == r)),
                    (Operator::NotEqual, l, r) => Ok(Value::Boolean(l != r)),
                    // Type errors
                    (Operator::Plus, _, _) => Err(EvalError::TypeError {
                        message: "Operands must be two numbers or two strings.".to_string(),
                        line,
                    }),
                    (Operator::Minus | Operator::Star | Operator::Slash, _, _) => {
                        Err(EvalError::TypeError {
                            message: "Operands must be numbers.".to_string(),
                            line,
                        })
                    }
                    (
                        Operator::Greater
                        | Operator::GreaterEqual
                        | Operator::Less
                        | Operator::LessEqual,
                        _,
                        _,
                    ) => Err(EvalError::TypeError {
                        message: "Operands must be numbers.".to_string(),
                        line,
                    }),
                }
            }
            Expr::Group(expr) => expr.evaluate(line),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(l) => write!(f, "{}", l),
            Expr::Variable(s) => write!(f, "{}", s),
            Expr::UnaryOp(op, e) => write!(f, "({} {})", op, e),
            Expr::BinaryOp(l, op, r) => write!(f, "({} {} {})", op, l, r),
            Expr::Group(e) => write!(f, "(group {})", e),
        }
    }
}

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => {
                let n = n.to_string();
                if n.ends_with(".0") {
                    write!(f, "{}", n)
                } else if !n.contains('.') {
                    write!(f, "{}.0", n)
                } else {
                    write!(f, "{}", trim_trailing_zeroes(n.as_str()))
                }
            }
            Literal::String(s) => write!(f, "{}", s),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negate,
    Not,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Negate => write!(f, "-"),
            UnaryOperator::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Star => write!(f, "*"),
            Operator::Slash => write!(f, "/"),
            Operator::Less => write!(f, "<"),
            Operator::LessEqual => write!(f, "<="),
            Operator::Greater => write!(f, ">"),
            Operator::GreaterEqual => write!(f, ">="),
            Operator::Equal => write!(f, "=="),
            Operator::NotEqual => write!(f, "!="),
        }
    }
}

pub struct Parser<'a> {
    tokens: Peekable<IntoIter<Token<'a>>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    // Entry point for parsing
    pub fn parse(&mut self) -> Result<Vec<(Expr, usize)>, ParserError<'a>> {
        let mut statements = Vec::new();

        while let Some(token) = self.tokens.peek() {
            if token.kind == TokenKind::EOF {
                break;
            }
            let line = token.clone().line;

            let expr = self.parse_expression()?;

            statements.push((expr, line));
        }

        Ok(statements)
    }

    fn parse_expression(&mut self) -> Result<Expr, ParserError<'a>> {
        self.parse_equality()
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
                    if let Some(next_token) = self.tokens.next() {
                        if let TokenKind::RightParen = next_token.kind {
                            Ok(Expr::Group(Box::new(expr)))
                        } else {
                            Err(ParserError::ExpectedToken(token.line, next_token, "')'"))
                        }
                    } else {
                        Err(ParserError::UnexpectedEOF(token.line))
                    }
                }
                TokenKind::EOF => Err(ParserError::UnexpectedEOF(token.line)),
                _ => Err(ParserError::ExpectedExpression(token.line, token)),
            }
        } else {
            Err(ParserError::UnexpectedEOF(0))
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
            Err(ParserError::UnexpectedEOF(0))
        }
    }
}
