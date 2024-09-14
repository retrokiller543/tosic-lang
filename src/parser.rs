use crate::token::{trim_trailing_zeroes, Reserved, Token};
use std::fmt::Display;
use std::iter::Peekable;
use std::str::FromStr;
use std::vec::IntoIter;

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Variable(String),
    UnaryOp(UnaryOperator, Box<Expr>), // Unary operators like - and !
    BinaryOp(Box<Expr>, Operator, Box<Expr>),
    Group(Box<Expr>),
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
    Negate, // For - (negation)
    Not,    // For ! (logical NOT)
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
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Star => write!(f, "*"),
            Operator::Slash => write!(f, "/"),
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

    // Parses unary operations (like -expr or !expr)
    fn parse_unary(&mut self) -> Option<Expr> {
        // Handle - (negation)
        if let Some(Token::Minus) = self.tokens.peek() {
            self.tokens.next(); // Consume the minus
            let expr = self.parse_expression()?;
            return Some(Expr::UnaryOp(UnaryOperator::Negate, Box::new(expr)));
        }

        // Handle ! (logical NOT)
        if let Some(Token::Bang) = self.tokens.peek() {
            self.tokens.next(); // Consume the bang
            let expr = self.parse_expression()?;
            return Some(Expr::UnaryOp(UnaryOperator::Not, Box::new(expr)));
        }

        // If no unary operator, parse as primary expression
        self.parse_primary()
    }

    // Parses primary expressions: numbers, strings, booleans, nil, and parentheses
    fn parse_primary(&mut self) -> Option<Expr> {
        match self.tokens.peek() {
            // Handle numeric literals
            Some(Token::LitNum(_)) => self.parse_number(),

            // Handle string literals
            Some(Token::LitStr(ref s)) => {
                let s = s.clone();
                self.tokens.next(); // Consume the string token
                Some(Expr::Literal(Literal::String(s.into_owned())))
            }

            // Handle boolean literals 'true' and 'false'
            Some(Token::Reserved(Reserved::True)) => {
                self.tokens.next(); // Consume 'true' token
                Some(Expr::Literal(Literal::True))
            }
            Some(Token::Reserved(Reserved::False)) => {
                self.tokens.next(); // Consume 'false' token
                Some(Expr::Literal(Literal::False))
            }

            // Handle nil literal
            Some(Token::Reserved(Reserved::Nil)) => {
                self.tokens.next(); // Consume 'nil' token
                Some(Expr::Literal(Literal::Nil))
            }

            // Handle expressions inside parentheses
            Some(Token::LeftParen) => self.parse_parentheses(),

            // Handle variable identifiers
            Some(Token::Ident(ref id)) => {
                let id = id.clone();
                self.tokens.next(); // Consume identifier token
                Some(Expr::Variable(id.to_string()))
            }

            _ => None,
        }
    }

    // Parses number literals
    fn parse_number(&mut self) -> Option<Expr> {
        if let Some(Token::LitNum(ref s)) = self.tokens.peek() {
            if let Ok(value) = f64::from_str(s) {
                self.tokens.next(); // Consume the token
                return Some(Expr::Literal(Literal::Number(value)));
            }
        }
        None
    }

    // Parses expressions inside parentheses and groups them
    fn parse_parentheses(&mut self) -> Option<Expr> {
        if self.tokens.next() == Some(Token::LeftParen) {
            let expr = self.parse_expression()?;
            if self.tokens.next() == Some(Token::RightParen) {
                return Some(Expr::Group(Box::new(expr)));
            }
        }
        None
    }

    // Parses binary expressions with operators
    fn parse_expression(&mut self) -> Option<Expr> {
        let mut expr = self.parse_unary()?;

        // Parse binary operators and chain the expressions together
        while let Some(op) = self.parse_operator() {
            let right = self.parse_unary()?;
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }

        Some(expr)
    }

    // Parses operators
    fn parse_operator(&mut self) -> Option<Operator> {
        match self.tokens.peek() {
            Some(Token::Plus) => {
                self.tokens.next(); // Consume the plus
                Some(Operator::Plus)
            }
            Some(Token::Minus) => {
                self.tokens.next(); // Consume the minus
                Some(Operator::Minus)
            }
            Some(Token::Star) => {
                self.tokens.next(); // Consume the star
                Some(Operator::Star)
            }
            Some(Token::Slash) => {
                self.tokens.next(); // Consume the slash
                Some(Operator::Slash)
            }
            _ => None,
        }
    }

    // Entry point for parsing
    pub fn parse(&mut self) -> Vec<Expr> {
        let mut statements = Vec::new();

        while let Some(expr) = self.parse_expression() {
            statements.push(expr);
        }

        statements
    }
}
