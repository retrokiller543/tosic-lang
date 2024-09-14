use crate::token::{trim_trailing_zeroes, Reserved, Token};
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
    pub fn parse(&mut self) -> Vec<Expr> {
        let mut statements = Vec::new();

        while let Some(expr) = self.parse_expression() {
            statements.push(expr);
        }

        statements
    }

    // Parses an expression (handles addition and subtraction)
    fn parse_expression(&mut self) -> Option<Expr> {
        self.parse_term() // Start by parsing terms (handles * and /)
    }

    // Parses terms (handles multiplication and division)
    fn parse_term(&mut self) -> Option<Expr> {
        let mut expr = self.parse_factor()?; // Parse the left-hand side

        while let Some(op) = self.parse_operator(&[
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Slash,
            Token::Less,
            Token::LessEqual,
            Token::Greater,
            Token::GreaterEqual,
            Token::EqualEqual,
            Token::BangEqual,
        ]) {
            let right = self.parse_factor()?; // Parse the right-hand side
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right)); // Build the BinaryOp
        }

        Some(expr)
    }

    // Parses factors (numbers, variables, unary operations, and grouped expressions)
    fn parse_factor(&mut self) -> Option<Expr> {
        if let Some(Token::Minus) = self.tokens.peek() {
            self.tokens.next(); // Consume the minus
            let expr = self.parse_factor()?; // Recursively call parse_factor to handle the expression after the minus
            return Some(Expr::UnaryOp(UnaryOperator::Negate, Box::new(expr)));
        }

        if let Some(Token::Bang) = self.tokens.peek() {
            self.tokens.next(); // Consume the bang
            let expr = self.parse_factor()?; // Recursively call parse_factor to handle the expression after the bang
            return Some(Expr::UnaryOp(UnaryOperator::Not, Box::new(expr)));
        }

        // Handle grouped expressions (parentheses)
        if let Some(Token::LeftParen) = self.tokens.peek() {
            return self.parse_parentheses();
        }

        // Handle literals and identifiers
        self.parse_primary()
    }

    // Parses primary expressions (numbers, variables, literals)
    fn parse_primary(&mut self) -> Option<Expr> {
        match self.tokens.peek() {
            Some(Token::LitNum(_)) => self.parse_number(),

            Some(Token::LitStr(ref s)) => {
                let s = s.clone();
                self.tokens.next(); // Consume the string token
                Some(Expr::Literal(Literal::String(s.into_owned())))
            }

            Some(Token::Reserved(Reserved::True)) => {
                self.tokens.next(); // Consume 'true' token
                Some(Expr::Literal(Literal::True))
            }

            Some(Token::Reserved(Reserved::False)) => {
                self.tokens.next(); // Consume 'false' token
                Some(Expr::Literal(Literal::False))
            }

            Some(Token::Reserved(Reserved::Nil)) => {
                self.tokens.next(); // Consume 'nil' token
                Some(Expr::Literal(Literal::Nil))
            }

            Some(Token::Ident(ref id)) => {
                let id = id.clone();
                self.tokens.next(); // Consume the identifier
                Some(Expr::Variable(id.to_string()))
            }

            _ => None,
        }
    }

    // Parses number literals
    fn parse_number(&mut self) -> Option<Expr> {
        if let Some(Token::LitNum(ref s)) = self.tokens.peek() {
            if let Ok(value) = f64::from_str(s) {
                self.tokens.next(); // Consume the number token
                return Some(Expr::Literal(Literal::Number(value)));
            }
        }
        None
    }

    // Parses expressions inside parentheses and groups them
    fn parse_parentheses(&mut self) -> Option<Expr> {
        self.tokens.next(); // Consume the '('
        let expr = self.parse_expression()?; // Parse the expression inside the parentheses
        if self.tokens.next() == Some(Token::RightParen) {
            return Some(Expr::Group(Box::new(expr))); // Return a grouped expression
        }
        None
    }

    // Parses binary operators and maps them to the correct `Operator` enum
    fn parse_operator(&mut self, expected: &[Token<'a>]) -> Option<Operator> {
        if let Some(token) = self.tokens.peek() {
            for op in expected {
                if token == op {
                    self.tokens.next(); // Consume the operator token
                    return match op {
                        Token::Plus => Some(Operator::Plus),
                        Token::Minus => Some(Operator::Minus),
                        Token::Star => Some(Operator::Star),
                        Token::Slash => Some(Operator::Slash),
                        Token::Less => Some(Operator::Less),
                        Token::LessEqual => Some(Operator::LessEqual),
                        Token::Greater => Some(Operator::Greater),
                        Token::GreaterEqual => Some(Operator::GreaterEqual),
                        Token::EqualEqual => Some(Operator::Equal),
                        Token::BangEqual => Some(Operator::NotEqual),
                        _ => None,
                    };
                }
            }
        }
        None
    }
}
