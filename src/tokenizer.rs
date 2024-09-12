use std::fmt::Display;
use std::str::FromStr;

#[derive(Debug)]
pub enum Token {
    LeftParen,
    RightParen,
    EOF
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LeftParen => write!(f, "LEFT_PAREN ( null"),
            Token::RightParen => write!(f, "RIGHT_PAREN ) null"),
            Token::EOF => write!(f, "EOF  null"),
        }
    }
}

impl FromStr for Token {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "(" => Ok(Token::LeftParen),
            ")" => Ok(Token::RightParen),
            _ => Ok(Token::EOF),
        }
    }
}

pub fn tokenize(code: String) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = code.chars();

    while let Some(c) = chars.next() {
        if c == ' ' || c == '\n' || c == '\t' {
            continue;
        }

        tokens.push(Token::from_str(&c.to_string()).unwrap());
    }

    tokens.push(Token::EOF);

    tokens
}