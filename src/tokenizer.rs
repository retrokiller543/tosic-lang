use std::fmt::Display;
use std::str::FromStr;

#[derive(Debug)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Star,
    Dot,
    Comma,
    Plus,
    Minus,
    Semicolon,
    EOF
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LeftParen => write!(f, "LEFT_PAREN ( null"),
            Token::RightParen => write!(f, "RIGHT_PAREN ) null"),
            Token::LeftBrace => write!(f, "LEFT_BRACE {{ null"),
            Token::RightBrace => write!(f, "RIGHT_BRACE }} null"),
            Token::Star => write!(f, "STAR * null"),
            Token::Dot => write!(f, "DOT . null"),
            Token::Comma => write!(f, "COMMA , null"),
            Token::Plus => write!(f, "PLUS + null"),
            Token::Minus => write!(f, "MINUS - null"),
            Token::Semicolon => write!(f, "SEMICOLON ; null"),
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
            "{" => Ok(Token::LeftBrace),
            "}" => Ok(Token::RightBrace),
            "*" => Ok(Token::Star),
            "." => Ok(Token::Dot),
            "," => Ok(Token::Comma),
            "+" => Ok(Token::Plus),
            "-" => Ok(Token::Minus),
            ";" => Ok(Token::Semicolon),
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