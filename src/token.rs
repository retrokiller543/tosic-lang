use crate::error::TokenError;
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
    Slash,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Semicolon,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    EOF,
}

impl Token {
    pub fn from_string(s: &str, line: usize) -> Result<Self, TokenError> {
        s.parse()
            .map_err(|_| TokenError::InvalidToken(s.to_string(), line))
    }
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
            Token::Slash => write!(f, "SLASH / null"),
            Token::Equal => write!(f, "EQUAL = null"),
            Token::EqualEqual => write!(f, "EQUAL_EQUAL == null"),
            Token::Bang => write!(f, "BANG ! null"),
            Token::BangEqual => write!(f, "BANG_EQUAL != null"),
            Token::Less => write!(f, "LESS < null"),
            Token::LessEqual => write!(f, "LESS_EQUAL <= null"),
            Token::Greater => write!(f, "GREATER > null"),
            Token::GreaterEqual => write!(f, "GREATER_EQUAL >= null"),
            Token::Semicolon => write!(f, "SEMICOLON ; null"),
            Token::EOF => write!(f, "EOF  null"),
        }
    }
}

impl FromStr for Token {
    type Err = TokenError;

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
            "/" => Ok(Token::Slash),
            "=" => Ok(Token::Equal),
            "!" => Ok(Token::Bang),
            "<" => Ok(Token::Less),
            ">" => Ok(Token::Greater),
            token => Err(TokenError::InvalidToken(token.to_string(), 0)),
        }
    }
}