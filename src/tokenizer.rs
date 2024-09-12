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
    Semicolon,
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
            token => Err(TokenError::InvalidToken(token.to_string(), 0)),
        }
    }
}

pub fn tokenize(code: String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let lines = code.lines();

    for (i, line) in lines.enumerate() {
        let chars = line.chars();

        for c in chars {
            if c.is_whitespace() {
                continue;
            }

            match Token::from_string(&c.to_string(), i + 1) {
                Ok(token) => tokens.push(token),
                Err(err) => eprintln!("{}", err),
            }
        }
    }

    tokens.push(Token::EOF);

    tokens
}
