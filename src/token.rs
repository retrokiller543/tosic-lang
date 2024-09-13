use std::borrow::Cow;
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
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
    LitStr(Cow<'a, str>),
    LitNum(String),
    Ident(Cow<'a, str>),
    EOF,
}

impl<'a> Display for Token<'a> {
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
            Token::LitStr(s) => write!(f, "STRING \"{}\" {}", s, s),
            Token::LitNum(n) => {
                if n.ends_with(".0") {
                    write!(f, "NUMBER {} {}", n.replace(".0", ""), n)
                } else if !n.contains(".") {
                    write!(f, "NUMBER {} {}.0", n, n)
                } else {
                    write!(f, "NUMBER {} {}", n, trim_trailing_zeroes(n))
                }
            },
            Token::Ident(s) => write!(f, "IDENTIFIER {} null", s),
            Token::EOF => write!(f, "EOF  null"),
        }
    }
}

fn trim_trailing_zeroes(s: &str) -> String {
    // s = 42.0000 output = 42.0
    if !s.contains(".") {
        return s.to_string();
    }

    let trimmed = s.trim_end_matches("0").to_string();

    if !trimmed.ends_with(".") {
        trimmed
    } else {
        trimmed + "0"
    }
}
