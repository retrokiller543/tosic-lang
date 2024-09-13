use std::borrow::Cow;
use std::fmt::{Debug, Display};
use std::str::FromStr;

#[derive(PartialEq)]
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
    Reserved(Reserved),
    EOF,
}

#[derive(PartialEq)]
pub enum Reserved {
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl Debug for Reserved {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reserved::And => write!(f, "AND and null"),
            Reserved::Class => write!(f, "CLASS class null"),
            Reserved::Else => write!(f, "ELSE else null"),
            Reserved::False => write!(f, "FALSE false null"),
            Reserved::For => write!(f, "FOR for null"),
            Reserved::Fun => write!(f, "FUN fun null"),
            Reserved::If => write!(f, "IF if null"),
            Reserved::Nil => write!(f, "NIL nil null"),
            Reserved::Or => write!(f, "OR or null"),
            Reserved::Print => write!(f, "PRINT print null"),
            Reserved::Return => write!(f, "RETURN return null"),
            Reserved::Super => write!(f, "SUPER super null"),
            Reserved::This => write!(f, "THIS this null"),
            Reserved::True => write!(f, "TRUE true null"),
            Reserved::Var => write!(f, "VAR var null"),
            Reserved::While => write!(f, "WHILE while null"),
        }
    }
}

impl FromStr for Reserved {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "and" => Ok(Reserved::And),
            "class" => Ok(Reserved::Class),
            "else" => Ok(Reserved::Else),
            "false" => Ok(Reserved::False),
            "for" => Ok(Reserved::For),
            "fun" => Ok(Reserved::Fun),
            "if" => Ok(Reserved::If),
            "nil" => Ok(Reserved::Nil),
            "or" => Ok(Reserved::Or),
            "print" => Ok(Reserved::Print),
            "return" => Ok(Reserved::Return),
            "super" => Ok(Reserved::Super),
            "this" => Ok(Reserved::This),
            "true" => Ok(Reserved::True),
            "var" => Ok(Reserved::Var),
            "while" => Ok(Reserved::While),
            _ => Err(()),
        }
    }
}

impl<'a> Debug for Token<'a> {
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
            Token::Reserved(r) => write!(f, "{}", r),
            Token::EOF => write!(f, "EOF  null"),
        }
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LitStr(s) => write!(f, "{}", s),
            Token::LitNum(n) => write!(f, "{}", n),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Reserved(r) => write!(f, "{}", r),
            Token::EOF => write!(f, "EOF"),
            Token::RightBrace => write!(f, "}}"),
            Token::LeftBrace => write!(f, "{{"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::Star => write!(f, "*"),
            Token::Dot => write!(f, "."),
            Token::Comma => write!(f, ","),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Slash => write!(f, "/"),
            Token::Equal => write!(f, "="),
            Token::EqualEqual => write!(f, "=="),
            Token::Bang => write!(f, "!"),
            Token::BangEqual => write!(f, "!="),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Semicolon => write!(f, ";"),
        }
    }
}

impl Display for Reserved {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reserved::And => write!(f, "and"),
            Reserved::Class => write!(f, "class"),
            Reserved::Else => write!(f, "else"),
            Reserved::False => write!(f, "false"),
            Reserved::Fun => write!(f, "fun"),
            Reserved::For => write!(f, "for"),
            Reserved::If => write!(f, "if"),
            Reserved::Nil => write!(f, "nil"),
            Reserved::Or => write!(f, "or"),
            Reserved::Print => write!(f, "print"),
            Reserved::Return => write!(f, "return"),
            Reserved::Super => write!(f, "super"),
            Reserved::This => write!(f, "this"),
            Reserved::True => write!(f, "true"),
            Reserved::Var => write!(f, "var"),
            Reserved::While => write!(f, "while"),
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
