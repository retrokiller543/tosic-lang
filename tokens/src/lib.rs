use std::borrow::Cow;
use std::fmt::{Debug, Display};
use std::str::FromStr;

#[derive(PartialEq, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub line: usize,
}

impl Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(PartialEq, Clone)]
pub enum TokenKind<'a> {
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

#[derive(PartialEq, Clone)]
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

impl<'a> Debug for TokenKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::LeftParen => write!(f, "LEFT_PAREN ( null"),
            TokenKind::RightParen => write!(f, "RIGHT_PAREN ) null"),
            TokenKind::LeftBrace => write!(f, "LEFT_BRACE {{ null"),
            TokenKind::RightBrace => write!(f, "RIGHT_BRACE }} null"),
            TokenKind::Star => write!(f, "STAR * null"),
            TokenKind::Dot => write!(f, "DOT . null"),
            TokenKind::Comma => write!(f, "COMMA , null"),
            TokenKind::Plus => write!(f, "PLUS + null"),
            TokenKind::Minus => write!(f, "MINUS - null"),
            TokenKind::Slash => write!(f, "SLASH / null"),
            TokenKind::Equal => write!(f, "EQUAL = null"),
            TokenKind::EqualEqual => write!(f, "EQUAL_EQUAL == null"),
            TokenKind::Bang => write!(f, "BANG ! null"),
            TokenKind::BangEqual => write!(f, "BANG_EQUAL != null"),
            TokenKind::Less => write!(f, "LESS < null"),
            TokenKind::LessEqual => write!(f, "LESS_EQUAL <= null"),
            TokenKind::Greater => write!(f, "GREATER > null"),
            TokenKind::GreaterEqual => write!(f, "GREATER_EQUAL >= null"),
            TokenKind::Semicolon => write!(f, "SEMICOLON ; null"),
            TokenKind::LitStr(s) => write!(f, "STRING \"{}\" {}", s, s),
            TokenKind::LitNum(n) => {
                if n.ends_with(".0") {
                    write!(f, "NUMBER {} {}", n.replace(".0", ""), n)
                } else if !n.contains(".") {
                    write!(f, "NUMBER {} {}.0", n, n)
                } else {
                    write!(f, "NUMBER {} {}", n, trim_trailing_zeroes(n))
                }
            }
            TokenKind::Ident(s) => write!(f, "IDENTIFIER {} null", s),
            TokenKind::Reserved(r) => write!(f, "{:?}", r),
            TokenKind::EOF => write!(f, "EOF  null"),
        }
    }
}

impl<'a> Display for TokenKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::LitStr(s) => write!(f, "{}", s),
            TokenKind::LitNum(n) => {
                if n.ends_with(".0") {
                    write!(f, "{}", n)
                } else if !n.contains(".") {
                    write!(f, "{}.0", n)
                } else {
                    write!(f, "{}", trim_trailing_zeroes(n))
                }
            }
            TokenKind::Ident(s) => write!(f, "{}", s),
            TokenKind::Reserved(r) => write!(f, "{}", r),
            TokenKind::EOF => write!(f, "EOF"),
            TokenKind::RightBrace => write!(f, "}}"),
            TokenKind::LeftBrace => write!(f, "{{"),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Equal => write!(f, "="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::BangEqual => write!(f, "!="),
            TokenKind::Less => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::Semicolon => write!(f, ";"),
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

pub fn trim_trailing_zeroes(s: &str) -> String {
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
