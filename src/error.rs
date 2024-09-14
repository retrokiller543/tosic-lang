use crate::token::Token;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum TokenError {
    #[error("[line {1}] Error: Unexpected character: {0}")]
    InvalidToken(String, usize),
    #[error("[line {0}] Error: Unterminated string.")]
    UnterminatedString(usize),
}

#[derive(Debug, Error)]
pub enum ParserError<'a> {
    #[error("[line {0}] Error at {1}: Unexpected token: {2}")]
    UnexpectedToken(usize, Token<'a>, &'static str),
    #[error("[line {0}] Error at {1}: Expected expression.")]
    ExpectedExpression(usize, Token<'a>),
    #[error("[line {0}] Error at '{1}': Expected {2}.")]
    ExpectedToken(usize, Token<'a>, &'static str),
    #[error("[line {0}] Error at {1}: Invalid number literal.")]
    InvalidNumber(usize, Token<'a>),
    #[error("[line {0}] Error: Unexpected end of file.")]
    UnexpectedEOF(usize),
}

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("[line {line}] Error: Undefined variable '{name}'.")]
    UndefinedVariable { name: String, line: usize },
    #[error("[line {line}] Error: {message}")]
    TypeError { message: String, line: usize },
    #[error("[line {line}] Runtime error: {message}")]
    RuntimeError { message: String, line: usize },
}