use thiserror::Error;
use tokens::Token;

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
