use thiserror::Error;

#[derive(Debug, Error)]
pub enum TokenError {
    #[error("[line {1}] Error: Unexpected character: {0}")]
    InvalidToken(String, usize),
    #[error("[line {0}] Error: Unterminated string.")]
    UnterminatedString(usize),
}
