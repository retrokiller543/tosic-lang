use thiserror::Error;

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("[line {line}] Error: Undefined variable '{name}'.")]
    UndefinedVariable { name: String, line: usize },
    #[error("[line {line}] Error: {message}")]
    TypeError { message: String, line: usize },
    #[error("[line {line}] Runtime error: {message}")]
    RuntimeError { message: String, line: usize },
    #[error("{0}")]
    GenericError(String),
}
