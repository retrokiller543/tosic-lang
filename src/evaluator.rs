use crate::error::EvalError;
use crate::parser::Expr;

pub struct Evaluator {
    pub statements: Vec<(Expr, usize)>, // Include line numbers
}

impl Evaluator {
    pub fn new(statements: Vec<(Expr, usize)>) -> Self {
        Self { statements }
    }

    pub fn evaluate(&self) -> Result<(), EvalError> {
        for (expr, line) in &self.statements {
            let value = expr.evaluate(*line)?;
            println!("{}", value);
        }
        Ok(())
    }
}
