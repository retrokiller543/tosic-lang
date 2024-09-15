use statement::error::EvalError;
use statement::Expr;

pub struct Evaluator {
    pub statements: Vec<(Expr, usize)>,
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
