use crate::environment::Environment;
use statement::error::EvalError;
use statement::traits::{EvalResult, ExprVisitor, StmtVisitor};
use statement::value::Value;
use statement::{Expr, Literal, Operator, Stmt, UnaryOperator};
use std::cell::RefCell;
use std::rc::Rc;

pub mod environment;

pub struct Evaluator {
    environment: Rc<RefCell<Environment>>,
    output: Rc<RefCell<dyn Output>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            environment: Rc::new(RefCell::new(Environment::new())),
            output: Rc::new(RefCell::new(StandardOutput {})),
        }
    }

    pub fn new_with_output(output: Rc<RefCell<dyn Output>>) -> Self {
        Evaluator {
            environment: Rc::new(RefCell::new(Environment::new())),
            output,
        }
    }

    pub fn evaluate(&mut self, statements: &[Stmt]) -> EvalResult<()> {
        for statement in statements {
            statement.accept(self)?;
        }
        Ok(())
    }

    pub fn evaluate_expr(&mut self, expr: &Expr) -> EvalResult<Value> {
        expr.accept(self)
    }

    fn execute_block(
        &mut self,
        statements: &[Stmt],
        environment: Rc<RefCell<Environment>>,
    ) -> EvalResult<()> {
        let previous = self.environment.clone();
        self.environment = environment;

        for statement in statements {
            statement.accept(self)?;
        }

        self.environment = previous;
        Ok(())
    }
}

// Helper function to determine truthiness
fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Boolean(b) => *b,
        Value::Nil => false,
        _ => true,
    }
}

impl ExprVisitor<Value> for Evaluator {
    fn visit_literal_expr(&mut self, literal: &Literal) -> EvalResult<Value> {
        match literal {
            Literal::Number(n) => Ok(Value::Number(*n)),
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::True => Ok(Value::Boolean(true)),
            Literal::False => Ok(Value::Boolean(false)),
            Literal::Nil => Ok(Value::Nil),
        }
    }

    fn visit_variable_expr(&mut self, name: &String) -> EvalResult<Value> {
        self.environment.borrow().get(name)
    }

    fn visit_unary_expr(&mut self, op: &UnaryOperator, expr: &Expr) -> EvalResult<Value> {
        let right = expr.accept(self)?;
        match (op, right) {
            (UnaryOperator::Negate, Value::Number(n)) => Ok(Value::Number(-n)),
            (UnaryOperator::Not, value) => Ok(Value::Boolean(!is_truthy(&value))),
            (UnaryOperator::Negate, _) => Err(EvalError::TypeError {
                message: "Operand must be a number.".to_string(),
                line: 0,
            }),
        }
    }

    fn visit_binary_expr(&mut self, left: &Expr, op: &Operator, right: &Expr) -> EvalResult<Value> {
        let left_value = left.accept(self)?;
        let right_value = right.accept(self)?;

        match (op, left_value, right_value) {
            // Arithmetic operations
            (Operator::Plus, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
            (Operator::Minus, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
            (Operator::Star, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
            (Operator::Slash, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
            // String concatenation
            (Operator::Plus, Value::String(l), Value::String(r)) => Ok(Value::String(l + &r)),
            // Comparison operators
            (Operator::Greater, Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l > r)),
            (Operator::GreaterEqual, Value::Number(l), Value::Number(r)) => {
                Ok(Value::Boolean(l >= r))
            }
            (Operator::Less, Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l < r)),
            (Operator::LessEqual, Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l <= r)),
            // Equality operators
            (Operator::Equal, l, r) => Ok(Value::Boolean(l == r)),
            (Operator::NotEqual, l, r) => Ok(Value::Boolean(l != r)),
            // Type errors
            (Operator::Plus, _, _) => Err(EvalError::TypeError {
                message: "Operands must be two numbers or two strings.".to_string(),
                line: 0,
            }),
            (Operator::Minus | Operator::Star | Operator::Slash, _, _) => {
                Err(EvalError::TypeError {
                    message: "Operands must be numbers.".to_string(),
                    line: 0,
                })
            }
            (
                Operator::Greater | Operator::GreaterEqual | Operator::Less | Operator::LessEqual,
                _,
                _,
            ) => Err(EvalError::TypeError {
                message: "Operands must be numbers.".to_string(),
                line: 0,
            }),
        }
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> EvalResult<Value> {
        expr.accept(self)
    }

    fn visit_assign_expr(&mut self, name: &String, value: &Expr) -> EvalResult<Value> {
        let evaluated_value = value.accept(self)?;
        self.environment
            .borrow_mut()
            .assign(name, evaluated_value.clone())?;
        Ok(evaluated_value)
    }
}

impl StmtVisitor<()> for Evaluator {
    fn visit_expression_stmt(&mut self, expr: &Expr) -> EvalResult<()> {
        expr.accept(self)?;
        Ok(())
    }

    fn visit_print_stmt(&mut self, expr: &Expr) -> EvalResult<()> {
        let value = expr.accept(self)?;
        self.output.borrow_mut().write(&value.to_string());
        Ok(())
    }

    fn visit_var_stmt(&mut self, name: &String, initializer: &Option<Expr>) -> EvalResult<()> {
        let value = if let Some(expr) = initializer {
            expr.accept(self)?
        } else {
            Value::Nil
        };
        self.environment.borrow_mut().define(name, value);
        Ok(())
    }

    fn visit_block_stmt(&mut self, statements: &Vec<Stmt>) -> EvalResult<()> {
        let new_env = Environment::new_enclosing(self.environment.clone());
        self.execute_block(statements, new_env)
    }

    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: &Option<Box<Stmt>>,
    ) -> EvalResult<()> {
        let condition_value = condition.accept(self)?;
        if is_truthy(&condition_value) {
            then_branch.accept(self)
        } else if let Some(else_branch) = else_branch {
            else_branch.accept(self)
        } else {
            Ok(())
        }
    }

    fn visit_while_stmt(&mut self, condition: &Expr, body: &Stmt) -> EvalResult<()> {
        while is_truthy(&condition.accept(self)?) {
            body.accept(self)?;
        }
        Ok(())
    }
}

pub trait Output {
    fn write(&mut self, s: &str);
}

// Standard output implementation
pub struct StandardOutput;

impl Output for StandardOutput {
    fn write(&mut self, s: &str) {
        println!("{}", s);
    }
}
