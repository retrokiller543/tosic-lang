use crate::error::EvalError;
use crate::value::Value;
use std::fmt::Display;
use tokens::trim_trailing_zeroes;

pub mod error;
pub mod value;

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Variable(String),
    UnaryOp(UnaryOperator, Box<Expr>),
    BinaryOp(Box<Expr>, Operator, Box<Expr>),
    Group(Box<Expr>),
}

impl Expr {
    pub fn evaluate(&self, line: usize) -> Result<Value, EvalError> {
        match self {
            Expr::Literal(literal) => match literal {
                Literal::Number(n) => Ok(Value::Number(*n)),
                Literal::String(s) => Ok(Value::String(s.clone())),
                Literal::True => Ok(Value::Boolean(true)),
                Literal::False => Ok(Value::Boolean(false)),
                Literal::Nil => Ok(Value::Nil),
            },
            Expr::Variable(name) => {
                // Variable handling is not implemented yet
                Err(EvalError::UndefinedVariable {
                    name: name.clone(),
                    line,
                })
            }
            Expr::UnaryOp(op, expr) => {
                let value = expr.evaluate(line)?;
                match (op, value) {
                    (UnaryOperator::Negate, Value::Number(n)) => Ok(Value::Number(-n)),
                    (UnaryOperator::Not, Value::Boolean(b)) => Ok(Value::Boolean(!b)),
                    (UnaryOperator::Not, Value::Nil) => Ok(Value::Boolean(true)),
                    (UnaryOperator::Not, _) => Ok(Value::Boolean(false)),
                    (UnaryOperator::Negate, _) => Err(EvalError::TypeError {
                        message: "Operand must be a number.".to_string(),
                        line,
                    }),
                }
            }
            Expr::BinaryOp(left, op, right) => {
                let left_value = left.evaluate(line)?;
                let right_value = right.evaluate(line)?;
                match (op, left_value, right_value) {
                    // Arithmetic operations
                    (Operator::Plus, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l + r))
                    }
                    (Operator::Minus, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l - r))
                    }
                    (Operator::Star, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l * r))
                    }
                    (Operator::Slash, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(l / r))
                    }
                    // String concatenation
                    (Operator::Plus, Value::String(l), Value::String(r)) => {
                        Ok(Value::String(l + &r))
                    }
                    // Comparison operators
                    (Operator::Greater, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Boolean(l > r))
                    }
                    (Operator::GreaterEqual, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Boolean(l >= r))
                    }
                    (Operator::Less, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Boolean(l < r))
                    }
                    (Operator::LessEqual, Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Boolean(l <= r))
                    }
                    // Equality operators
                    (Operator::Equal, l, r) => Ok(Value::Boolean(l == r)),
                    (Operator::NotEqual, l, r) => Ok(Value::Boolean(l != r)),
                    // Type errors
                    (Operator::Plus, _, _) => Err(EvalError::TypeError {
                        message: "Operands must be two numbers or two strings.".to_string(),
                        line,
                    }),
                    (Operator::Minus | Operator::Star | Operator::Slash, _, _) => {
                        Err(EvalError::TypeError {
                            message: "Operands must be numbers.".to_string(),
                            line,
                        })
                    }
                    (
                        Operator::Greater
                        | Operator::GreaterEqual
                        | Operator::Less
                        | Operator::LessEqual,
                        _,
                        _,
                    ) => Err(EvalError::TypeError {
                        message: "Operands must be numbers.".to_string(),
                        line,
                    }),
                }
            }
            Expr::Group(expr) => expr.evaluate(line),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(l) => write!(f, "{}", l),
            Expr::Variable(s) => write!(f, "{}", s),
            Expr::UnaryOp(op, e) => write!(f, "({} {})", op, e),
            Expr::BinaryOp(l, op, r) => write!(f, "({} {} {})", op, l, r),
            Expr::Group(e) => write!(f, "(group {})", e),
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(String, Option<Expr>),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expression(expr) => write!(f, "{}", expr),
            Stmt::Print(expr) => write!(f, "print {}", expr),
            Stmt::Var(name, expr) => match expr {
                Some(expr) => write!(f, "var {} = {}", name, expr),
                None => write!(f, "var {}", name),
            },
            Stmt::Block(statements) => {
                for statement in statements {
                    write!(f, "{}", statement)?;
                }
                Ok(())
            }
            Stmt::If(condition, then_branch, else_branch) => {
                write!(f, "if {} {{\n{}\n}}", condition, then_branch)?;
                if let Some(else_branch) = else_branch {
                    write!(f, "else {{\n{}\n}}", else_branch)?;
                }
                Ok(())
            }
            Stmt::While(condition, body) => write!(f, "while {} {{\n{}\n}}", condition, body),
        }
    }
}

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => {
                let n = n.to_string();
                if n.ends_with(".0") {
                    write!(f, "{}", n)
                } else if !n.contains('.') {
                    write!(f, "{}.0", n)
                } else {
                    write!(f, "{}", trim_trailing_zeroes(n.as_str()))
                }
            }
            Literal::String(s) => write!(f, "{}", s),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negate,
    Not,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Negate => write!(f, "-"),
            UnaryOperator::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Star => write!(f, "*"),
            Operator::Slash => write!(f, "/"),
            Operator::Less => write!(f, "<"),
            Operator::LessEqual => write!(f, "<="),
            Operator::Greater => write!(f, ">"),
            Operator::GreaterEqual => write!(f, ">="),
            Operator::Equal => write!(f, "=="),
            Operator::NotEqual => write!(f, "!="),
        }
    }
}
