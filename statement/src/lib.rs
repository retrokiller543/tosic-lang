use crate::error::EvalError;
use crate::traits::{EvalResult, ExprVisitor, StmtVisitor};
use crate::value::Value;
use std::fmt::Display;
use tokens::trim_trailing_zeroes;

pub mod error;
pub mod traits;
pub mod value;

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Variable(String),
    UnaryOp(UnaryOperator, Box<Expr>),
    BinaryOp(Box<Expr>, Operator, Box<Expr>),
    Group(Box<Expr>),
    Assign(String, Box<Expr>),
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut impl ExprVisitor<T>) -> EvalResult<T> {
        match self {
            Expr::Literal(literal) => visitor.visit_literal_expr(literal),
            Expr::Variable(name) => visitor.visit_variable_expr(name),
            Expr::UnaryOp(op, expr) => visitor.visit_unary_expr(op, expr),
            Expr::BinaryOp(left, op, right) => visitor.visit_binary_expr(left, op, right),
            Expr::Group(expr) => visitor.visit_grouping_expr(expr),
            Expr::Assign(name, value) => visitor.visit_assign_expr(name, value),
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
            Expr::Assign(name, value) => write!(f, "({} = {})", name, value),
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

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut impl StmtVisitor<T>) -> EvalResult<T> {
        match self {
            Stmt::Expression(expr) => visitor.visit_expression_stmt(expr),
            Stmt::Print(expr) => visitor.visit_print_stmt(expr),
            Stmt::Var(name, initializer) => visitor.visit_var_stmt(name, initializer),
            Stmt::Block(statements) => visitor.visit_block_stmt(statements),
            Stmt::If(condition, then_branch, else_branch) => {
                visitor.visit_if_stmt(condition, then_branch, else_branch)
            }
            Stmt::While(condition, body) => visitor.visit_while_stmt(condition, body),
        }
    }
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
