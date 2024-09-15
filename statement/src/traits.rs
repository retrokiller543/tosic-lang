use crate::error::EvalError;
use crate::{Expr, Literal, Operator, Stmt, UnaryOperator};

// Define a Result type alias for convenience
pub type EvalResult<T> = std::result::Result<T, EvalError>;

pub trait ExprVisitor<T> {
    fn visit_literal_expr(&mut self, literal: &Literal) -> EvalResult<T>;
    fn visit_variable_expr(&mut self, name: &String) -> EvalResult<T>;
    fn visit_unary_expr(&mut self, op: &UnaryOperator, right: &Expr) -> EvalResult<T>;
    fn visit_binary_expr(&mut self, left: &Expr, op: &Operator, right: &Expr) -> EvalResult<T>;
    fn visit_grouping_expr(&mut self, expr: &Expr) -> EvalResult<T>;
    fn visit_assign_expr(&mut self, name: &String, value: &Expr) -> EvalResult<T>;
}

pub trait StmtVisitor<T> {
    fn visit_expression_stmt(&mut self, expr: &Expr) -> EvalResult<T>;
    fn visit_print_stmt(&mut self, expr: &Expr) -> EvalResult<T>;
    fn visit_var_stmt(&mut self, name: &String, initializer: &Option<Expr>) -> EvalResult<T>;
    fn visit_block_stmt(&mut self, statements: &Vec<Stmt>) -> EvalResult<T>;
    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: &Option<Box<Stmt>>,
    ) -> EvalResult<T>;
    fn visit_while_stmt(&mut self, condition: &Expr, body: &Stmt) -> EvalResult<T>;
}
