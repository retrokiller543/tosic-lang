// evaluator.rs or value.rs
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => {
                let n_str = n.to_string();
                if n_str.ends_with(".0") {
                    write!(f, "{}", n_str)
                } else if !n_str.contains('.') {
                    write!(f, "{}.0", n_str)
                } else {
                    write!(f, "{}", n_str.trim_end_matches('0').trim_end_matches('.'))
                }
            },
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
        }
    }
}
