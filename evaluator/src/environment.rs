use statement::error::EvalError;
use statement::value::Value;
use std::collections::HashMap;

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_enclosing(enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }))
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Result<(), EvalError> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            Ok(())
        } else if let Some(ref mut enclosing) = self.enclosing {
            enclosing.borrow_mut().assign(name, value)
        } else {
            Err(EvalError::UndefinedVariable {
                name: name.to_string(),
                line: 0,
            })
        }
    }

    pub fn get(&self, name: &str) -> Result<Value, EvalError> {
        if let Some(value) = self.values.get(name) {
            Ok(value.clone())
        } else if let Some(ref enclosing) = self.enclosing {
            enclosing.borrow().get(name)
        } else {
            Err(EvalError::UndefinedVariable {
                name: name.to_string(),
                line: 0,
            })
        }
    }
}
