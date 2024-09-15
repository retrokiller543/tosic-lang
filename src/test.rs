#![cfg(test)]

use super::*;
use evaluator::{Evaluator, Output};
use lexer::Lexer;
use parser::Parser;
use statement::error::EvalError;
use std::cell::RefCell;
use std::rc::Rc;

// Custom output handler for capturing printed output
struct TestOutput {
    pub output: Vec<String>,
}

impl Output for TestOutput {
    fn write(&mut self, s: &str) {
        self.output.push(s.to_string());
    }
}

// Helper function to evaluate code and capture output
fn evaluate_code(code: &str) -> Result<Vec<String>, EvalError> {
    // Lex the code
    let mut lexer = Lexer::new(code);
    let tokens = lexer
        .lex()
        .map_err(|_err| EvalError::GenericError("Lexing error".to_string()))?;

    // Parse the code
    let mut parser = Parser::new(tokens);
    let statements = parser
        .parse()
        .map_err(|err| EvalError::GenericError(err.to_string()))?;

    // Set up the evaluator with custom output
    let test_output = Rc::new(RefCell::new(TestOutput { output: Vec::new() }));
    let mut evaluator = Evaluator::new_with_output(test_output.clone());

    // Evaluate the statements
    evaluator
        .evaluate(&statements)
        .map_err(|err| EvalError::GenericError(err.to_string()))?;

    // Retrieve the captured output
    let output = test_output.borrow().output.clone();
    Ok(output)
}

#[test]
fn test_variable_declaration_and_printing() {
    let code = r#"
            var x = 10;
            print x;
        "#;

    let output = evaluate_code(code).expect("Evaluation failed");
    assert_eq!(output, vec!["10"]);
}

#[test]
fn test_arithmetic_expressions() {
    let code = r#"
            print 2 + 3 * 4 - 5 / (1 + 1);
        "#;

    let output = evaluate_code(code).expect("Evaluation failed");
    assert_eq!(output, vec!["11.5"]);
}

#[test]
fn test_comparison_and_logical_expressions() {
    let code = r#"
            print 5 > 3;
            print 5 <= 3;
            print true == false;
        "#;

    let output = evaluate_code(code).expect("Evaluation failed");
    assert_eq!(output, vec!["true", "false", "false"]);
}

#[test]
fn test_if_statement_true_condition() {
    let code = r#"
            if (true) {
                print "This should print.";
            } else {
                print "This should not print.";
            }
        "#;

    let output = evaluate_code(code).expect("Evaluation failed");
    assert_eq!(output, vec!["This should print."]);
}

#[test]
fn test_if_statement_false_condition() {
    let code = r#"
            if (false) {
                print "This should not print.";
            } else {
                print "This should print.";
            }
        "#;

    let output = evaluate_code(code).expect("Evaluation failed");
    assert_eq!(output, vec!["This should print."]);
}

#[test]
fn test_while_loop() {
    let code = r#"
            var i = 0;
            while (i < 3) {
                print i;
                i = i + 1;
            }
        "#;

    let output = evaluate_code(code).expect("Evaluation failed");
    assert_eq!(output, vec!["0", "1", "2"]);
}

#[test]
fn test_block_and_scope() {
    let code = r#"
            var a = "global a";
            {
                var a = "local a";
                print a; // Should print "local a"
            }
            print a; // Should print "global a"
        "#;

    let output = evaluate_code(code).expect("Evaluation failed");
    assert_eq!(output, vec!["local a", "global a"]);
}

#[test]
fn test_undefined_variable_error() {
    let code = r#"
            print undefinedVar;
        "#;

    let result = evaluate_code(code);
    assert!(result.is_err());
    if let Err(EvalError::UndefinedVariable { name, .. }) = result {
        assert_eq!(name, "undefinedVar");
    } else {
        panic!("Expected UndefinedVariable error");
    }
}

#[test]
fn test_type_error_in_arithmetic() {
    let code = r#"
            print "string" + 5;
        "#;

    let result = evaluate_code(code);
    assert!(result.is_err());
    if let Err(EvalError::TypeError { message, .. }) = result {
        assert!(message.contains("Operands must be two numbers or two strings."));
    } else {
        panic!("Expected TypeError");
    }
}

#[test]
fn test_variable_assignment() {
    let code = r#"
            var x;
            x = 42;
            print x;
        "#;

    let output = evaluate_code(code).expect("Evaluation failed");
    assert_eq!(output, vec!["42"]);
}
