use std::error::Error;

// Import your modules here
// Assuming your modules are named accordingly

fn main() -> Result<(), Box<dyn Error>> {
    // Define test cases as pairs of description and code snippet
    let test_cases = vec![
        (
            "Test Variable Declaration and Printing",
            r#"
            var x = 10;
            print x;
            "#,
        ),
        (
            "Test Arithmetic Expressions",
            r#"
            print 2 + 3 * 4 - 5 / (1 + 1);
            "#,
        ),
        (
            "Test Comparison and Logical Expressions",
            r#"
            print 5 > 3;
            print 5 <= 3;
            print true == false;
            print (5 > 3) and (2 < 4);
            "#,
        ),
        (
            "Test If Statement (True Condition)",
            r#"
            if (true) {
                print "This should print.";
            } else {
                print "This should not print.";
            }
            "#,
        ),
        (
            "Test If Statement (False Condition)",
            r#"
            if (false) {
                print "This should not print.";
            } else {
                print "This should print.";
            }
            "#,
        ),
        (
            "Test While Loop",
            r#"
            var i = 0;
            while (i < 3) {
                print i;
                i = i + 1;
            }
            "#,
        ),
        (
            "Test Block and Scope",
            r#"
            var a = "global a";
            {
                var a = "local a";
                print a; // Should print "local a"
            }
            print a; // Should print "global a"
            "#,
        ),
        (
            "Test Undefined Variable Error",
            r#"
            print undefinedVar;
            "#,
        ),
        (
            "Test Type Error in Arithmetic",
            r#"
            print "string" + 5;
            "#,
        ),
        (
            "Test Variable Assignment",
            r#"
            var x;
            x = 42;
            print x;
            "#,
        ),
        // Add more test cases as needed
    ];

    for (description, code) in test_cases {
        println!("=== {} ===", description);
        println!("Code:\n{}\n", code.trim());

        // Tokenize the code
        let tokens = match tokenize(code) {
            Ok(tokens) => tokens,
            Err(e) => {
                eprintln!("Tokenizer Error: {:?}", e);
                continue;
            }
        };

        // Parse the tokens
        let mut parser = Parser::new(tokens);
        let statements = match parser.parse() {
            Ok(statements) => statements,
            Err(e) => {
                eprintln!("Parser Error: {:?}", e);
                continue;
            }
        };

        // Evaluate the statements
        let mut evaluator = Evaluator::new();
        match evaluator.evaluate(&statements) {
            Ok(_) => {}
            Err(EvalError::UndefinedVariable { name, line }) => {
                eprintln!("Runtime Error: Undefined variable '{}' at line {}", name, line);
            }
            Err(EvalError::TypeError { message, line }) => {
                eprintln!("Runtime Error: Type error at line {}: {}", line, message);
            }
            Err(e) => {
                eprintln!("Runtime Error: {:?}", e);
            }
        }

        println!("\n"); // Add space between test cases
    }

    Ok(())
}
