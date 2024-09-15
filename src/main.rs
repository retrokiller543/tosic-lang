mod test;

use clap::{Parser, Subcommand};
use lexer::Lexer;
use statement::{Expr, Stmt};
use std::fs;
use std::process::exit;
use tokens::Token;

#[derive(Parser)]
#[command(author, version, about = "Interpreter for the Lox language")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Tokenize the input file
    #[command(about = "Tokenize the input file", alias = "token")]
    Tokenize {
        /// The input file to tokenize
        filename: String,
    },
    /// Parse the input file
    #[command(about = "Parse the input file")]
    Parse {
        /// The input file to parse
        filename: String,
    },
    /// Evaluate the input file
    #[command(about = "Evaluate expressions the input file", alias = "eval")]
    Evaluate {
        /// The input file to evaluate
        filename: String,
    },
    /// Run the input file
    #[command(about = "Run the input file")]
    Run { filename: String },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Tokenize { filename } => {
            tokenize_file(filename);
        }
        Commands::Parse { filename } => {
            parse_file_command(filename);
        }
        Commands::Evaluate { filename } => {
            evaluate_file(filename);
        }
        Commands::Run { filename } => {
            parse_statements(filename);
        }
    }
}

fn parse_statements(filename: &str) {
    let statements = match statements_file(filename) {
        Ok(statements) => statements,
        Err(err) => {
            eprintln!("Parsing error: {}", err);
            exit(70);
        }
    };

    let mut evaluator = evaluator::Evaluator::new();

    match evaluator.evaluate(&statements) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Evaluation error: {}", err);
            exit(70);
        }
    }
}

fn read_file(filename: &str) -> Result<String, std::io::Error> {
    let file_contents = fs::read_to_string(filename)?;

    if file_contents.trim().is_empty() {
        println!("EOF null");
        exit(0);
    }

    Ok(file_contents)
}

fn lex_file(file_contents: &str) -> (Vec<Token>, bool) {
    let mut lexer = Lexer::new(file_contents);
    let mut has_error = false;

    (
        lexer.lex().unwrap_or_else(|tokens| {
            has_error = true;
            tokens
        }),
        has_error,
    )
}

fn parse_file(filename: &str) -> Result<Vec<(Expr, usize)>, String> {
    let file_contents = read_file(filename).map_err(|e| e.to_string())?;

    let (tokens, errors) = lex_file(&file_contents);

    if errors {
        return Err("Lexing errors occurred.".to_string());
    }

    parser::Parser::new(tokens)
        .parse_expressions()
        .map_err(|err| err.to_string())
}

fn statements_file(filename: &str) -> Result<Vec<Stmt>, String> {
    let file_contents = read_file(filename).map_err(|e| e.to_string())?;

    let (tokens, errors) = lex_file(&file_contents);

    if errors {
        return Err("Lexing errors occurred.".to_string());
    }

    parser::Parser::new(tokens)
        .parse()
        .map_err(|err| err.to_string())
}

fn tokenize_file(filename: &str) {
    let file_contents = match read_file(filename) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("Failed to read file {}: {}", filename, err);
            exit(1);
        }
    };

    let (tokens, errors) = lex_file(&file_contents);

    for token in tokens {
        println!("{:?}", token);
    }

    if errors {
        exit(65);
    }
}

fn parse_file_command(filename: &str) {
    let expressions = match parse_file(filename) {
        Ok(exprs) => exprs,
        Err(err) => {
            eprintln!("Parsing error: {}", err);
            exit(65);
        }
    };

    for (expr, _) in expressions {
        println!("{}", expr);
    }
}

fn evaluate_file(filename: &str) {
    let expressions = match parse_file(filename) {
        Ok(exprs) => exprs,
        Err(err) => {
            eprintln!("Parsing error: {}", err);
            exit(65);
        }
    };

    let mut evaluator = evaluator::Evaluator::new();

    for (expr, _) in expressions {
        match evaluator.evaluate_expr(&expr) {
            Ok(value) => println!("{}", value),
            Err(err) => {
                eprintln!("Evaluation error: {}", err);
                exit(70);
            }
        }
    }
}
