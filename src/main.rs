mod error;
mod lexer;
mod parser;
pub mod token;
mod evaluator;
mod value;

use crate::lexer::Lexer;
use std::env;
use std::fs;
use std::process::exit;

fn read_file(filename: &str) -> String {
    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!("Failed to read file {}", filename);
        String::new()
    });

    if file_contents.is_empty() {
        exit(1);
    }

    file_contents
}

fn lex_file(file_contents: &str) -> Vec<token::Token> {
    let mut lexer = Lexer::new(file_contents);
    lexer.lex().unwrap_or_else(|_| {
        exit(65);
    })
}

fn parse_file(filename: &str) -> Vec<(parser::Expr, usize)> {
    let file_contents = read_file(filename);
    let tokens = lex_file(&file_contents);
    parser::Parser::new(tokens).parse().unwrap_or_else(|err| {
        eprintln!("{}", err);
        exit(65);
    })
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = read_file(filename);
            let tokens = lex_file(&file_contents);

            for token in tokens {
                println!("{:?}", token);
            }
        }
        "parse" => {
            let expressions = parse_file(filename);

            for (expr, _) in expressions {
                println!("{}", expr);
            }
        }
        "evaluate" => {
            let expressions = parse_file(filename);

            let evaluator = evaluator::Evaluator::new(expressions);

            evaluator.evaluate().unwrap_or_else(|err| {
                eprintln!("{}", err);
                exit(65);
            })
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
