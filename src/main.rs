mod error;
mod lexer;
pub mod token;

use crate::lexer::Lexer;
use std::env;
use std::fs;

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
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            if !file_contents.is_empty() {
                let lexer = Lexer::new(&file_contents);
                let mut has_errors = false;

                for token_result in lexer {
                    match token_result {
                        Ok(token) => println!("{}", token),
                        Err(err) => {
                            eprintln!("{}", err);
                            has_errors = true;
                        },
                    }
                }

                println!("EOF  null");

                if has_errors {
                    std::process::exit(65);
                }
            } else {
                println!("EOF  null"); // Placeholder, remove this line when implementing the scanner
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
