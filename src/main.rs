mod error;
pub mod token;
mod lexer;

use std::env;
use std::fs;
use std::process::exit;

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
                let _ = match lexer::Lexer::lex(file_contents) {
                    Ok(_) => (),
                    Err(_) => exit(65),
                };
            } else {
                println!("EOF  null"); // Placeholder, remove this line when implementing the scanner
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
