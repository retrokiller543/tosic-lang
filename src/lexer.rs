use crate::token::Token;

pub struct Lexer;

impl Lexer {
    pub fn lex(code: String) -> anyhow::Result<Vec<Token>> {
        let mut tokens = Vec::new();
        let lines = code.lines();
        let mut invalid_tokens = false;

        for (i, line) in lines.enumerate() {
            let mut chars = line.chars().peekable();

            while let Some(c) = chars.next() {
                if c.is_whitespace() {
                    continue;
                }

                if c == '=' && chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::EqualEqual);
                    continue;
                }

                if c == '!' && chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::BangEqual);
                    continue;
                }

                if c == '<' && chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::LessEqual);
                    continue;
                }

                if c == '>' && chars.peek() == Some(&'=') {
                    chars.next();
                    tokens.push(Token::GreaterEqual);
                    continue;
                }

                if c == '/' && chars.peek() == Some(&'/') {
                    chars.next();
                    while let Some(c) = chars.next() {
                        if c == '\n' {
                            break;
                        }
                    }
                    continue;
                }

                match Token::from_string(&c.to_string(), i + 1) {
                    Ok(token) => tokens.push(token),
                    Err(err) => {
                        eprintln!("{}", err);
                        invalid_tokens = true;
                    },
                }
            }
        }

        tokens.push(Token::EOF);

        Self::print_tokens(&tokens);

        if invalid_tokens {
            return Err(anyhow::anyhow!("Invalid tokens"));
        }

        Ok(tokens)
    }

    fn print_tokens(tokens: &Vec<Token>) {
        for token in tokens {
            println!("{}", token);
        }
    }
}