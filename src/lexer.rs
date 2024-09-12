use crate::error::TokenError;
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

                if c == '"' {
                    let mut s = String::new();
                    let mut should_include = true;

                    while let Some(c) = chars.next() {
                        if c == '"' {
                            break;
                        }

                        s.push(c);

                        if chars.peek() == None {
                            invalid_tokens = true;
                            should_include = false;
                            break;
                        }
                    }

                    if should_include {
                        tokens.push(Token::LitStr(s));
                    } else {
                        let err = TokenError::UnterminatedString(i + 1);
                        eprintln!("{}", err);
                    }

                    continue;
                }

                if c.is_ascii_digit() {
                    let mut s = String::new();
                    s.push(c);

                    while let Some(c) = chars.next() {
                        if c.is_ascii_digit() || c == '.' {
                            s.push(c);
                        } else {
                            break;
                        }
                    }

                    tokens.push(Token::LitNum(s.parse()?));

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

