use crate::error::TokenError;
use crate::token::Token;
use anyhow::Result;
use std::borrow::Cow;

pub struct Lexer<'a> {
    #[allow(dead_code)]
    input: &'a str,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    current_line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.chars().peekable(),
            current_line: 1,
        }
    }

    fn lex_next(&mut self) -> Result<Token<'a>> {
        while let Some(&c) = self.chars.peek() {
            if c.is_whitespace() && self.chars.peek().is_some() {
                self.chars.next();
                continue;
            }

            match c {
                '(' => return Ok(self.consume(Token::LeftParen)),
                ')' => return Ok(self.consume(Token::RightParen)),
                '{' => return Ok(self.consume(Token::LeftBrace)),
                '}' => return Ok(self.consume(Token::RightBrace)),
                '*' => return Ok(self.consume(Token::Star)),
                '.' => return Ok(self.consume(Token::Dot)),
                ',' => return Ok(self.consume(Token::Comma)),
                '+' => return Ok(self.consume(Token::Plus)),
                '-' => return Ok(self.consume(Token::Minus)),
                ';' => return Ok(self.consume(Token::Semicolon)),
                '/' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'/') {
                        self.skip_comment();
                    } else {
                        return Ok(Token::Slash);
                    }
                }
                '=' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        return Ok(Token::EqualEqual);
                    } else {
                        return Ok(Token::Equal);
                    }
                }
                '!' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        return Ok(Token::BangEqual);
                    } else {
                        return Ok(Token::Bang);
                    }
                }
                '<' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        return Ok(Token::LessEqual);
                    } else {
                        return Ok(Token::Less);
                    }
                }
                '>' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        return Ok(Token::GreaterEqual);
                    } else {
                        return Ok(Token::Greater);
                    }
                }
                '"' => return self.lex_string(),
                c if c.is_ascii_digit() => return self.lex_number(),
                _ => {
                    self.chars.next();
                    return Err(TokenError::InvalidToken(c.to_string(), self.current_line).into());
                }
            }
        }

        Ok(Token::EOF)
    }

    fn consume(&mut self, token: Token<'a>) -> Token<'a> {
        self.chars.next();
        token
    }

    fn skip_comment(&mut self) {
        for c in self.chars.by_ref() {
            if c == '\n' {
                self.current_line += 1;
                break;
            }
        }
    }

    fn lex_string(&mut self) -> Result<Token<'a>> {
        self.chars.next(); // Consume the opening "
        let mut s = String::new();

        while let Some(&c) = self.chars.peek() {
            if c == '"' {
                self.chars.next(); // Consume closing "
                return Ok(Token::LitStr(Cow::Owned(s)));
            } else if c == '\n' {
                return Err(TokenError::UnterminatedString(self.current_line).into());
            }
            s.push(c);
            self.chars.next();
        }

        Err(TokenError::UnterminatedString(self.current_line).into())
    }

    fn lex_number(&mut self) -> Result<Token<'a>> {
        let mut s = String::new();
        while let Some(&c) = self.chars.peek() {
            if c.is_ascii_digit() || c == '.' {
                s.push(c);
                self.chars.next();
            } else {
                break;
            }
        }

        Ok(Token::LitNum(s))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lex_next() {
            Ok(Token::EOF) => None,
            token => Some(token),
        }
    }
}
