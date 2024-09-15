pub mod error;

use crate::error::TokenError;
use std::borrow::Cow;
use tokens::{Reserved, Token, TokenKind};

pub struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    current_line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
            current_line: 1,
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token<'a>>, Vec<Token<'a>>> {
        let mut tokens = Vec::new();
        let mut has_errors = false;

        for token in &mut *self {
            if let Err(err) = token {
                has_errors = true;

                eprintln!("{}", err);

                continue;
            }

            tokens.push(token.unwrap());
        }

        tokens.push(Token {
            kind: TokenKind::EOF,
            line: self.current_line,
        });

        if has_errors {
            Err(tokens)
        } else {
            Ok(tokens)
        }
    }

    fn lex_next(&mut self) -> anyhow::Result<Token<'a>> {
        while let Some(&c) = self.chars.peek() {
            if c.is_whitespace() && self.chars.peek().is_some() {
                if c == '\n' {
                    self.current_line += 1;
                }

                self.chars.next();

                continue;
            }

            match c {
                '(' => return Ok(self.consume(TokenKind::LeftParen)),
                ')' => return Ok(self.consume(TokenKind::RightParen)),
                '{' => return Ok(self.consume(TokenKind::LeftBrace)),
                '}' => return Ok(self.consume(TokenKind::RightBrace)),
                '*' => return Ok(self.consume(TokenKind::Star)),
                '.' => return Ok(self.consume(TokenKind::Dot)),
                ',' => return Ok(self.consume(TokenKind::Comma)),
                '+' => return Ok(self.consume(TokenKind::Plus)),
                '-' => return Ok(self.consume(TokenKind::Minus)),
                ';' => return Ok(self.consume(TokenKind::Semicolon)),
                '/' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'/') {
                        self.skip_comment();
                    } else {
                        return Ok(Token {
                            kind: TokenKind::Slash,
                            line: self.current_line,
                        });
                    }
                }
                '=' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        return Ok(Token {
                            kind: TokenKind::EqualEqual,
                            line: self.current_line,
                        });
                    } else {
                        return Ok(Token {
                            kind: TokenKind::Equal,
                            line: self.current_line,
                        });
                    }
                }
                '!' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        return Ok(Token {
                            kind: TokenKind::BangEqual,
                            line: self.current_line,
                        });
                    } else {
                        return Ok(Token {
                            kind: TokenKind::Bang,
                            line: self.current_line,
                        });
                    }
                }
                '<' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        return Ok(Token {
                            kind: TokenKind::LessEqual,
                            line: self.current_line,
                        });
                    } else {
                        return Ok(Token {
                            kind: TokenKind::Less,
                            line: self.current_line,
                        });
                    }
                }
                '>' => {
                    self.chars.next();
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        return Ok(Token {
                            kind: TokenKind::GreaterEqual,
                            line: self.current_line,
                        });
                    } else {
                        return Ok(Token {
                            kind: TokenKind::Greater,
                            line: self.current_line,
                        });
                    }
                }
                '"' => return self.lex_string(),
                c if c.is_ascii_digit() => return self.lex_number(),
                c if c.is_alphanumeric() || c == '_' => return self.lex_identifier(),
                _ => {
                    self.chars.next();
                    return Err(TokenError::InvalidToken(c.to_string(), self.current_line).into());
                }
            }
        }

        Ok(Token {
            kind: TokenKind::EOF,
            line: self.current_line,
        })
    }

    fn consume(&mut self, kind: TokenKind<'a>) -> Token<'a> {
        self.chars.next();
        Token {
            kind,
            line: self.current_line,
        }
    }

    fn skip_comment(&mut self) {
        for c in self.chars.by_ref() {
            if c == '\n' {
                self.current_line += 1;
                break;
            }
        }
    }

    fn lex_string(&mut self) -> anyhow::Result<Token<'a>> {
        self.chars.next(); // Consume the opening "
        let mut s = String::new();

        while let Some(&c) = self.chars.peek() {
            if c == '"' {
                self.chars.next();
                return Ok(Token {
                    kind: TokenKind::LitStr(Cow::Owned(s)),
                    line: self.current_line,
                });
            } else if c == '\n' {
                return Err(TokenError::UnterminatedString(self.current_line).into());
            }
            s.push(c);
            self.chars.next();
        }

        Err(TokenError::UnterminatedString(self.current_line).into())
    }

    fn lex_number(&mut self) -> anyhow::Result<Token<'a>> {
        let mut s = String::new();
        while let Some(&c) = self.chars.peek() {
            if c.is_ascii_digit() || c == '.' {
                s.push(c);
                self.chars.next();
            } else {
                break;
            }
        }

        Ok(Token {
            kind: TokenKind::LitNum(s.parse()?),
            line: self.current_line,
        })
    }

    fn lex_identifier(&mut self) -> anyhow::Result<Token<'a>> {
        let mut s = String::new();

        while let Some(&c) = self.chars.peek() {
            if c.is_alphanumeric() || c == '_' {
                s.push(c);
                self.chars.next();
            } else {
                break;
            }
        }

        if let Ok(reserved) = s.parse::<Reserved>() {
            Ok(Token {
                kind: TokenKind::Reserved(reserved),
                line: self.current_line,
            })
        } else {
            Ok(Token {
                kind: TokenKind::Ident(Cow::from(s)),
                line: self.current_line,
            })
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = anyhow::Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lex_next() {
            Ok(Token {
                kind: TokenKind::EOF,
                ..
            }) => None,
            token => Some(token),
        }
    }
}
