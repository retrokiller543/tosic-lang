use crate::token::Token;

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens }
    }

    pub fn parse(&mut self) {
        for token in &self.tokens {
            if token == &Token::EOF {
                break;
            }

            println!("{}", token);
        }
    }
}
