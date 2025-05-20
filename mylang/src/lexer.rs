use crate::token::Token;
use std::collections::HashMap;

fn keywords() -> HashMap<String, Token> {
    let mut keywords = HashMap::new();
    keywords.insert("fn".to_string(), Token::Function);
    keywords.insert("let".to_string(), Token::Let);
    keywords.insert("class".to_string(), Token::Class);
    keywords
}

pub struct Lexer {
    input: String,
    position: usize,      // current position in input (points to current char)
    read_position: usize, // current reading position in input (after current char)
    ch: u8,               // current char under examination
    keywords: HashMap<String, Token>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input,
            keywords: keywords(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0; // ASCII code for "NUL" character, signifies EOF
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            b'=' => Token::Assign,
            b'+' => Token::Plus,
            b',' => Token::Comma,
            b';' => Token::Semicolon,
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b'.' => Token::Dot,
            0 => Token::Eof,
            _ => {
                if is_letter(self.ch) {
                    let ident = self.read_identifier();
                    // Check if the identifier is a keyword
                    if let Some(keyword_token) = self.keywords.get(&ident) {
                        return keyword_token.clone();
                    }
                    return Token::Ident(ident);
                } else if is_digit(self.ch) {
                    return Token::Int(self.read_number());
                } else {
                    Token::Illegal
                }
            }
        };

        self.read_char();
        tok
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token();
            if tok == Token::Eof {
                tokens.push(tok);
                break;
            }
            tokens.push(tok);
        }
        tokens
    }
}

fn is_letter(ch: u8) -> bool {
    ch.is_ascii_alphabetic() || ch == b'_'
}

fn is_digit(ch: u8) -> bool {
    ch.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;

    #[test]
    fn test_next_token_simple() {
        let input = "let five = 5;";
        let expected_tokens = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input.to_string());
        let actual_tokens = lexer.lex();

        assert_eq!(actual_tokens, expected_tokens);
    }
}
