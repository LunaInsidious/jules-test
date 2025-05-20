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
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char(); // Consume the second '='
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char(); // Consume the '='
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            b'*' => Token::Asterisk,
            b'/' => Token::Slash,
            b'%' => Token::Percent,
            b'<' => {
                if self.peek_char() == b'=' {
                    self.read_char(); // Consume the '='
                    Token::LTEq
                } else {
                    Token::LT
                }
            }
            b'>' => {
                if self.peek_char() == b'=' {
                    self.read_char(); // Consume the '='
                    Token::GTEq
                } else {
                    Token::GT
                }
            }
            b',' => Token::Comma,
            b';' => Token::Semicolon,
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b'.' => Token::Dot,
            b'"' => {
                if let Some(string_val) = self.read_string() {
                    Token::String(string_val)
                } else {
                    Token::Illegal // Unterminated string
                }
            }
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

    // Returns Some(String) if terminated, None if unterminated (hits EOF)
    fn read_string(&mut self) -> Option<String> {
        let mut value = String::new();
        // Current self.ch is the opening quote.
        // The loop will call read_char() to get the next character for processing.
        
        loop {
            self.read_char(); // Read the next character from input.

            if self.ch == b'"' { // End of string
                return Some(value);
            } else if self.ch == b'\\' { // Potential escape sequence
                self.read_char(); // Read the character *after* the backslash
                match self.ch {
                    b'"'  => value.push('"'),
                    b'\\' => value.push('\\'),
                    0 => return None, // EOF during escape sequence (unterminated)
                    // Any other character after \: treat \ and that char literally
                    other_char => {
                        value.push('\\');
                        value.push(other_char as char);
                    }
                }
            } else if self.ch == 0 { // EOF, but not a closing quote (and not part of escape)
                return None; // Unterminated string
            } else { // Normal character
                value.push(self.ch as char);
            }
        }
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
        let input = r#"let five = 5;"#; 
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

    #[test]
    fn test_string_literal_lexing() {
        struct TestCase {
            input: &'static str,
            expected_tokens: Vec<Token>,
        }

        let tests = vec![
            TestCase {
                input: r#""hello world""#,
                expected_tokens: vec![
                    Token::String("hello world".to_string()),
                    Token::Eof,
                ],
            },
            TestCase {
                input: r#""""#, // Empty string
                expected_tokens: vec![
                    Token::String("".to_string()),
                    Token::Eof,
                ],
            },
            TestCase {
                input: r#""\"escaped\"""#, // "escaped" - current lexer reads \ as literal
                expected_tokens: vec![
                    Token::String("\"escaped\"".to_string()), 
                    Token::Eof,
                ],
            },
            TestCase {
                input: r#""unterminated"#, // Unterminated string
                expected_tokens: vec![
                    Token::Illegal, // Because read_string returns None, next_token makes it Illegal
                    Token::Eof,     // After Illegal, next call to next_token (if any) on "" would yield EOF
                                    // The current lexer.lex() continues until explicit EOF token
                                    // If Token::Illegal is returned, read_char() is not called again in that next_token call.
                                    // The subsequent next_token() call in lex() will then see self.ch as 0 (EOF)
                                    // because read_string consumed all input.
                ],
            },
             TestCase { // Test case: unterminated string then more content.
                input: r#""unterminated then space"#,
                // Expected: Illegal (for the unterminated string part),
                // then the lexer effectively stops or should try to recover.
                // Current `lex()` loop:
                // 1. next_token() on `"`: calls read_string. read_string consumes to EOF, returns None. next_token returns Illegal.
                // 2. `lex()` loop pushes Illegal.
                // 3. next_token() again: self.ch is 0 (EOF). next_token returns Token::Eof.
                // 4. `lex()` loop pushes Eof and breaks.
                // So, the " then space" part is never tokenized. This is acceptable for current error handling.
                expected_tokens: vec![
                    Token::Illegal,
                    Token::Eof,
                ],
            },
        ];

        for tt in tests {
            let mut lexer = Lexer::new(tt.input.to_string());
            let mut actual_tokens = Vec::new();
            loop {
                let tok = lexer.next_token();
                let is_eof = tok == Token::Eof;
                actual_tokens.push(tok);
                if is_eof {
                    break;
                }
            }
            assert_eq!(actual_tokens, tt.expected_tokens, "Input: '{}'", tt.input);
        }
    }
}
