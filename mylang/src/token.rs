#[derive(Debug, PartialEq, Clone)]
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Token {
    Illegal,
    Eof,
    Ident(String),
    Int(String),
    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    Class, // New token for 'class' keyword
    Dot,   // New token for '.' operator
    String(String), // For string literals
}
