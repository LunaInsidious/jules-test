use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Token {
    Illegal,
    Eof,
    Ident(String),
    Int(String),
    String(String), // For string literals

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Percent,
    LT, // <
    GT, // >
    Eq, // ==
    NotEq, // !=
    LTEq, // <=
    GTEq, // >=
    // TODO: Add other operators if needed

    // Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Dot,

    // Keywords
    Function,
    Let,
    Class,
    // TODO: Add True, False, If, Else, Return
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Illegal => write!(f, "ILLEGAL"),
            Token::Eof => write!(f, "EOF"),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Int(s) => write!(f, "{}", s),
            Token::String(s) => write!(f, "{}", s), // Note: fmt::Display for String(s) should probably be `s` not `"s"`
                                                    // if it represents the token's value/content.
                                                    // If it's meant to be the literal form as seen in code, then `format!("\"{}\"", s)`
                                                    // But the prompt example `Token::String(s) -> s` suggests the content.
            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::LT => write!(f, "<"),
            Token::GT => write!(f, ">"),
            Token::Eq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::LTEq => write!(f, "<="),
            Token::GTEq => write!(f, ">="),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Dot => write!(f, "."),
            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Class => write!(f, "class"),
        }
    }
}
