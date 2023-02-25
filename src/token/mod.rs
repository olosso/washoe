use crate::parser::Precedence;
use std::fmt;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum TokenType {
    Illegal,
    EOF,
    SOF,

    // Identifiers + literals
    Ident,
    Int,
    String,

    // Operators
    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Bang,
    Equal,
    NotEqual,
    GT,
    LT,
    GTOE,
    LTOE,

    // Delimiters
    Comma,
    Semicolon,
    Colon,

    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,

    If,
    Else,
    Return,

    True,
    False,

    // Special
    Program,
}

impl TokenType {
    pub fn string(&self) -> String {
        let s = match self {
            TokenType::Illegal => "ILLEGAL",
            TokenType::EOF => "EOF",
            TokenType::SOF => "SOF",

            TokenType::Ident => "IDENT",
            TokenType::Int => "INT",
            TokenType::String => "STRING",

            TokenType::Assign => "=",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Asterisk => "*",
            TokenType::Slash => "/",
            TokenType::Bang => "!",
            TokenType::Equal => "==",
            TokenType::NotEqual => "!=",
            TokenType::GT => ">",
            TokenType::LT => "<",
            TokenType::GTOE => ">=",
            TokenType::LTOE => "<=",

            TokenType::Comma => ",",
            TokenType::Semicolon => ";",
            TokenType::Colon => ":",

            TokenType::LParen => "(",
            TokenType::RParen => ")",
            TokenType::LBracket => "[",
            TokenType::RBracket => "]",
            TokenType::LBrace => "{",
            TokenType::RBrace => "}",

            TokenType::Function => "FUNCTION",
            TokenType::Let => "LET",

            TokenType::If => "IF",
            TokenType::Else => "ELSE",
            TokenType::Return => "RETURN",

            TokenType::True => "TRUE",
            TokenType::False => "FALSE",

            TokenType::Program => "PROGRAM",
        };

        String::from(s)
    }

    pub fn keywords() -> Vec<String> {
        let keywords = ["let", "func", "if", "else", "return", "true", "false"];

        keywords.map(|x| x.to_string()).to_vec()
    }

    pub fn is_statement(&self) -> bool {
        matches!(&self, TokenType::Let | TokenType::Return)
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.string())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Self {
        Token {
            token_type,
            literal,
        }
    }

    pub fn from_keyword(literal: String) -> Self {
        let token_type = match literal.as_str() {
            "let" => TokenType::Let,
            "func" => TokenType::Function,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            "true" => TokenType::True,
            "false" => TokenType::False,
            _ => TokenType::Illegal,
        };

        Token {
            token_type,
            literal,
        }
    }

    /// Returns the precedence level of each operator used in infix operations.
    /// Returns an Error if called with on an non-math operator.
    pub fn precedence(&self) -> Option<Precedence> {
        let p = match self.token_type {
            TokenType::Equal => Precedence::EQUALS,
            TokenType::NotEqual => Precedence::EQUALS,
            TokenType::LT => Precedence::LESSGREATER,
            TokenType::GT => Precedence::LESSGREATER,
            TokenType::Plus => Precedence::SUM,
            TokenType::Minus => Precedence::SUM,
            TokenType::Asterisk => Precedence::PRODUCT,
            TokenType::Slash => Precedence::PRODUCT,
            TokenType::LParen => Precedence::CALL,
            TokenType::LBracket => Precedence::INDEX,
            _ => return None,
        };

        Some(p)
    }
}
