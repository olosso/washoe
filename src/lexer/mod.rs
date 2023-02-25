use crate::token::*;

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    pub ch: char,
    pub chars: Vec<char>,
}

impl Lexer {
    ///
    /// Lexer produces tokens. Produces tokens only when next_token is called. Does not lex anything prematurely!
    ///
    /// Usage:
    /// let mut l = Lexer::new(String::from(input));
    /// let mut token = l.next_token();
    ///
    /// Call next_token() when you want a new token.
    pub fn new(source: String) -> Self {
        let chars = source.chars().collect();
        let mut l = Lexer {
            input: source,
            position: 0,      // Where the lexer currently is, starts at 0.
            read_position: 1, // Which character will be when read_char() is called.
            ch: '\0',
            chars,
        };

        l.trim_input();
        l.read_char();
        l
    }

    /// Remove whitespaces from the beginning and end of the input string.
    fn trim_input(&mut self) {
        let mut chars = self.input.chars();
        let mut c = chars.next().unwrap();

        let mut i: usize = 0;
        while c == ' ' {
            i += 1;
            c = match chars.next() {
                Some(x) => x,
                None => break,
            };
        }

        let mut chars = self.input.chars().rev();
        let mut c = chars.next().unwrap();

        let mut j: usize = self.input.chars().count();
        while c == ' ' {
            j -= 1;
            c = match chars.next() {
                Some(x) => x,
                None => break,
            };
        }

        self.input = (self.input[i..j]).to_string();
    }

    /// Read a character and move reading position forward.
    /// If the lexer moves past the end of the input string, return NULL.
    fn read_char(&mut self) {
        if self.read_position > self.chars.len() {
            self.ch = '\0';
        } else {
            self.ch = *self.chars.get(self.position).unwrap();
        }

        // REVIEW Is this the right placement for this code? It makes the peeking look funny.
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn is_keyword(s: &String) -> bool {
        TokenType::keywords().contains(s)
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char()
        }
    }

    /// Get next token and moves the lexer forward one step.
    pub fn next_token(&mut self) -> Token {
        // If the current character is a whitespace, just skip it.
        self.skip_whitespace();

        let token = match self.ch {
            // Resolving identifier
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut identifier = String::from("");

                while self.ch.is_alphabetic() || self.ch == '_' {
                    identifier.push(self.ch);
                    self.read_char();
                }

                if Self::is_keyword(&identifier) {
                    return Token::from_keyword(identifier);
                } else {
                    return Token::new(TokenType::Ident, identifier);
                }
            }

            // Resolving integer
            '0'..='9' => {
                let mut int = String::from("");

                while self.ch.is_numeric() {
                    int.push(self.ch);
                    self.read_char();
                }

                return Token::new(TokenType::Int, int);
            }

            // Resolving whether token is Assign or Equal
            '=' => {
                // FIXME This is a bit hacky just to pass all the tests.
                match self.peek() {
                    Some(x) => match x {
                        '=' => {
                            self.read_char();
                            self.read_char();
                            return Token::new(TokenType::Equal, "==".to_string());
                        }
                        _ => {
                            self.read_char();
                            return Token::new(TokenType::Assign, "=".to_string());
                        }
                    },
                    None => return Token::new(TokenType::Assign, "=".to_string()),
                }
            }

            // Resolving whether token is Not or NotEqual
            '!' => {
                self.read_char();

                // FIXME This is a bit hacky just to pass all the tests.
                match self.ch {
                    '=' => {
                        self.read_char();
                        return Token::new(TokenType::NotEqual, "!=".to_string());
                    }
                    _ => return Token::new(TokenType::Bang, "!".to_string()),
                }
            }

            '"' => {
                let mut string = String::new();

                while self.peek().filter(|x| *x != '"').is_some() {
                    if self.peek().filter(|x| *x == '\\').is_some() {
                        self.read_char();
                        if self.peek().filter(|x| *x == '"').is_some() {
                            self.read_char();
                        }
                        string.push(self.ch);
                        continue;
                    }
                    self.read_char();
                    string.push(self.ch);
                }
                // Get past the quotation mark
                self.read_char();
                self.read_char();
                return Token::new(TokenType::String, string);
            }
            ';' => Token::new(TokenType::Semicolon, self.ch.to_string()),
            '(' => Token::new(TokenType::LParen, self.ch.to_string()),
            ')' => Token::new(TokenType::RParen, self.ch.to_string()),
            '[' => Token::new(TokenType::LBracket, self.ch.to_string()),
            ']' => Token::new(TokenType::RBracket, self.ch.to_string()),
            '{' => Token::new(TokenType::LBrace, self.ch.to_string()),
            '}' => Token::new(TokenType::RBrace, self.ch.to_string()),
            ',' => Token::new(TokenType::Comma, self.ch.to_string()),
            ':' => Token::new(TokenType::Colon, self.ch.to_string()),
            '+' => Token::new(TokenType::Plus, self.ch.to_string()),
            '-' => Token::new(TokenType::Minus, self.ch.to_string()),
            '<' => Token::new(TokenType::LT, self.ch.to_string()),
            '>' => Token::new(TokenType::GT, self.ch.to_string()),
            '*' => Token::new(TokenType::Asterisk, self.ch.to_string()),
            '/' => Token::new(TokenType::Slash, self.ch.to_string()),
            '\0' => Token::new(TokenType::EOF, "EOF".to_string()),
            _ => Token::new(TokenType::Illegal, "ILLEGAL".to_string()),
        };

        self.read_char();
        token
    }

    /// Check which character will be read next.
    pub fn peek(&self) -> Option<char> {
        if self.position <= self.chars.len() {
            Some(self.chars[self.position])
        } else {
            None
        }
    }

    pub fn tokens(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        while self.ch != '\0' {
            let token = self.next_token();
            tokens.push(token);
        }
        tokens
    }
}

#[cfg(test)]
mod lexer_helper_tests {
    use super::*;

    #[test]
    fn test_trim() {
        let input = "    let foo = 1    ";
        let mut l = Lexer::new(String::from(input));

        assert_eq!(String::from("let foo = 1"), l.input);
    }

    #[test]
    fn test_whitespace() {
        assert!('\n'.is_whitespace());
        assert!(!'a'.is_whitespace());
    }
}

#[cfg(test)]
mod token_tests {
    use super::*;

    #[test]
    fn test_lexing_hash() {
        let input = r#"{"hello": 1, true: 2, 3: "three"}"#;
        // It works, don't @ me.
        let expected = vec![
            Token {
                token_type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            Token {
                token_type: TokenType::String,
                literal: "hello".to_string(),
            },
            Token {
                token_type: TokenType::Colon,
                literal: ":".to_string(),
            },
            Token {
                token_type: TokenType::Int,
                literal: "1".to_string(),
            },
            Token {
                token_type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Token {
                token_type: TokenType::True,
                literal: "true".to_string(),
            },
            Token {
                token_type: TokenType::Colon,
                literal: ":".to_string(),
            },
            Token {
                token_type: TokenType::Int,
                literal: "2".to_string(),
            },
            Token {
                token_type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Token {
                token_type: TokenType::Int,
                literal: "3".to_string(),
            },
            Token {
                token_type: TokenType::Colon,
                literal: ":".to_string(),
            },
            Token {
                token_type: TokenType::String,
                literal: "three".to_string(),
            },
            Token {
                token_type: TokenType::RBrace,
                literal: "}".to_string(),
            },
        ];

        let mut l = Lexer::new(String::from(input));
        let mut token = l.next_token();

        for expected_token in expected.into_iter() {
            // Check that token types returned by the lexer is correct.
            assert_eq!(
                expected_token.token_type, token.token_type,
                "Expected TokenType::{:?}, but got TokenType::{:?}",
                expected_token.token_type, token.token_type
            );
            // Check that the literal returned by the lexer is correct.
            assert_eq!(
                expected_token.literal, token.literal,
                "Expected literal {:?}, but got literal {:?}",
                expected_token, token.literal
            );
            token = l.next_token();
        }
    }

    #[test]
    fn test_lexing_let() {
        let input = "let";
        let ex_token = TokenType::Let;

        let mut l = Lexer::new(String::from(input));
        let mut token = l.next_token();

        assert_eq!(
            ex_token, token.token_type,
            "Expected TokenType::{:?}, but got TokenType::{:?}",
            ex_token, token.token_type
        );
        assert_eq!(
            input.to_string(),
            token.literal,
            "Expected literal {:?}, but got literal {:?}",
            input,
            token.literal
        );
    }

    #[test]
    fn test_lexing_identifiers() {
        let input = "foo";
        let ex_token = TokenType::Ident;

        let mut l = Lexer::new(String::from(input));
        let mut token = l.next_token();

        assert_eq!(
            ex_token, token.token_type,
            "Expected TokenType::{:?}, but got TokenType::{:?}",
            ex_token, token.token_type
        );
        assert_eq!(
            input.to_string(),
            token.literal,
            "Expected literal {:?}, but got literal {:?}",
            input,
            token.literal
        );
    }

    #[test]
    fn test_lexing_operators_and_delimiters() {
        let input = "=+(){},;";

        let expected = vec![
            TokenType::Assign,
            TokenType::Plus,
            TokenType::LParen,
            TokenType::RParen,
            TokenType::LBrace,
            TokenType::RBrace,
            TokenType::Comma,
            TokenType::Semicolon,
            TokenType::EOF,
        ];

        let mut l = Lexer::new(String::from(input));
        let mut token = l.next_token();

        for (ex_token, ex_literal) in expected.into_iter().zip(input.chars()) {
            assert_eq!(
                ex_token, token.token_type,
                "Expected TokenType::{:?}, but got TokenType::{:?}",
                ex_token, token.token_type
            );
            assert_eq!(
                ex_literal.to_string(),
                token.literal,
                "Expected literal {:?}, but got literal {:?}",
                ex_token,
                token.literal
            );
            token = l.next_token();
        }
    }

    #[test]
    fn test_lexing_keywords_let_and_func() {
        let input = r#"let five = 5;

let ten = 10;

let add_two_numbers_ = func(x, y) {
x + y;
}

let result = add(five, ten);
"elise"
"fur elise"
"eine \"kleine\" nachtmusik"
"\"eine kleine nachtmusik\""
"#;

        // It works, don't @ me.
        let expected = vec![
            Token {
                token_type: TokenType::Let,
                literal: "let".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "five".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Int,
                literal: "5".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::Let,
                literal: "let".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "ten".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Int,
                literal: "10".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::Let,
                literal: "let".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "add_two_numbers_".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Function,
                literal: "func".to_string(),
            },
            Token {
                token_type: TokenType::LParen,
                literal: "(".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "x".to_string(),
            },
            Token {
                token_type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "y".to_string(),
            },
            Token {
                token_type: TokenType::RParen,
                literal: ")".to_string(),
            },
            Token {
                token_type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "x".to_string(),
            },
            Token {
                token_type: TokenType::Plus,
                literal: "+".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "y".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::RBrace,
                literal: "}".to_string(),
            },
            Token {
                token_type: TokenType::Let,
                literal: "let".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "result".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "add".to_string(),
            },
            Token {
                token_type: TokenType::LParen,
                literal: "(".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "five".to_string(),
            },
            Token {
                token_type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "ten".to_string(),
            },
            Token {
                token_type: TokenType::RParen,
                literal: ")".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::String,
                literal: "elise".to_string(),
            },
            Token {
                token_type: TokenType::String,
                literal: "fur elise".to_string(),
            },
            Token {
                token_type: TokenType::String,
                literal: r#"eine "kleine" nachtmusik"#.to_string(),
            },
            Token {
                token_type: TokenType::String,
                literal: r#""eine kleine nachtmusik""#.to_string(),
            },
            Token {
                token_type: TokenType::EOF,
                literal: "EOF".to_string(),
            },
        ];

        let mut l = Lexer::new(String::from(input));
        let mut token = l.next_token();

        for expected_token in expected.into_iter() {
            // Check that token types returned by the lexer is correct.
            assert_eq!(
                expected_token.token_type, token.token_type,
                "Expected TokenType::{:?}, but got TokenType::{:?}",
                expected_token.token_type, token.token_type
            );
            // Check that the literal returned by the lexer is correct.
            assert_eq!(
                expected_token.literal, token.literal,
                "Expected literal {:?}, but got literal {:?}",
                expected_token, token.literal
            );
            token = l.next_token();
        }
    }

    #[test]
    //#[ignore]
    fn test_lexing_math_operators() {
        let input = ";
!-/**/;
truefalse;
true false;
";

        let expected = vec![
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::Bang,
                literal: "!".to_string(),
            },
            Token {
                token_type: TokenType::Minus,
                literal: "-".to_string(),
            },
            Token {
                token_type: TokenType::Slash,
                literal: "/".to_string(),
            },
            Token {
                token_type: TokenType::Asterisk,
                literal: "*".to_string(),
            },
            Token {
                token_type: TokenType::Asterisk,
                literal: "*".to_string(),
            },
            Token {
                token_type: TokenType::Slash,
                literal: "/".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::Ident,
                literal: "truefalse".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::True,
                literal: "true".to_string(),
            },
            Token {
                token_type: TokenType::False,
                literal: "false".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::EOF,
                literal: "EOF".to_string(),
            },
        ];

        let mut l = Lexer::new(String::from(input));
        let mut token = l.next_token();

        for expected_token in expected.into_iter() {
            // Check that token types returned by the lexer is correct.
            assert_eq!(
                expected_token.token_type, token.token_type,
                "Expected TokenType::{:?}, but got TokenType::{:?}",
                expected_token.token_type, token.token_type
            );
            // Check that the literal returned by the lexer is correct.
            assert_eq!(
                expected_token.literal, token.literal,
                "Expected literal {:?}, but got literal {:?}",
                expected_token, token.literal
            );
            token = l.next_token();
        }
    }

    #[test]
    //#[ignore]
    fn test_two_char_operators() {
        let input = "==; !; !=; =;";

        let expected = vec![
            Token {
                token_type: TokenType::Equal,
                literal: "==".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::Bang,
                literal: "!".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::NotEqual,
                literal: "!=".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::EOF,
                literal: "EOF".to_string(),
            },
        ];

        let mut l = Lexer::new(String::from(input));
        let mut token = l.next_token();

        for expected_token in expected.into_iter() {
            // Check that token types returned by the lexer is correct.
            assert_eq!(
                expected_token.token_type, token.token_type,
                "Expected TokenType::{:?}, but got TokenType::{:?}",
                expected_token.token_type, token.token_type
            );
            // Check that the literal returned by the lexer is correct.
            assert_eq!(
                expected_token.literal, token.literal,
                "Expected literal {:?}, but got literal {:?}",
                expected_token, token.literal
            );
            token = l.next_token();
        }
    }

    #[test]
    fn test_lexing_logical_operators() {
        let input = "if (true == true) {
return 1
} else {
return 2
};
";

        let expected = vec![
            Token {
                token_type: TokenType::If,
                literal: "if".to_string(),
            },
            Token {
                token_type: TokenType::LParen,
                literal: "(".to_string(),
            },
            Token {
                token_type: TokenType::True,
                literal: "true".to_string(),
            },
            Token {
                token_type: TokenType::Equal,
                literal: "==".to_string(),
            },
            Token {
                token_type: TokenType::True,
                literal: "true".to_string(),
            },
            Token {
                token_type: TokenType::RParen,
                literal: ")".to_string(),
            },
            Token {
                token_type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            Token {
                token_type: TokenType::Return,
                literal: "return".to_string(),
            },
            Token {
                token_type: TokenType::Int,
                literal: "1".to_string(),
            },
            Token {
                token_type: TokenType::RBrace,
                literal: "}".to_string(),
            },
            Token {
                token_type: TokenType::Else,
                literal: "else".to_string(),
            },
            Token {
                token_type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            Token {
                token_type: TokenType::Return,
                literal: "return".to_string(),
            },
            Token {
                token_type: TokenType::Int,
                literal: "2".to_string(),
            },
            Token {
                token_type: TokenType::RBrace,
                literal: "}".to_string(),
            },
            Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                token_type: TokenType::EOF,
                literal: "EOF".to_string(),
            },
        ];

        let mut l = Lexer::new(String::from(input));
        let mut token = l.next_token();

        for expected_token in expected.into_iter() {
            // Check that token types returned by the lexer is correct.
            assert_eq!(
                expected_token.token_type, token.token_type,
                "Expected TokenType::{:?}, but got TokenType::{:?}",
                expected_token.token_type, token.token_type
            );
            // Check that the literal returned by the lexer is correct.
            assert_eq!(
                expected_token.literal, token.literal,
                "Expected literal {:?}, but got literal {:?}",
                expected_token, token.literal
            );
            token = l.next_token();
        }
    }
}
