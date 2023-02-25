mod tests;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;
use std::collections::HashMap;
use std::{error::Error, fmt};

/// By this ordering, take this expression:
///
/// 1 + 2 == -3 * 8 < ten()
///
/// The result would be evaluated like so:
/// (((1 + 2) - 1) == (((-3) * 8) < (ten())))
/// 1. ten() (=> 10)
/// 2. -3 (=> -3)
/// 3. -3 * 8 (=> -24)
/// 4. 1 + 2 (=> 3)
/// 5. 3 - 1 (=> 2)
/// 6. -24 < 10 (=> true)
/// 7. 2 == true (=> Error :))
#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    EMPTY,       // Empty identifier _
    LOWEST,      // IntLiterals, Identifiers
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // foo(X)
    INDEX,       // [...][0]
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    prefix_parse_fns:
        HashMap<TokenType, for<'a> fn(&'a mut Self) -> Result<Expression, ParseError>>,
    infix_parse_fns:
        HashMap<TokenType, for<'a> fn(&'a mut Self, Expression) -> Result<Expression, ParseError>>,
}

impl Parser {
    /// Parser::new
    /// Create a new parser given a lexer.
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token {
                token_type: TokenType::SOF,
                literal: String::from("Start of file"),
            },
            peek_token: Token {
                token_type: TokenType::SOF,
                literal: String::from("Start of file"),
            },
            prefix_parse_fns: Self::prefixes(),
            infix_parse_fns: Self::infixes(),
        };

        parser.next_token();
        parser.next_token();

        assert!(!parser.cur_tokentype_is(TokenType::SOF));
        assert!(!parser.peek_tokentype_is(TokenType::SOF));

        parser
    }

    fn prefixes() -> HashMap<TokenType, for<'a> fn(&'a mut Self) -> Result<Expression, ParseError>>
    {
        let mut prefix_fns = HashMap::new();

        prefix_fns.insert(
            TokenType::Ident,
            Self::parse_identifier as for<'a> fn(&'a mut Self) -> Result<Expression, ParseError>,
        );
        prefix_fns.insert(TokenType::True, Self::parse_bool);
        prefix_fns.insert(TokenType::False, Self::parse_bool);
        prefix_fns.insert(TokenType::Int, Self::parse_integer_literal);
        prefix_fns.insert(TokenType::String, Self::parse_string_literal);
        prefix_fns.insert(TokenType::Bang, Self::parse_prefix_expression);
        prefix_fns.insert(TokenType::Minus, Self::parse_prefix_expression);
        prefix_fns.insert(TokenType::LParen, Self::parse_grouped_expression);
        prefix_fns.insert(TokenType::LBracket, Self::parse_bracket);
        prefix_fns.insert(TokenType::If, Self::parse_if_expression);
        prefix_fns.insert(TokenType::Function, Self::parse_func_expression);
        prefix_fns.insert(TokenType::LBrace, Self::parse_hashmap_expression);

        prefix_fns
    }

    fn infixes(
    ) -> HashMap<TokenType, for<'a> fn(&'a mut Self, Expression) -> Result<Expression, ParseError>>
    {
        let mut infix_fns = HashMap::new();

        infix_fns.insert(
            TokenType::Plus,
            Self::parse_infix_expression
                as for<'a> fn(&'a mut Self, Expression) -> Result<Expression, ParseError>,
        );
        infix_fns.insert(TokenType::Minus, Self::parse_infix_expression);
        infix_fns.insert(TokenType::Asterisk, Self::parse_infix_expression);
        infix_fns.insert(TokenType::Slash, Self::parse_infix_expression);
        infix_fns.insert(TokenType::Equal, Self::parse_infix_expression);
        infix_fns.insert(TokenType::NotEqual, Self::parse_infix_expression);
        infix_fns.insert(TokenType::GT, Self::parse_infix_expression);
        infix_fns.insert(TokenType::LT, Self::parse_infix_expression);
        infix_fns.insert(TokenType::LParen, Self::parse_call_expression);
        infix_fns.insert(TokenType::LBracket, Self::parse_index_expression);

        infix_fns
    }
    /// Parser::next_token
    /// Advance the lexer of the parser to receive a new token. Also updates the peek_token.
    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    /// Parser::parse_program
    /// Goes through tokens found by the lexer and trys to find the statements.
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut program = Program::new(vec![]);

        /*
         * Loops over the Tokens produced by the Lexer.
         * Forwards the execution to a parser function if it finds something interesting.
         * Ends execution when EOF Token is encountered.
         */
        while !self.cur_tokentype_is(TokenType::EOF) {
            let statement = self.parse_statement()?;
            program.statements.push(statement);
            self.next_token();
        }

        Ok(program)
    }

    /// Parses BlockStatements found in IfExpressions.
    fn parse_block_statement(&mut self) -> Statement {
        assert!(self.cur_tokentype_is(TokenType::LBrace));

        let lbrace = self.current_token.clone();
        let mut block: Vec<Statement> = vec![];
        self.next_token();

        /*
         * Loops over the Tokens produced by the Lexer.
         * Forwards the execution to a parser function if it finds something interesting.
         * Ends execution when EOF Token is encountered.
         */
        while !self.cur_tokentype_is(TokenType::RBrace) {
            let statement = self.parse_statement();
            if let Ok(x) = statement {
                block.push(x);
            }
            self.next_token();
        }

        Statement::Block(lbrace, block)
    }

    /// Forwards the Parser to the correct statement parsing function based on the current token type.
    /// Let and Return statements are easy to identify: The statement must begin with those keywords.
    /// If neither of those is the case, then the statement is interpreted as a ExpressionStatement.
    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        let statement = match &self.current_token.token_type {
            TokenType::Let => self.parse_let_statement()?,
            TokenType::Return => self.parse_return_statement()?,
            _ => self.parse_expression_statement()?,
        };

        Ok(statement)
    }

    /// Parses the following statement type:
    /// let <identifier> = <expression>;
    /// This is the most complicated statement in the language.
    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        assert!(self.cur_tokentype_is(TokenType::Let));

        // current_token is Let
        let let_token = self.current_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            return Err(PE::new(
                "Let statement not followed by an identifier.".to_string(),
            ));
        };
        // current_token is Ident
        let name = Expression::Identifier(
            self.current_token.clone(),
            self.current_token.literal.clone(),
        );

        if !self.expect_peek(TokenType::Assign) {
            return Err(PE::new(
                "Identifier not followed by an assignment in let statement.".to_string(),
            ));
        };
        // current_token is Assign
        //
        self.next_token();

        let expression = self.parse_expression(Precedence::LOWEST)?;

        if !self.expect_peek(TokenType::Semicolon) {
            return Err(PE::new(
                "LetStatement not finished with semicolon.".to_string(),
            ));
        };

        Ok(Statement::Let(let_token, name, expression))
    }

    /// Parses the following statement type:
    /// return <expression>;
    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        assert!(self.cur_tokentype_is(TokenType::Return));

        let return_token = self.current_token.clone();

        if self.peek_tokentype_is(TokenType::Semicolon) {
            return Err(PE::new("Empty ReturnStatement not allowed.".to_string()));
        };

        self.next_token();

        let expression = self.parse_expression(Precedence::LOWEST)?;

        if !self.expect_peek(TokenType::Semicolon) {
            return Err(PE::new(
                "ReturnStatement not finished with semicolon.".to_string(),
            ));
        };

        Ok(Statement::Return(return_token, expression))
    }

    /// Parses the following statement type:
    /// <expression>;
    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        if !(self.cur_tokentype_is(TokenType::Int)
            || self.cur_tokentype_is(TokenType::String)
            || self.cur_tokentype_is(TokenType::Ident)
            || self.cur_tokentype_is(TokenType::Minus)
            || self.cur_tokentype_is(TokenType::Bang)
            || self.cur_tokentype_is(TokenType::True)
            || self.cur_tokentype_is(TokenType::False)
            || self.cur_tokentype_is(TokenType::If)
            || self.cur_tokentype_is(TokenType::Function)
            || self.cur_tokentype_is(TokenType::LParen)
            || self.cur_tokentype_is(TokenType::LBracket)
            || self.cur_tokentype_is(TokenType::LBrace))
        {
            return Err(PE::new_t(
                "First token in Expression was a {:?}, this is not supported.".to_string(),
                self.current_token.to_owned(),
            ));
        }

        let token = self.current_token.clone();

        /*
         * This is the starting point to parsing an Expression statement, this means that the
         * first symbol must have been either a Identifier, Number or one of the Prefix operators.
         * We haven't actually parsed anything yet, so we start with the Precedence LOWEST.
         */
        let expression = self.parse_expression(Precedence::LOWEST)?;

        /*
         * If a semicolon is found after the expression statement, it is ignored.
         * This makes the semicolon following the expression optional.
         */
        if self.peek_tokentype_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expr(token, expression))
    }

    /// This is the master expression parsing function. It parses every expression found/expected in statements.
    /// Let's assume the expression is -1 + 2 * 3;, which should result in ((-1) + (2 * 3))
    /// 0. parse_expression(LOWEST) (*1)
    /// 1. Current token is -, which has a prefix function. Get it and call it.
    /// 2. Before the prefix creates a PrefixExpression and returns,
    ///    it advances the lexer and calls parse_expression(PREFIX) (*2).
    /// 3. Current token is 1. It has prefix handler. Get it and call it.
    /// 4. The prefix function for IntTokens simply creates an IntegerLiteral expression.
    /// 3. (*2) Continues. The loop condition fails, since the
    ///    next token (+) has lower precedence.
    /// 4. (*2) Returns Expression::Int(1)
    /// 5. The prefix function from the 2. step continues and returns a Expression::Prefix(-, 1)
    /// 6. (*1) Continues. The Expression::Prefix(-, 1) is assigned to left.
    /// 7. The loop condition evaluates to true, because precedence LOWEST < SUM.
    /// 8. Peek token is +, which has a infix function. Get it.
    /// 9. Advance lexer, now current token is +.
    /// 10. Call the infix handler retrieved in 8 with the Expression::Prefix(-, 1) as an argument.
    /// 11. The infix handler stores the precedence of +, and advances the lexer. Current token is 2.
    /// 12. parse_expression(SUM) (*3)
    /// 13. 2 has a prefix handler, it creates a IntegerLiteral(2), which is assigned to left.
    /// 14. The loop condition evaluates to true, because SUM < PRODUCT.
    /// 15. Get the infix function of *.
    /// 16. Advance the lexer, current token is now *.
    /// 17. Call the infix handler retrieved in 15 with the IntegerLiteral(2) as an argument.
    /// 18. The infix handler stores the precedence of *, and advances the lexer. Current token is 3.
    /// 19. parse_expression(LOWEST) (*4)
    /// 20. 3 has a prefix handler, it creates a IntegerLiteral(3), which is assigned to left.
    /// 21. The loop condition evaluates to false, since the next token is ;
    /// 22. (*4) Returns IntegerLiteral(3)
    /// 23. The infix handler from step 18 continues.
    /// 24. It returns a Expression::Infix(IntegerLiteral(2), *, IntegerLiteral(3))
    /// 25. (*3) Continues with left = Expression::Infix(IntegerLiteral(2), *, IntegerLiteral(3)).
    /// 26. The loop condition evaluates to false, since the next token is ;
    /// 27. (*3) Returns left = Expression::Infix(IntegerLiteral(2), *, IntegerLiteral(3)).
    /// 28. The infix call from step 11 continues.
    /// 29. It returns
    ///     Expression::Infix(
    ///         Expression::Prefix(-, 1),
    ///         +,
    ///         Expression::Infix(IntegerLiteral(2), *, IntegerLiteral(3))
    ///     )
    /// 30. (*1) Continues, and the loop condition evaluates to false.
    /// 31. (*1) Returns the Expression formed in step 29.
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let prefix_handler =
            if let Some(f) = self.prefix_parse_fns.get(&self.current_token.token_type) {
                f
            } else {
                return Err(PE::new_t(
                    "Failed to parse expression. Didn't find prefix handler for".to_string(),
                    self.current_token.to_owned(),
                ));
            };

        let mut left = prefix_handler(self)?;

        while !self.peek_tokentype_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
            let peek_token_type = &self.peek_token.token_type.clone();
            let infix_handler = if let Some(f) = self.infix_parse_fns.get(peek_token_type) {
                *f
            } else {
                return Ok(left);
            };

            self.next_token();
            left = infix_handler(self, left)?;
        }

        Ok(left)
    }

    /// Token { ident, "foo" } => Identifier { Token { ident, "foo" }, "foo" }
    /// Note that this is a prefix function -> It can be a leaf in the AST.
    fn parse_bool(&mut self) -> Result<Expression, ParseError> {
        assert!(self.cur_tokentype_is(TokenType::True) || self.cur_tokentype_is(TokenType::False));

        Ok(Expression::Bool(
            self.current_token.clone(),
            self.cur_tokentype_is(TokenType::True),
        ))
    }

    /// Token { ident, "foo" } => Identifier { Token { ident, "foo" }, "foo" }
    /// Note that this is a prefix function -> It can be a leaf in the AST.
    fn parse_identifier(&mut self) -> Result<Expression, ParseError> {
        assert!(self.cur_tokentype_is(TokenType::Ident));

        Ok(Expression::Identifier(
            self.current_token.clone(),
            self.current_token.literal.clone(),
        ))
    }

    /// Token { int, "1" } => IntegerLiteral { Token { int, "1" }, 1 }
    /// Note that this is a prefix function -> It can be a leaf in the AST.
    fn parse_integer_literal(&mut self) -> Result<Expression, ParseError> {
        assert!(self.cur_tokentype_is(TokenType::Int));

        let current_token = self.current_token.clone();
        let int: i32 = current_token
            .literal
            .parse()
            .expect("Failed to parse assumed Integer Token");

        Ok(Expression::IntegerLiteral(self.current_token.clone(), int))
    }

    /// Token { String, "hello" } => StringLiteral { Token { String, "hello" }, "hello" }
    /// Note that this is a prefix function -> It can be a leaf in the AST.
    fn parse_string_literal(&mut self) -> Result<Expression, ParseError> {
        assert!(self.cur_tokentype_is(TokenType::String));

        let current_token = self.current_token.clone();
        let string: String = current_token.literal;

        Ok(Expression::StringLiteral(
            self.current_token.clone(),
            string,
        ))
    }

    /// This is only called from parse_expression.
    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        assert!(self.cur_tokentype_is(TokenType::Minus) || self.cur_tokentype_is(TokenType::Bang));

        /*
         * Store the current token information.
         * Since this token is a prefix, it is one of the following: -, !.
         * So for example
         * current_token = Token { Minus, "-" }
         * literal = "-"
         */
        let current_token = self.current_token.clone();
        let literal = self.current_token.literal.clone();

        self.next_token();

        /*
         * We continue expression parsing by setting the Precedence to Prefix,
         * which means it has high associative power.
         * Indeed, only a function call has higher precedence.
         * The expression -1 + 2 will be evaluated ((-1) + 2), and not -(1 + 2).
         */
        let right = self.parse_expression(Precedence::PREFIX)?;

        Ok(Expression::Prefix(current_token, literal, Box::new(right)))
    }

    /// This is only called from parse_expression.
    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        assert!(self.cur_tokentype_is(TokenType::LParen));

        // We encountered a parenthesis, so let's go right ahead.
        self.next_token();

        /*
         *  We simply continues parsing with Precedence set to lowest,
         *  which means we ignore what the Precedence was of the previous operator.
         */
        let exp = self.parse_expression(Precedence::LOWEST)?;

        if self.expect_peek(TokenType::RParen) {
            Ok(exp)
        } else {
            Err(PE::new(
                "Left Parethesis never closed with Right Parenthesis.".to_string(),
            ))
        }
    }

    /// Parses both If and IfElse Expressions.
    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        assert!(self.cur_tokentype_is(TokenType::If));

        let if_token = self.current_token.clone();

        if !self.expect_peek(TokenType::LParen) {
            return Err(PE::new(
                "If token not followed by Left Parenthesis.".to_string(),
            ));
        };

        let lparen = self.current_token.clone();
        let condition = self.parse_grouped_expression()?;

        if !self.expect_peek(TokenType::LBrace) {
            return Err(PE::new(
                "Right Parenthesis not followed by Left Brace in IfExpression.".to_string(),
            ));
        };

        let consequence = self.parse_block_statement();
        self.next_token();

        let alternative = if (self.cur_tokentype_is(TokenType::Else)) {
            if !self.expect_peek(TokenType::LBrace) {
                return Err(PE::new(
                    "ElseToken not followed by Left Brace in IfExpression.".to_string(),
                ));
            };
            Some(Box::new(self.parse_block_statement()))
        } else {
            None
        };

        Ok(Expression::If(
            if_token,
            Box::new(condition),
            Box::new(consequence),
            alternative,
        ))
    }

    fn parse_func_expression(&mut self) -> Result<Expression, ParseError> {
        assert!(self.cur_tokentype_is(TokenType::Function));

        let func_token = self.current_token.clone();

        if !self.expect_peek(TokenType::LParen) {
            return Err(PE::new(
                "FunctionToken not followed by Left Parenthesis.".to_string(),
            ));
        };

        let params = self.parse_function_parameters()?;
        assert!(self.cur_tokentype_is(TokenType::RParen));

        if !self.expect_peek(TokenType::LBrace) {
            return Err(PE::new(
                "Expected LBrace to start function Body, but got {:?}".to_string(),
            ));
        }

        let body = Box::new(self.parse_block_statement());

        Ok(Expression::Func(func_token, params, body))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Expression>, ParseError> {
        assert!(
            self.cur_tokentype_is(TokenType::LParen) || self.cur_tokentype_is(TokenType::LBracket)
        );
        let me = self.current_token.token_type;
        let other = if me == TokenType::LParen {
            TokenType::RParen
        } else {
            TokenType::RBracket
        };

        let mut params = vec![];

        if self.peek_tokentype_is(other) {
            self.next_token();
            return Ok(params);
        }

        self.next_token();
        params.push(self.parse_identifier()?);

        while self.peek_tokentype_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            params.push(self.parse_identifier()?);
        }

        if !self.expect_peek(other) {
            return Err(PE::new(format!(
                "Expected {} to be closed with {}, but got {}",
                me, other, self.peek_token.token_type,
            )));
        }

        Ok(params)
    }

    fn parse_bracket(&mut self) -> Result<Expression, ParseError> {
        assert!(self.cur_tokentype_is(TokenType::LBracket));

        let bracket_token = self.current_token.clone();

        let exprs = self.parse_call_args()?;
        assert!(self.cur_tokentype_is(TokenType::RBracket));

        Ok(Expression::Array(bracket_token, exprs))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        assert!(
            self.cur_tokentype_is(TokenType::Plus)
                || self.cur_tokentype_is(TokenType::Minus)
                || self.cur_tokentype_is(TokenType::Asterisk)
                || self.cur_tokentype_is(TokenType::Slash)
                || self.cur_tokentype_is(TokenType::Equal)
                || self.cur_tokentype_is(TokenType::NotEqual)
                || self.cur_tokentype_is(TokenType::LT)
                || self.cur_tokentype_is(TokenType::GT),
        );

        // Store the current values of token, because next we are going to forward the lexer.
        let current_token = self.current_token.clone();
        let operator = self.current_token.literal.clone();

        // Need the precedence of this operator to compare to the ones following.
        let precedence = current_token.precedence().unwrap();

        // What comes next, I wonder.
        self.next_token();

        /*
         * Whatever comes out of here will be set to the right child node of an Infix expression.
         *
         * The important bit here, is that for this parse_expression call the current token will be
         * something like an identifier, while the precedence will be that of the previous infix operator.
         * This allows the function to compare the Precedence of the previous operator with the next ones,
         * which determines which operator get to keep the current token.
         */
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix(
            current_token,
            Box::new(left),
            operator,
            Box::new(right),
        ))
    }

    fn parse_hashmap_expression(&mut self) -> Result<Expression, ParseError> {
        assert!(self.cur_tokentype_is(TokenType::LBrace));
        let rbracket = self.current_token.clone();

        if self.peek_tokentype_is(TokenType::RBrace) {
            self.next_token();
            return Ok(Expression::HashMap(rbracket, vec![], vec![]));
        };

        let mut keys = vec![];
        let mut values = vec![];

        self.next_token();
        loop {
            let key = self.parse_expression(Precedence::LOWEST)?;

            if !self.expect_peek(TokenType::Colon) {
                return Err(ParseError::new(
                    "HashMap key not followed by a colon.".to_string(),
                ));
            };
            self.next_token();
            let value = self.parse_expression(Precedence::LOWEST)?;
            keys.push(key);
            values.push(value);

            if self.expect_peek(TokenType::RBrace) {
                break;
            };

            if !self.expect_peek(TokenType::Comma) {
                return Err(ParseError::new(
                    "HashMap entry not followed by a comma.".to_string(),
                ));
            };

            self.next_token();
        }

        Ok(Expression::HashMap(rbracket, keys, values))
    }

    fn parse_call_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        assert!(self.cur_tokentype_is(TokenType::LParen));

        // Store the current values of token, because next we are going to forward the lexer.
        let lparen = self.current_token.clone();

        let args = self.parse_call_args()?;

        Ok(Expression::Call(lparen, Box::new(left), args))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        assert!(self.cur_tokentype_is(TokenType::LBracket));

        // Store the current values of token, because next we are going to forward the lexer.
        let lbracket = self.current_token.clone();

        let args = self.parse_call_args()?;
        assert!(args.len() == 1);

        Ok(Expression::Index(
            lbracket,
            Box::new(left),
            Box::new(args[0].clone()),
        ))
    }

    fn parse_call_args(&mut self) -> Result<Vec<Expression>, ParseError> {
        assert!(
            self.cur_tokentype_is(TokenType::LParen) || self.cur_tokentype_is(TokenType::LBracket)
        );
        let me = self.current_token.token_type;
        let other = if me == TokenType::LParen {
            TokenType::RParen
        } else {
            TokenType::RBracket
        };

        let mut args = vec![];

        if self.peek_tokentype_is(other) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::LOWEST)?);

        while self.peek_tokentype_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::LOWEST)?);
        }

        if !self.expect_peek(other) {
            return Err(PE::new(format!(
                "Expected {me} to be closed with {other}, but got {}",
                self.peek_token.token_type
            )));
        }

        Ok(args)
    }

    // Helper function.
    fn cur_tokentype_is(&self, tt: TokenType) -> bool {
        self.current_token.token_type == tt
    }

    /// Does the next token have the token type of the argument?
    fn peek_tokentype_is(&self, tt: TokenType) -> bool {
        self.peek_token.token_type == tt
    }

    /// Checks if the next token has the token type of the argument.
    /// If it does, then it also advances the current token.
    fn expect_peek(&mut self, tt: TokenType) -> bool {
        if self.peek_tokentype_is(tt) {
            self.next_token();
            true
        } else {
            false
        }
    }

    /// Return the Precedence of the current token.
    /// If token doesn't have a Precedence (symbol isn't used in expressions) returns Precedence::LOWEST.
    fn cur_precedence(&self) -> Precedence {
        match self.current_token.precedence() {
            Some(precedence) => precedence,
            None => Precedence::LOWEST,
        }
    }

    /// Return the Precedence of the next token.
    /// If token doesn't have a Precedence (symbol isn't used in expressions) returns Precedence::LOWEST.
    fn peek_precedence(&self) -> Precedence {
        match self.peek_token.precedence() {
            Some(precedence) => precedence,
            None => Precedence::LOWEST,
        }
    }

    /// A helper function for debugging purposes.
    pub fn print_tokens(&self) {
        println!(
            "###PARSER_TOKENS###:\nCURRENT_TOKEN: {:?}\nNEXT_TOKEN: {:?}",
            self.current_token, self.peek_token
        );
    }
}

type PE = ParseError;
#[derive(Debug)]
pub struct ParseError(String, Option<Token>);
impl ParseError {
    fn msg(&self) -> String {
        match &self.1 {
            None => self.0.to_owned(),
            Some(t) => format!(
                "{}. Problem encountered at token {:?}",
                self.0.to_owned(),
                t
            ),
        }
    }

    pub fn new(s: String) -> Self {
        ParseError(s, None)
    }
    pub fn new_t(s: String, t: Token) -> Self {
        ParseError(s, Some(t))
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg())
    }
}
