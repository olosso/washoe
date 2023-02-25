#[cfg(test)]
mod parser_tests {
    use crate::parser::*;

    fn init(input: &str) -> Program {
        let input = String::from(input);

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program().unwrap()
    }

    #[test]
    fn test_parse_let_statement() {
        let program = init("let x = 5;");

        assert_eq!(
            program.statements.len(),
            1,
            "Expected program to have 1 statement, but received {:?}",
            program.statements.len()
        );

        let parsed = &program.statements[0];

        assert!(matches!(parsed, Statement::Let(_, _, _)));
        assert_eq!(parsed.token().unwrap().literal, "let");
        assert_eq!(parsed.token().unwrap().token_type, TokenType::Let);
        assert_eq!(parsed.name().unwrap(), "x");
    }

    #[test]
    fn test_parse_let_statement2() {
        let program = init(r#"let x = "hello world";"#);

        assert_eq!(
            program.statements.len(),
            1,
            "Expected program to have 1 statement, but received {:?}",
            program.statements.len()
        );

        let parsed = &program.statements[0];
        let expr = parsed.expr().unwrap();

        assert!(matches!(parsed, Statement::Let(_, _, _)));
        assert_eq!(parsed.token().unwrap().literal, "let");
        assert_eq!(parsed.token().unwrap().token_type, TokenType::Let);
        assert_eq!(expr.token().token_type, TokenType::String);
        assert_eq!(*expr.string().unwrap(), String::from("hello world"));
    }

    #[test]
    #[should_panic(expected = "Let statement not followed by an identifier")]
    fn test_parse_let_statement_without_identifier_should_panic() {
        let malformed_input = "let = 1;".to_string();
        let lexer = Lexer::new(malformed_input);
        let mut parser = Parser::new(lexer);

        let program: Program = parser.parse_program().unwrap();
    }

    #[test]
    #[should_panic(expected = "Identifier not followed by an assignment in let statement")]
    fn test_parse_let_statement_without_assignment_should_panic() {
        let malformed_input = "let x 5;".to_string();

        let lexer = Lexer::new(malformed_input);
        let mut parser = Parser::new(lexer);

        let program: Program = parser.parse_program().unwrap();
    }

    #[test]
    fn test_parse_return_statements() {
        let program = init(
            "
     return 5;
     return 10;
     ",
        );

        assert_eq!(
            program.statements.len(),
            2,
            "Expected program to have 2 statements, but received {:?}",
            program.statements.len()
        );

        let parsed = &program.statements[1];

        assert!(matches!(parsed, Statement::Return(_, _)));
        assert_eq!(parsed.token().unwrap().literal, "return");
        assert_eq!(parsed.token().unwrap().token_type, TokenType::Return);
        assert!(matches!(
            *parsed.expr().unwrap(),
            Expression::IntegerLiteral(..)
        ));
    }

    /*
     * Testing Expressions with a single LiteralExpression.
     */
    struct LiteralExpressionTest<'a, T> {
        exp: &'a Expression,
        expected: T,
    }

    impl<'a, T> LiteralExpressionTest<'a, T> {
        fn new(exp: &'a Expression, expected: T) -> Self {
            LiteralExpressionTest { exp, expected }
        }
    }

    impl<'a> LiteralExpressionTest<'a, i32> {
        fn test(&self) {
            test_integer_literal(self.exp, self.expected)
        }
    }

    impl<'a> LiteralExpressionTest<'a, String> {
        fn test(&self) {
            test_identifier(self.exp, &self.expected)
        }
    }

    impl<'a> LiteralExpressionTest<'a, bool> {
        fn test(&self) {
            test_bool(self.exp, self.expected)
        }
    }

    #[test]
    fn test_literal_expression() {
        LiteralExpressionTest::new(
            &Expression::IntegerLiteral(Token::new(TokenType::Int, "10".to_string()), 10),
            10,
        )
        .test();

        LiteralExpressionTest::new(
            &Expression::Identifier(
                Token::new(TokenType::Ident, "foo".to_string()),
                "foo".to_string(),
            ),
            "foo".to_string(),
        )
        .test();

        LiteralExpressionTest::new(
            &Expression::Bool(Token::new(TokenType::True, "true".to_string()), true),
            true,
        )
        .test();

        LiteralExpressionTest::new(
            &Expression::Bool(Token::new(TokenType::True, "false".to_string()), false),
            false,
        )
        .test();
    }

    fn test_integer_literal(exp: &Expression, value: i32) {
        assert!(
            matches!(exp, Expression::IntegerLiteral(..)),
            "Expression not IntegerLiteral, got {exp:?}",
        );

        assert_eq!(
            exp.int().unwrap(),
            value,
            "IntegerLiteral doesn't have value {value}, got {:?}",
            exp.int().unwrap()
        );

        assert_eq!(
            exp.token_literal(),
            value.to_string(),
            "IntegerLiteral doesn't have literal {value}, got {:?}",
            exp.token_literal()
        );
    }

    fn test_identifier(exp: &Expression, value: &String) {
        assert!(
            matches!(exp, Expression::Identifier(..)),
            "Expression not Identifier, got {exp:?}",
        );

        assert_eq!(
            &exp.to_string(),
            value,
            "Identifier doesn't have value {value}, got {:?}",
            &exp.to_string()
        );

        assert_eq!(
            &exp.token_literal(),
            value,
            "Identifier doesn't have literal {value}, got {:?}",
            &exp.token_literal()
        );
    }

    fn test_bool(exp: &Expression, value: bool) {
        assert!(
            matches!(exp, Expression::Bool(..)),
            "Expression not Bool, got {exp:?}",
        );

        assert_eq!(
            &exp.buul().unwrap(),
            &value,
            "Bool doesn't have value {value}, got {:?}",
            &exp.buul().unwrap()
        );

        assert_eq!(
            &exp.token_literal(),
            &value.to_string(),
            "Identifier doesn't have literal {value}, got {:?}",
            &exp.token_literal()
        );
    }

    /*
     * Testing Expressions a single InfixExpression.
     */
    struct InfixTest<L, R> {
        input: String,
        left: L,
        operator: String,
        right: R,
    }

    impl<L, R> InfixTest<L, R> {
        fn new(input: &str, left: L, operator: &str, right: R) -> Self {
            Self {
                input: input.to_string(),
                left,
                operator: operator.to_string(),
                right,
            }
        }
    }

    impl InfixTest<i32, i32> {
        fn test(&self) {
            let program = init(&self.input);

            // Has the program correctly parsed it as a InfixExpression?
            assert!(matches!(
                program.statements[0].expr().unwrap(),
                Expression::Infix(_, _, _, _)
            ));

            // Does the program only contain 1 statement?
            assert_eq!(program.statements.len(), 1);

            LiteralExpressionTest::new(
                program.statements[0].expr().unwrap().left().unwrap(),
                self.left,
            )
            .test();
            // Has the Infix operator been parsed correctly?
            assert_eq!(
                program.statements[0].expr().unwrap().op().unwrap(),
                self.operator
            );

            LiteralExpressionTest::new(
                program.statements[0].expr().unwrap().right().unwrap(),
                self.right,
            )
            .test();
        }
    }

    impl InfixTest<&str, &str> {
        fn test(&self) {
            let program = init(&self.input);

            // Has the program correctly parsed it as a InfixExpression?
            assert!(matches!(
                program.statements[0].expr().unwrap(),
                Expression::Infix(_, _, _, _)
            ));

            // Does the program only contain 1 statement?
            assert_eq!(program.statements.len(), 1);

            LiteralExpressionTest::new(
                program.statements[0].expr().unwrap().left().unwrap(),
                self.left.to_owned(),
            )
            .test();

            // Has the Infix operator been parsed correctly?
            assert_eq!(
                program.statements[0].expr().unwrap().op().unwrap(),
                self.operator
            );

            LiteralExpressionTest::new(
                program.statements[0].expr().unwrap().right().unwrap(),
                self.right.to_owned(),
            )
            .test();
        }
    }

    impl InfixTest<bool, bool> {
        fn test(&self) {
            let program = init(&self.input);

            // Has the program correctly parsed it as a InfixExpression?
            assert!(matches!(
                program.statements[0].expr().unwrap(),
                Expression::Infix(_, _, _, _)
            ));

            // Does the program only contain 1 statement?
            assert_eq!(program.statements.len(), 1);

            LiteralExpressionTest::new(
                program.statements[0].expr().unwrap().left().unwrap(),
                self.left.to_owned(),
            )
            .test();

            // Has the Infix operator been parsed correctly?
            assert_eq!(
                program.statements[0].expr().unwrap().op().unwrap(),
                self.operator
            );

            LiteralExpressionTest::new(
                program.statements[0].expr().unwrap().right().unwrap(),
                self.right.to_owned(),
            )
            .test();
        }
    }

    #[test]
    fn test_infix_operations() {
        let infix_tests = vec![
            InfixTest::new("5+5", 5, "+", 5),
            InfixTest::new("5-5", 5, "-", 5),
            InfixTest::new("5*5", 5, "*", 5),
            InfixTest::new("5/5", 5, "/", 5),
            InfixTest::new("5>5", 5, ">", 5),
            InfixTest::new("5<5", 5, "<", 5),
            InfixTest::new("5==5", 5, "==", 5),
            InfixTest::new("5!=5", 5, "!=", 5),
        ];

        for test in infix_tests.into_iter() {
            test.test()
        }

        let infix_tests = vec![
            InfixTest::new("foo+bar", "foo", "+", "bar"),
            InfixTest::new("foo-bar", "foo", "-", "bar"),
            InfixTest::new("foo*bar", "foo", "*", "bar"),
            InfixTest::new("foo/bar", "foo", "/", "bar"),
            InfixTest::new("foo>bar", "foo", ">", "bar"),
            InfixTest::new("foo<bar", "foo", "<", "bar"),
            InfixTest::new("foo==bar", "foo", "==", "bar"),
            InfixTest::new("foo!=bar", "foo", "!=", "bar"),
        ];

        for test in infix_tests.into_iter() {
            test.test()
        }

        let infix_tests = vec![
            InfixTest::new("true==false", true, "==", false),
            InfixTest::new("true!=false", true, "!=", false),
        ];

        for test in infix_tests.into_iter() {
            test.test()
        }
    }

    #[test]
    fn test_longer_infix_operations() {
        let program = init("5+5+5");

        // Does the program only contain 1 statement?
        assert_eq!(program.statements.len(), 1);

        // Has the program correctly parsed it as a ExpressionStatement?
        assert!(matches!(program.statements[0], Statement::Expr(_, _)));
        assert!(matches!(
            program.statements[0].expr().unwrap(),
            Expression::Infix(..)
        ));
        assert!(matches!(
            **program.statements[0].expr().unwrap().left().unwrap(),
            Expression::Infix(..)
        ));
        assert!(matches!(
            **program.statements[0]
                .expr()
                .unwrap()
                .left()
                .unwrap()
                .left()
                .unwrap(),
            Expression::IntegerLiteral(..)
        ));
        assert!(matches!(
            **program.statements[0]
                .expr()
                .unwrap()
                .left()
                .unwrap()
                .right()
                .unwrap(),
            Expression::IntegerLiteral(..)
        ));
        assert!(matches!(
            **program.statements[0].expr().unwrap().right().unwrap(),
            Expression::IntegerLiteral(..)
        ));
    }

    /*
     * Testing Precedence parsing.
     */
    struct PrecedenceTest<'a> {
        input: &'a str,
        output: &'a str,
    }

    impl<'a> PrecedenceTest<'a> {
        fn new(input: &'a str, output: &'a str) -> Self {
            PrecedenceTest { input, output }
        }

        fn test(&self) {
            let mut program = init(self.input);
            assert_eq!(program.to_string(), self.output);
        }
    }

    #[test]
    fn test_precedence_parsing() {
        let tests = vec![
            PrecedenceTest::new("1+1+1", "((1 + 1) + 1)"),
            PrecedenceTest::new("1+1*1", "(1 + (1 * 1))"),
            PrecedenceTest::new("1+1*1/1", "(1 + ((1 * 1) / 1))"),
            PrecedenceTest::new("1+1*-1/1", "(1 + ((1 * (-1)) / 1))"),
            PrecedenceTest::new("1+-1*-1/1", "(1 + (((-1) * (-1)) / 1))"),
            PrecedenceTest::new(
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            PrecedenceTest::new("1*1<10", "((1 * 1) < 10)"),
            PrecedenceTest::new("1*1<10+1", "((1 * 1) < (10 + 1))"),
            PrecedenceTest::new("true == false == true", "((true == false) == true)"),
            PrecedenceTest::new("3 <    5 == true", "((3 < 5) == true)"),
            PrecedenceTest::new("3 <    5 == true", "((3 < 5) == true)"),
            // Parenthesis
            PrecedenceTest::new("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            PrecedenceTest::new("1 + (2 + 3) * 4", "(1 + ((2 + 3) * 4))"),
            PrecedenceTest::new("-(1 + 1)", "(-(1 + 1))"),
            PrecedenceTest::new(
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            PrecedenceTest::new(
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            PrecedenceTest::new("[1+1, 2*3, 10]", "[(1 + 1), (2 * 3), 10]"),
        ];

        for test in tests.into_iter() {
            test.test();
        }
    }

    #[test]
    #[should_panic(expected = "Left Parethesis never closed")]
    fn test_parenthesis_panic() {
        PrecedenceTest::new("-(1 + 1", "(-(1 + 1))").test();
        PrecedenceTest::new("1 + (2 + 3 * 4", "(1 + ((2 + 3) * 4))").test();
    }

    #[test]
    #[should_panic]
    fn test_parenthesis_panic2() {
        PrecedenceTest::new("[1,2,3)", "").test();
        PrecedenceTest::new("(1,1,3]", "").test();
    }

    /*
     * IfElse tests
     */
    #[test]
    fn test_if_expression() {
        let program = init("if (a<b) { a }");
        let expression = &program.statements[0].expr().unwrap();

        assert!(matches!(expression, Expression::If(..)));

        assert_eq!(expression.token_literal(), "if".to_string());

        let infix = expression.condition().unwrap();
        InfixTest::new(&infix.to_string(), "a", "<", "b");

        let consequence = expression.consequence().unwrap();
        assert!(matches!(**consequence, Statement::Block(..)));

        let alternative = expression.alternative().unwrap();
        assert!(alternative.is_none());
    }

    #[test]
    fn test_if_else_expression() {
        let program = init("if (a<b) { a } else { b }");

        let expression = &program.statements[0].expr().unwrap();

        assert!(matches!(expression, Expression::If(..)));

        let condition = expression.condition().unwrap();
        assert!(matches!(**condition, Expression::Infix(..)));

        assert_eq!(expression.token_literal(), "if".to_string());

        let infix = expression.condition().unwrap();
        InfixTest::new(&infix.to_string(), "a", "<", "b");

        let consequence = expression.consequence().unwrap();
        assert!(matches!(**consequence, Statement::Block(..)));

        let alternative = expression.alternative().unwrap().clone().unwrap();
        assert!(matches!(*alternative, Statement::Block(..)));
    }

    #[test]
    fn test_if_else_expression2() {
        let program = init("if (a<b) { a; b; c } else { b; b; c }");

        let expression = &program.statements[0].expr().unwrap();

        assert!(matches!(expression, Expression::If(..)));

        assert_eq!(expression.token_literal(), "if".to_string());

        let infix = expression.condition().unwrap();
        InfixTest::new(&infix.to_string(), "a", "<", "b");

        let consequence = expression.consequence().unwrap();
        assert!(matches!(**consequence, Statement::Block(..)));
        assert_eq!(consequence.len(), 3);

        let alternative = expression.alternative().unwrap().clone().unwrap();
        assert!(matches!(*alternative, Statement::Block(..)));
        assert_eq!(alternative.len(), 3);
    }

    /*
     * FunctionExpression tests
     */
    struct FuncTest {
        input: String,
        params: Vec<String>,
    }

    impl FuncTest {
        fn new(input: &str, params: Vec<&str>) -> Self {
            FuncTest {
                input: input.to_string(),
                params: params.iter().map(|x| x.to_string()).collect(),
            }
        }
    }

    #[test]
    fn test_func_expression() {
        let cases = vec![
            FuncTest::new("func() { return 1; }", vec![]),
            FuncTest::new("func(a) { return a; }", vec!["a"]),
            FuncTest::new("func(a, b) { return a + b; }", vec!["a", "b"]),
        ];

        for case in cases {
            let program = init(&case.input);
            let expr = &program.statements[0].expr().unwrap();
            let params = expr.params().unwrap();
            let body = expr.body().unwrap();

            assert_eq!(program.len(), 1);
            assert!(matches!(expr, Expression::Func(..)));
            assert_eq!(params.len(), case.params.len());
            assert!(params
                .iter()
                .zip(case.params)
                .all(|(a, b)| a.token_literal() == b));
            assert!(matches!(**body, Statement::Block(..)));
            assert_eq!(body.len(), 1);
        }
    }

    /*
     * FunctionExpression tests
     */
    struct CallTest {
        input: String,
        identifier: String,
        args: Vec<String>,
    }

    impl CallTest {
        fn new(input: &str, identifier: &str, args: Vec<&str>) -> Self {
            CallTest {
                input: input.to_string(),
                identifier: identifier.to_string(),
                args: args.iter().map(|x| x.to_string()).collect(),
            }
        }
    }
    #[test]
    fn test_call_expression() {
        let cases = vec![
            CallTest::new("foo()", "foo", vec![]),
            CallTest::new("foo(x)", "foo", vec!["x"]),
            CallTest::new("foo(x,y)", "foo", vec!["x", "y"]),
            CallTest::new("func() { return 1; }()", "func() { return 1; }", vec![]),
            CallTest::new(
                "func(x) { return x; }(a)",
                "func(x) { return x; }",
                vec!["a"],
            ),
            CallTest::new(
                "func(x, y) { return x + y; }(a, b)",
                "func(x, y) { return (x + y); }",
                vec!["a", "b"],
            ),
            CallTest::new("foo((a+b), (-b))", "foo", vec!["(a + b)", "(-b)"]),
            CallTest::new(
                "func(x, y) { return a + b; }((a+b), (-b))",
                "func(x, y) { return (a + b); }",
                vec!["(a + b)", "(-b)"],
            ),
            CallTest::new("foo(1, 2, foo(3, 4))", "foo", vec!["1", "2", "foo(3, 4)"]),
        ];

        for case in cases {
            let program = init(&case.input);
            let expr = &program.statements[0].expr().unwrap();
            let name = expr.name().unwrap();
            let args = expr.args().unwrap();

            assert_eq!(program.len(), 1);
            assert!(matches!(expr, Expression::Call(..)));
            assert_eq!(args.len(), case.args.len());
            assert!(args.iter().zip(case.args).all(|(a, b)| a.to_string() == b));
            assert_eq!(name.to_string(), case.identifier);
        }
    }

    /*
     * FunctionExpression tests
     */
    struct ProgramTest {
        input: String,
        statements: Vec<String>,
    }
    impl ProgramTest {
        fn new(input: &str, statements: Vec<&str>) -> Self {
            ProgramTest {
                input: input.to_string(),
                statements: statements.iter().map(|x| x.to_string()).collect(),
            }
        }

        fn str(&self) -> String {
            self.statements
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        }
    }

    #[test]
    fn test_program_parsing() {
        let cases = vec![
            ProgramTest::new(
                "let x = 1; let y = 2; let z = x + y;",
                vec!["let x = 1;", "let y = 2;", "let z = (x + y);"],
            ),
            ProgramTest::new(
                "if(a) { b } else { c }; func(a) { return a; }; let z = x + y;",
                vec![
                    "if(a) { b } else { c };",
                    "func(a) { return a; };",
                    "let z = (x + y);",
                ],
            ),
            ProgramTest::new(
                "if(a) { b } else { c }; func(a) { 1+1; return a; }; let z = x + y;",
                vec![
                    "if(a) { b } else { c };",
                    "func(a) { (1 + 1); return a; };",
                    "let z = (x + y);",
                ],
            ),
        ];

        for case in cases {
            let program = init(&case.input);
            assert_eq!(program.len(), case.statements.len());
            // assert!(program
            //     .statements
            //     .iter()
            //     .zip(case.statements)
            //     .all(|(a, b)| a.to_string() == b));
        }
    }

    #[test]
    fn test_hashmap_parsing() {
        let cases = vec![
            "{}",
            r#"{"hello": 1}"#,
            r#"{"hello": 1, true: 2, 3: "three"}"#,
        ];

        for case in cases {
            let program = init(case);
            let expr = program.statements[0].expr().unwrap();

            assert!(matches!(expr, Expression::HashMap(..)))
        }
    }
}
