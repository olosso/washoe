#[cfg(test)]
mod evaluator_tests {
    use crate::ast::*;
    use crate::evaluator::*;
    use crate::lexer::*;
    use crate::object::*;
    use crate::parser::*;

    fn init(source_code: &str) -> Program {
        let lexer = Lexer::new(source_code.to_string());
        let mut parser = Parser::new(lexer);
        parser.parse_program().unwrap()
    }

    fn eval_fresh(p: &Program) -> Result<Object, EvalError> {
        let env = Rc::new(Environment::global());
        eval(p, &env)
    }

    /*
     * Integer
     */
    struct IntegerEvalTest {
        input: String,
        expected: i32,
        program: Program,
    }

    impl IntegerEvalTest {
        fn new(input: &str, expected: i32) -> Self {
            IntegerEvalTest {
                input: input.to_string(),
                expected,
                program: init(input),
            }
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let cases = vec![
            IntegerEvalTest::new("5", 5),
            IntegerEvalTest::new("10", 10),
            IntegerEvalTest::new("-5", -5),
            IntegerEvalTest::new("-10", -10),
            IntegerEvalTest::new("5 + 5", 10),
            IntegerEvalTest::new("5 + 5 + 5 + 5 - 10", 10),
            IntegerEvalTest::new("2 * 2 * 2 * 2 * 2", 32),
            IntegerEvalTest::new("-50 + 100 + -50", 0),
            IntegerEvalTest::new("5 * 2 + 10", 20),
            IntegerEvalTest::new("5 + 2 * 10", 25),
            IntegerEvalTest::new("20 + 2 * -10", 0),
            IntegerEvalTest::new("50 / 2 * 2 + 10", 60),
            IntegerEvalTest::new("2 * (5 + 10)", 30),
            IntegerEvalTest::new("3 * 3 * 3 + 10", 37),
            IntegerEvalTest::new("3 * (3 * 3) + 10", 37),
            IntegerEvalTest::new("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for case in cases {
            let value = eval_fresh(&case.program).unwrap();
            test_integer_object(value, case.expected);
        }
    }

    fn test_integer_object(obj: Object, expected: i32) {
        assert_eq!(obj.obtype(), Type::INTEGER);
        assert_eq!(obj.as_int().unwrap(), expected);
    }

    /*
     * Boolean
     */
    struct BoolEvalTest {
        input: String,
        expected: bool,
        program: Program,
    }

    impl BoolEvalTest {
        fn new(input: &str, expected: bool) -> Self {
            BoolEvalTest {
                input: input.to_string(),
                expected,
                program: init(input),
            }
        }
    }

    #[test]
    fn test_eval_bool_expression() {
        let cases = vec![
            BoolEvalTest::new("true", true),
            BoolEvalTest::new("false", false),
            BoolEvalTest::new("1 < 2", true),
            BoolEvalTest::new("1 > 2", false),
            BoolEvalTest::new("1 < 1", false),
            BoolEvalTest::new("1 > 1", false),
            BoolEvalTest::new("1 == 1", true),
            BoolEvalTest::new("1 == 2", false),
            BoolEvalTest::new("1 != 2", true),
            BoolEvalTest::new("1 != 1", false),
            BoolEvalTest::new("true == true", true),
            BoolEvalTest::new("false == false", true),
            BoolEvalTest::new("true == false", false),
            BoolEvalTest::new("false == true", false),
            BoolEvalTest::new("true != false", true),
            BoolEvalTest::new("false != false", false),
            BoolEvalTest::new("(1 < 2) == true", true),
            BoolEvalTest::new("(1 > 2) == true", false),
            BoolEvalTest::new("(1 < 2) == false", false),
        ];

        for case in cases {
            let value = eval_fresh(&case.program).unwrap();
            test_bool_object(value, case.expected);
        }
    }

    fn test_bool_object(obj: Object, expected: bool) {
        assert_eq!(obj.obtype(), Type::BOOLEAN);
        assert_eq!(obj.as_bool(), expected);
    }

    /*
     * Bang
     */
    struct BangEvalTest {
        input: String,
        expected: bool,
        program: Program,
    }

    impl BangEvalTest {
        fn new(input: &str, expected: bool) -> Self {
            BangEvalTest {
                input: input.to_string(),
                expected,
                program: init(input),
            }
        }
    }

    #[test]
    fn test_eval_bang_expression() {
        let cases = vec![
            BangEvalTest::new("!true", false),
            BangEvalTest::new("!false", true),
            BangEvalTest::new("!!true", true),
            BangEvalTest::new("!5", false),
            BangEvalTest::new("!!5", true),
        ];

        for case in cases {
            let value = eval_fresh(&case.program).unwrap();
            test_bool_object(value, case.expected);
        }
    }

    /*
     * Bang
     */
    struct IfElseTest {
        input: String,
        expected: i32,
        program: Program,
    }

    impl IfElseTest {
        fn new(input: &str, expected: i32) -> Self {
            IfElseTest {
                input: input.to_string(),
                expected,
                program: init(input),
            }
        }
    }

    #[test]
    fn test_eval_ifelse_expression() {
        let cases = vec![
            IfElseTest::new("if (true) { 1 }", 1),
            IfElseTest::new("if (false) { 1 } else { 2 }", 2),
            IfElseTest::new("if (1<2) { 1 } else { 2 }", 1),
            IfElseTest::new("if ((1<2)==true) { 1 } else { 2 }", 1),
            IfElseTest::new("if (1) { 1 } else { 2 }", 1),
            IfElseTest::new("if (0) { 1 } else { 2 }", 2),
            IfElseTest::new("if (false) { 1 }", 0),
            IfElseTest::new("if (false) { 1 } else { 2 }", 2),
            IfElseTest::new("let x = 1; if (true) { x + 2; } else { 2 }", 3),
        ];

        for case in cases {
            let value = eval_fresh(&case.program).unwrap();
            if case.expected == 0 {
                assert!(value.is_null())
            } else {
                assert_eq!(value.as_int().unwrap(), case.expected)
            }
        }
    }

    /*
     * TestReturn
     */
    struct ReturnTest {
        input: String,
        expected: i32,
        program: Program,
    }

    impl ReturnTest {
        fn new(input: &str, expected: i32) -> Self {
            ReturnTest {
                input: input.to_string(),
                expected,
                program: init(input),
            }
        }
    }

    #[test]
    fn test_eval_return_statements() {
        let cases = vec![
            ReturnTest::new("return 10; 5;", 10),
            ReturnTest::new("return 10; return 5;", 10),
            ReturnTest::new("10; return 5;", 5),
            ReturnTest::new("1; return 1+1; 5;", 2),
            ReturnTest::new("return 1+1; 1; 5;", 2),
            ReturnTest::new("1; 1; return 1+1;", 2),
            ReturnTest::new(
                "
if (10 > 1) {
if (10 > 1) {
return 10;
}
return 1;
}
",
                10,
            ),
            ReturnTest::new("if(1) { return 1; true + true; }", 1),
        ];

        for case in cases {
            let value = eval_fresh(&case.program).unwrap();
            assert_eq!(value.as_int().unwrap(), case.expected)
        }
    }

    /*
     * TestEvalError
     */
    struct EvalErrorTest {
        input: String,
        expected: String,
        program: Program,
    }

    impl EvalErrorTest {
        fn new(input: &str, expected: &str) -> Self {
            EvalErrorTest {
                input: input.to_string(),
                expected: expected.to_string(),
                program: init(input),
            }
        }
    }

    #[test]
    fn test_eval_error_statements() {
        let cases = vec![
            EvalErrorTest::new("true + true", "Evaluation error"),
            EvalErrorTest::new("if(1) { true + true; 1 }", "Evaluation error"),
            EvalErrorTest::new("if(1) { true + true; return 1 }", "Evaluation error"),
            EvalErrorTest::new("let a = 1; a+b", "Evaluation error"),
            EvalErrorTest::new("let a = 1; a+b", "Evaluation error"),
            EvalErrorTest::new("if (true) { let x + 2; }; x;", "Evaluation error"),
            EvalErrorTest::new("len(true)", "Bad arguments"),
            EvalErrorTest::new("len(1)", "Bad arguments"),
        ];

        for case in cases {
            let value = eval_fresh(&case.program);
            assert!(matches!(value, Err(EvalError(..))))
        }
    }

    /*
     * TestLetStatement
     */
    struct LetTest {
        input: String,
        expected: i32,
        program: Program,
    }

    impl LetTest {
        fn new(input: &str, expected: i32) -> Self {
            LetTest {
                input: input.to_string(),
                expected,
                program: init(input),
            }
        }
    }

    #[test]
    fn test_eval_let_statements() {
        let cases = vec![
            LetTest::new("let a = 1; a", 1),
            LetTest::new("let a = 1; let b = 2; a+b", 3),
            LetTest::new("let a = 1; let b = a + 1; b", 2),
        ];

        for case in cases {
            let value = eval_fresh(&case.program).unwrap();
            assert_eq!(value.as_int().unwrap(), case.expected)
        }
    }

    /*
     * TestFunctionLiteral
     */
    struct FunctionTest {
        input: String,
        program: Program,
    }

    impl FunctionTest {
        fn new(input: &str) -> Self {
            FunctionTest {
                input: input.to_string(),
                program: init(input),
            }
        }
    }

    #[test]
    fn test_func_objects() {
        let case = FunctionTest::new("func(x, y) { x + 1; (x + 3); x + y };");
        let value = eval_fresh(&case.program).unwrap();

        assert!(matches!(value, Object::Function(..)));
        assert_eq!(value.inspect(), "func(x, y) { (x + 1); (x + 3); (x + y) }");
        assert_eq!(value.body().unwrap(), "{ (x + 1); (x + 3); (x + y) }");
        assert_eq!(value.params().unwrap(), "(x, y)");
        assert_eq!(value.env().unwrap(), "");
    }

    /*
     * TestFunctionCall
     */
    struct TestFunctionCall {
        input: String,
        expected: i32,
        program: Program,
    }

    impl TestFunctionCall {
        fn new(input: &str, expected: i32) -> Self {
            TestFunctionCall {
                input: input.to_string(),
                expected,
                program: init(input),
            }
        }
    }

    #[test]
    fn test_func_call() {
        let cases = vec![
            TestFunctionCall::new("let id = func(x) { x; }; id(1);", 1),
            TestFunctionCall::new("let add = func(a,b) { a+b; }; add(1,2);", 3),
            TestFunctionCall::new("let add = func(a,b) { a+b; }; add(1, add(1,2));", 4),
            TestFunctionCall::new("func(x) { x; }(0);", 0),
        ];

        for case in cases {
            let value = eval_fresh(&case.program).unwrap();
            assert_eq!(value.as_int().unwrap(), case.expected)
        }
    }

    #[test]
    fn test_closure() {
        let program = init(
            "let newAdder = func(x) {
func(y) { x + y };
};
let addTwo = newAdder(2);
addTwo(2);",
        );
        let value = eval_fresh(&program).unwrap();

        assert!(matches!(value, Object::Integer(..)));
        assert!(matches!(value.as_int().unwrap(), 4));
    }

    #[test]
    fn test_builtin_len() {
        let cases = vec![
            IntegerEvalTest::new(r#"len("Hello")"#, 5),
            IntegerEvalTest::new(r#"len("Hello paganini")"#, 14),
            IntegerEvalTest::new(r#"len("")"#, 0),
            IntegerEvalTest::new(r#"len(" ")"#, 1),
        ];

        for case in cases {
            let value = eval_fresh(&case.program).unwrap();
            test_integer_object(value, case.expected);
        }
    }
}
