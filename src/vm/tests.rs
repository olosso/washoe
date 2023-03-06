#[cfg(test)]
mod vm_tests {
    use super::*;
    use crate::compiler::Compiler;
    use crate::object::Object;
    use crate::object::Object::*;
    use crate::vm::globals::Globals;
    use crate::vm::stack::Stack;
    use crate::vm::vm::VM;

    mod helpers {

        use super::*;

        pub fn parse(input: &str) -> crate::ast::Program {
            let lexer = crate::lexer::Lexer::new(input.to_string());
            let mut parser = crate::parser::Parser::new(lexer);
            parser.parse_program().unwrap()
        }

        pub fn test_integer_object(a: &Object, e: &Object) {
            assert!(matches!(a, Object::Integer(..)));
            if let Object::Integer(i) = a {
                if let Object::Integer(j) = e {
                    assert_eq!(i, j);
                }
            }
        }

        pub fn test_boolean_object(a: &Object, e: &Object) {
            assert!(matches!(a, Object::Boolean(..)));
            if let Object::Boolean(i) = a {
                if let Object::Boolean(j) = e {
                    assert_eq!(i, j);
                }
            }
        }

        pub fn test_null_object(a: &Object, e: &Object) {
            assert!(matches!(a, Object::Null));
        }

        pub struct VMCase<'s> {
            pub input: &'s str,
            pub expected: Object,
        }

        pub fn test_vm_run(cases: &[VMCase]) {
            for case in cases {
                let program = parse(case.input);

                // Generation
                let mut compiler = Compiler::default();
                compiler.compile(program);
                let mut globals = Globals::new();
                let mut stack = Stack::new();
                let mut vm = VM::new(compiler.bytecode(), &mut globals, &mut stack);
                vm.run();

                // Confirmation
                let top = vm.last_popped_obj();
                test_expected_object(top, &case.expected);
            }
        }

        pub fn test_expected_object(actual: &Object, expected: &Object) {
            use Object::*;
            match expected {
                Integer(_) => test_integer_object(actual, expected),
                Boolean(_) => test_boolean_object(actual, expected),
                Null => test_null_object(actual, expected),
                _ => todo!(),
            }
        }
    }

    mod tests {
        use super::helpers::*;
        use super::*;

        #[test]
        fn test_integer_arithmetic() {
            let cases = [
                VMCase {
                    input: "1",
                    expected: Integer(1),
                },
                VMCase {
                    input: "7 + 8",
                    expected: Integer(15),
                },
                VMCase {
                    input: "2 - 1",
                    expected: Integer(1),
                },
                VMCase {
                    input: "10 * 2",
                    expected: Integer(20),
                },
                VMCase {
                    input: "7 + 8 * 3",
                    expected: Integer(31),
                },
                VMCase {
                    input: "7 * 8 + 3",
                    expected: Integer(59),
                },
                VMCase {
                    input: "(7 + 8) * 3",
                    expected: Integer(45),
                },
                VMCase {
                    input: "7 * 9 / 3",
                    expected: Integer(21),
                },
                VMCase {
                    input: "1 + 3 * 3 - -10",
                    expected: Integer(20),
                },
            ];

            test_vm_run(&cases);
        }

        #[test]
        fn test_boolean_eval() {
            let cases = [
                VMCase {
                    input: "true",
                    expected: Boolean(true),
                },
                VMCase {
                    input: "false",
                    expected: Boolean(false),
                },
                VMCase {
                    input: "true == true",
                    expected: Boolean(true),
                },
                VMCase {
                    input: "true == false",
                    expected: Boolean(false),
                },
                VMCase {
                    input: "true != false",
                    expected: Boolean(true),
                },
                VMCase {
                    input: "1 > 2",
                    expected: Boolean(false),
                },
                VMCase {
                    input: "1 < 2",
                    expected: Boolean(true),
                },
                VMCase {
                    input: "(1 + 2) > 2",
                    expected: Boolean(true),
                },
                VMCase {
                    input: "(1 + 2) < (3 * 10)",
                    expected: Boolean(true),
                },
                VMCase {
                    input: "(1 > 2) == true",
                    expected: Boolean(false),
                },
                VMCase {
                    input: "(1 < 2) == true",
                    expected: Boolean(true),
                },
                VMCase {
                    input: "(1 < -2) == !true",
                    expected: Boolean(true),
                },
                VMCase {
                    input: "!!!true",
                    expected: Boolean(false),
                },
                VMCase {
                    input: "!(if (false) { 10 };)",
                    expected: Boolean(true),
                },
            ];

            test_vm_run(&cases);
        }

        #[test]
        fn test_if_eval() {
            let cases = [
                VMCase {
                    input: "if (true) { 23 }",
                    expected: Integer(23),
                },
                VMCase {
                    input: "if (false) { 23 }",
                    expected: Null,
                },
                VMCase {
                    input: "if (false) { 23 }; 42;",
                    expected: Integer(42),
                },
                VMCase {
                    input: "if (4 < 8) { 23 }",
                    expected: Integer(23),
                },
                VMCase {
                    input: "if (4 > 8) { 23 } else { 42 }",
                    expected: Integer(42),
                },
                VMCase {
                    input: "if (4 < 8) { 4; 8; 15; 16; 23; 42 } else { 0 }",
                    expected: Integer(42),
                },
                VMCase {
                    input: "if (4 > 8) { 4; 8; 15; 16; 23; 42 } else { 0; 1; 2; }",
                    expected: Integer(2),
                },
                VMCase {
                    input: "if (if (false) { 10 };) { 10 } else { 20 }",
                    expected: Integer(20),
                },
            ];

            test_vm_run(&cases);
        }

        #[test]
        fn test_global_let() {
            let cases = [
                VMCase {
                    input: "let x = 1; x;",
                    expected: Integer(1),
                },
                VMCase {
                    input: "let x = 1; let y = x; y",
                    expected: Integer(1),
                },
                VMCase {
                    input: "let x = 1; let y = 2; let z = x + y; z",
                    expected: Integer(3),
                },
            ];

            test_vm_run(&cases);
        }
    }
}
