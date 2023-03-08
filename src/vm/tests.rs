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

        pub fn test_string_object(a: &Object, e: &Object) {
            assert!(matches!(a, Object::String(..)));
            if let Object::String(i) = a {
                if let Object::String(j) = e {
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

        pub fn test_array_object(a: &Object, e: &Object) {
            assert!(matches!(a, Object::Array(_)));
            if let Object::Array(objs_i) = a {
                if let Object::Array(objs_j) = e {
                    for (i, j) in objs_i.iter().zip(objs_j) {
                        test_expected_object(i, j);
                    }
                }
            }
        }

        pub fn test_hashmap_object(a: &Object, e: &Object) {
            assert!(matches!(a, Object::HashMap(_)));
            dbg!(a);
            dbg!(e);
            if let Object::HashMap(hm_i) = a {
                if let Object::HashMap(hm_j) = e {
                    assert!(hm_i == hm_j)
                }
            }
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
                String(_) => test_string_object(actual, expected),
                Boolean(_) => test_boolean_object(actual, expected),
                Null => test_null_object(actual, expected),
                Array(_) => test_array_object(actual, expected),
                HashMap(_) => test_hashmap_object(actual, expected),
                _ => todo!(),
            }
        }
    }

    mod tests {
        use std::collections::HashMap;

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

        #[test]
        fn test_string_expressions() {
            let cases = [
                VMCase {
                    input: r#" "yoyo"; "#,
                    expected: String("yoyo".to_string()),
                },
                VMCase {
                    input: r#" "yo" + "yo"; "#,
                    expected: String("yoyo".to_string()),
                },
            ];

            test_vm_run(&cases);
        }

        #[test]
        fn test_array_expressions() {
            let cases = [
                VMCase {
                    input: "[]",
                    expected: Array(vec![]),
                },
                VMCase {
                    input: "[1, 2, 3]",
                    expected: Array(vec![Integer(1), Integer(2), Integer(3)]),
                },
                VMCase {
                    input: "[1 + 10, 2 + 20, 3 + 30]",
                    expected: Array(vec![Integer(11), Integer(22), Integer(33)]),
                },
            ];

            test_vm_run(&cases);
        }

        #[test]
        fn test_hashmap_expressions() {
            let cases = [
                VMCase {
                    input: "{}",
                    expected: HashMap(HashMap::new()),
                },
                VMCase {
                    input: "{1: 10, 2: 20, 3: 30}",
                    expected: HashMap(HashMap::from([
                        (Integer(1), Integer(10)),
                        (Integer(2), Integer(20)),
                        (Integer(3), Integer(30)),
                    ])),
                },
                VMCase {
                    input: "{1 + 2: 10 + 1, 2 + 2: 20 + 1}",
                    expected: HashMap(HashMap::from([
                        (Integer(3), Integer(11)),
                        (Integer(4), Integer(21)),
                    ])),
                },
            ];

            test_vm_run(&cases);
        }

        #[test]
        fn test_indexing_expressions() {
            let cases = [
                VMCase {
                    input: "[1][0]",
                    expected: Integer(1),
                },
                VMCase {
                    input: "[][0]",
                    expected: Null,
                },
                VMCase {
                    input: "{1: 10, 2: 20, 3: 30}[1]",
                    expected: Integer(10),
                },
                VMCase {
                    input: "{1: 10, 2: 20, 3: 30}[1 + 1]",
                    expected: Integer(20),
                },
                VMCase {
                    input: "[1, 2, 3][-1]",
                    expected: Null,
                },
                VMCase {
                    input: "{1: 10, 2: 20, 3: 30}[100]",
                    expected: Null,
                },
                VMCase {
                    input: "[[1, 2, 3]][0][0]",
                    expected: Integer(1),
                },
            ];

            test_vm_run(&cases);
        }
    }
}
