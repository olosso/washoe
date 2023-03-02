use crate::ast;
use crate::ast::{Expression, Statement};
use crate::code::{self, Instructions, Op};
use crate::object::Object;
use crate::object::Object::*;
use ast::Expression::*;
use ast::Statement::*;

/*
 * @COMPILER::COMPILER
 */
#[derive(Default, Debug)]
pub struct Compiler {
    instructions: code::Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    /*
     * Given an AST, produce bytecode.
     */
    pub fn compile(&mut self, program: ast::Program) {
        for statement in program.statements {
            self.c_statement(&statement);
        }
    }

    fn c_statement(&mut self, stmt: &Statement) {
        match stmt {
            Expr(_, e) => {
                self.c_expression(e);
                self.emit(Op::Pop, None);
            }
            _ => todo!(),
        }
    }

    fn c_expression(&mut self, expr: &Expression) {
        match expr {
            Bool(_, b) => {
                if (*b) {
                    self.emit(Op::True, None);
                } else {
                    self.emit(Op::False, None);
                }
            }
            IntegerLiteral(_, i) => {
                let obj = Integer(*i);
                let pos = [self.add_constant(obj) as u32];
                self.emit(Op::Constant, Some(&pos));
            }
            Prefix(_, op, r) => {
                self.c_expression(r);

                match op.as_str() {
                    "-" => self.emit(Op::Minus, None),
                    "!" => self.emit(Op::Bang, None),
                    _ => todo!(),
                };
            }
            Infix(_, l, op, r) => {
                if op == "<" {
                    self.c_expression(r);
                    self.c_expression(l);
                } else {
                    self.c_expression(l);
                    self.c_expression(r);
                }

                match op.as_str() {
                    "+" => self.emit(Op::Add, None),
                    "-" => self.emit(Op::Sub, None),
                    "*" => self.emit(Op::Mul, None),
                    "/" => self.emit(Op::Div, None),
                    "==" => self.emit(Op::Equal, None),
                    "!=" => self.emit(Op::NotEqual, None),
                    ">" | "<" => self.emit(Op::GT, None),
                    _ => todo!(),
                };
            }
            _ => todo!(),
        }
    }

    /// Add an Object to the constant pool.
    /// Returns the index of the constant.
    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    /// This is the internal function that the compiler uses to
    /// add instructions to its private instructions field.
    fn emit(&mut self, op: code::Op, operands: Option<&[u32]>) -> usize {
        let mut ins = code::make(op, operands);
        self.add_instruction(&mut ins)
    }

    /// A helper function for self.emit.
    fn add_instruction(&mut self, ins: &mut Instructions) -> usize {
        let pos_new_ins = self.instructions.len();
        self.instructions.append(ins);
        pos_new_ins
    }

    /// Public function to transfer instructions and constants
    /// to the VM.
    /// NOTE this consumes the Compiler.
    pub fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}

/*
 * @COMPILER::BYTECODE
 */
pub struct Bytecode {
    pub instructions: code::Instructions,
    pub constants: Vec<Object>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::code::{Instructions, Op::*};
    use crate::object::Object::*;

    fn parse(input: &str) -> crate::ast::Program {
        let lexer = crate::lexer::Lexer::new(input.to_string());
        let mut parser = crate::parser::Parser::new(lexer);
        parser.parse_program().unwrap()
    }

    struct CompilerCase<'s> {
        input: &'s str,
        expected_constants: Vec<Object>,
        expected_instructions: &'s [code::Instructions],
    }

    #[test]
    fn test_integer_arithmetic() {
        use code::make;
        let cases = [
            CompilerCase {
                input: "1; 2;",
                expected_constants: vec![Integer(1), Integer(2)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Pop, None),
                    make(Constant, Some(&[1])),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "3 + 2",
                expected_constants: vec![Integer(3), Integer(2)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(Add, None),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "3 + 2 + 5",
                expected_constants: vec![Integer(3), Integer(2), Integer(5)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(Add, None),
                    make(Constant, Some(&[2])),
                    make(Add, None),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "3 / 2",
                expected_constants: vec![Integer(3), Integer(2)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(Div, None),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                /*
                 *    +
                 *   / \
                 *  3   *
                 *     / \
                 *    2   5
                 */
                input: "3 + 2 * 5",
                // NOTE Notice the order of the objects in the constant pool!!
                expected_constants: vec![Integer(3), Integer(2), Integer(5)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(Constant, Some(&[2])),
                    make(Mul, None),
                    make(Add, None),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "true;",
                expected_constants: vec![],
                expected_instructions: &[make(True, None), make(Pop, None)],
            },
            CompilerCase {
                input: "false",
                expected_constants: vec![],
                expected_instructions: &[make(False, None), make(Pop, None)],
            },
            CompilerCase {
                input: "1 == 2",
                expected_constants: vec![Integer(1), Integer(2)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(Equal, None),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "1 != 2",
                expected_constants: vec![Integer(1), Integer(2)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(NotEqual, None),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "1 < 2",
                expected_constants: vec![Integer(2), Integer(1)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(GT, None),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "1 > 2",
                expected_constants: vec![Integer(1), Integer(2)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(GT, None),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "(1 + 3) > 2",
                expected_constants: vec![Integer(1), Integer(3), Integer(2)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(Add, None),
                    make(Constant, Some(&[2])),
                    make(GT, None),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "(1 + 3) < 2",
                expected_constants: vec![Integer(2), Integer(1), Integer(3)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(Constant, Some(&[2])),
                    make(Add, None),
                    make(GT, None),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "-(1 + 3)",
                expected_constants: vec![Integer(1), Integer(3)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(Add, None),
                    make(Minus, None),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "!true",
                expected_constants: vec![],
                expected_instructions: &[make(True, None), make(Bang, None), make(Pop, None)],
            },
        ];

        test_compiler_cases(&cases);
    }

    fn test_compiler_cases(cases: &[CompilerCase]) {
        for case in cases {
            let program = parse(case.input);
            let mut compiler = Compiler::default();

            compiler.compile(program);

            let bytecode = compiler.bytecode();

            test_constants(bytecode.constants, &case.expected_constants);
            test_instructions(bytecode.instructions, case.expected_instructions);
        }
    }

    fn test_constants(actual: Vec<Object>, expected: &Vec<Object>) {
        assert_eq!(
            actual.len(),
            expected.len(),
            "Constant pool doesn't have the same number of objects."
        );

        for (a, e) in actual.iter().zip(expected) {
            match a {
                Integer(i) => test_integer_object(a, e),
                _ => todo!(),
            }
        }
    }

    fn test_integer_object(a: &Object, e: &Object) {
        assert!(matches!(e, Object::Integer(..)));
        if let Object::Integer(i) = a {
            if let Object::Integer(j) = e {
                assert_eq!(i, j, "Constant pool not the same.");
            }
        }
    }

    fn test_instructions(actual: code::Instructions, expected: &[code::Instructions]) {
        let concat = concat_instructions(expected);

        assert_eq!(
            actual.len(),
            concat.len(),
            "Instruction lengths in bytes not the same."
        );
        assert!(
            actual.iter().eq(concat.iter()),
            "Instruction contents not the same."
        );
    }

    fn concat_instructions(inss: &[code::Instructions]) -> code::Instructions {
        let mut cat = vec![];
        for ins in inss {
            let mut v = ins.0.clone();
            cat.append(&mut v);
        }
        Instructions::new(cat)
    }
}
