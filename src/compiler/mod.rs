use crate::ast::{self, Node};
use crate::ast::{Expression, Statement};
use crate::code::{self, Instructions, Op};
use crate::object::Object;
use crate::object::Object::*;
use ast::Expression::*;
use ast::Statement::*;

mod symbol_table;
/*
 * @COMPILER::COMPILER
 */
#[derive(Default, Debug)]
pub struct Compiler {
    instructions: code::Instructions,
    constants: Vec<Object>,
    table: symbol_table::SymbolTable,
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
            Expr(_, expr) => {
                self.c_expression(expr);
                self.emit(Op::Pop, None);
            }
            Block(_, statements) => {
                for statement in statements {
                    self.c_statement(statement);
                }
            }
            Let(_, name, expr) => {
                self.c_expression(expr);
                // NOTE This unwrap cannot fail.
                let symbol = self.table.define(&name.to_string());
                self.emit(Op::SetGlobal, Some(&[symbol.index]));
            }
            _ => todo!(),
        }
    }

    fn c_expression(&mut self, expr: &Expression) {
        match expr {
            Identifier(_, name) => {
                let index = if let Some(symbol) = self.table.resolve(name) {
                    symbol.index
                } else {
                    panic!("No binding found for symbol with name {name}");
                };

                self.emit(Op::GetGlobal, Some(&[index]));
            }
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
            StringLiteral(_, string) => {
                let obj = String(string.to_string());
                let pos = [self.add_constant(obj) as u32];
                self.emit(Op::Constant, Some(&pos));
            }
            Expression::Array(_, expressions) => {
                for expression in expressions {
                    self.c_expression(expression);
                }
                self.emit(Op::BuildArray, Some(&[expressions.len() as u32]));
            }
            Expression::HashMap(_, keys, values) => {
                for (key, val) in keys.iter().zip(values) {
                    self.c_expression(key);
                    self.c_expression(val);
                }
                self.emit(Op::BuildHashMap, Some(&[(keys.len() * 2) as u32]));
            }
            Index(_, container, index) => {
                self.c_expression(container);
                self.c_expression(index);
                self.emit(Op::Index, None);
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
            If(_, cond, cons, alt) => {
                self.c_expression(cond);

                // object index
                let jump_cond_obj = [self.add_constant(Object::Null) as u32];

                // jump instruction number
                let jump_cond_ins = self.emit(Op::JumpMaybe, Some(&jump_cond_obj));

                self.c_statement(cons);

                // Remove Pop of the last expression in the condition block
                self.instructions.pop();

                // jump offset
                let mut cons_len = self.instructions.len() as i32 - jump_cond_ins as i32;

                let jump_uncond_obj = [self.add_constant(Object::Null) as u32];
                let jump_uncond_ins = self.emit(Op::Jump, Some(&jump_uncond_obj));
                if let Some(alt) = alt {
                    self.c_statement(alt);
                    // Remove Pop of the last expression in the alternative block
                    self.instructions.pop();
                } else {
                    self.emit(Op::Null, None);
                };
                self.constants[jump_uncond_obj[0] as usize] =
                    Integer(self.instructions.len() as i32 - jump_uncond_ins as i32);
                /*
                 * Add three to the conditional offset,
                 * to get over the unconditional Jump we just added.
                 */
                cons_len += 3;

                self.constants[jump_cond_obj[0] as usize] = Integer(cons_len);
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
    /// Returns the instruction index of the added instruction.
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

    #[test]
    fn test_jump_instructions() {
        use code::make;
        let cases = [
            CompilerCase {
                input: "if (4 > 8) { 10 }; 666;",
                expected_constants: vec![
                    Integer(4),
                    Integer(8),
                    Integer(9), // JumpMaybe offset
                    Integer(10),
                    Integer(4), // Jump offset
                    Integer(666),
                ],
                expected_instructions: &[
                    // 0000 OpConstant 4
                    make(Constant, Some(&[0])),
                    // 0003 OpConstant 8
                    make(Constant, Some(&[1])),
                    // 0006 OpGT
                    make(GT, None),
                    // 0007 JumpMaybe 10
                    make(JumpMaybe, Some(&[2])),
                    // 0010 OpConstant 10
                    make(Constant, Some(&[3])),
                    // 0013 Jump 5
                    make(Jump, Some(&[4])),
                    // 0016 OpNull
                    make(Op::Null, None),
                    // 0017 OpPop
                    make(Pop, None),
                    // 0018 OpConstant
                    make(Constant, Some(&[5])),
                    // 0021 OpPop
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "if (false) { 10 }; 666;",
                expected_constants: vec![
                    Integer(9), // JumpMaybe offset
                    Integer(10),
                    Integer(4), // Jump offset
                    Integer(666),
                ],
                expected_instructions: &[
                    make(False, None),
                    make(JumpMaybe, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(Jump, Some(&[2])),
                    make(Op::Null, None),
                    make(Pop, None),
                    make(Constant, Some(&[3])),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "if (4 > 8) { 42 } else { 23 }; 666;",
                expected_constants: vec![
                    Integer(4),
                    Integer(8),
                    Integer(9), // JumpMaybe offset
                    Integer(42),
                    Integer(6), // Jump offset
                    Integer(23),
                    Integer(666),
                ],
                expected_instructions: &[
                    // 0000 OpConstant 4
                    make(Constant, Some(&[0])),
                    // 0003 OpConstant 8
                    make(Constant, Some(&[1])),
                    // 0006 OpGT
                    make(GT, None),
                    // 0007 JumpMaybe 10
                    make(JumpMaybe, Some(&[2])),
                    // 0010 OpConstant 42
                    make(Constant, Some(&[3])),
                    // 0014 OpJump 6
                    make(Jump, Some(&[4])),
                    // 0017 OpConstant 23
                    make(Constant, Some(&[5])),
                    // 0020 OpPop
                    make(Pop, None),
                    // 0021 OpConstant 666
                    make(Constant, Some(&[6])),
                    // 0024 OpPop
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "if (4 < 8) { 42; 4; 8; } else { 23 }; 666;",
                expected_constants: vec![
                    Integer(8),
                    Integer(4),
                    Integer(17), // JumpMaybe offset
                    Integer(42),
                    Integer(4),
                    Integer(8),
                    Integer(6), // Jump offset
                    Integer(23),
                    Integer(666),
                ],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(GT, None),
                    make(JumpMaybe, Some(&[2])),
                    make(Constant, Some(&[3])),
                    make(Pop, None),
                    make(Constant, Some(&[4])),
                    make(Pop, None),
                    make(Constant, Some(&[5])),
                    make(Jump, Some(&[6])),
                    make(Constant, Some(&[7])),
                    make(Pop, None),
                    make(Constant, Some(&[8])),
                    make(Pop, None),
                ],
            },
        ];

        test_compiler_cases(&cases);
    }

    #[test]
    fn test_let_statement() {
        use code::make;
        let cases = [
            CompilerCase {
                input: "let x = 1;",
                expected_constants: vec![Integer(1)],
                expected_instructions: &[make(Constant, Some(&[0])), make(SetGlobal, Some(&[0]))],
            },
            CompilerCase {
                input: "let x = 1; x;",
                expected_constants: vec![Integer(1)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(SetGlobal, Some(&[0])),
                    make(GetGlobal, Some(&[0])),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "let x = 1; let y = 2;",
                expected_constants: vec![Integer(1), Integer(2)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(SetGlobal, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(SetGlobal, Some(&[1])),
                ],
            },
            CompilerCase {
                input: "let x = 1; let y = 2; let z = x + y;",
                expected_constants: vec![Integer(1), Integer(2)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(SetGlobal, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(SetGlobal, Some(&[1])),
                    make(GetGlobal, Some(&[0])),
                    make(GetGlobal, Some(&[1])),
                    make(Add, None),
                    make(SetGlobal, Some(&[2])),
                ],
            },
        ];

        test_compiler_cases(&cases);
    }

    #[test]
    fn test_string_literals() {
        use code::make;
        let cases = [
            CompilerCase {
                input: r#""yoyo";"#,
                expected_constants: vec![String("yoyo".to_string())],
                expected_instructions: &[make(Constant, Some(&[0])), make(Pop, None)],
            },
            CompilerCase {
                input: r#""yo" + "yo";"#,
                expected_constants: vec![String("yo".to_string()), String("yo".to_string())],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(Add, None),
                    make(Pop, None),
                ],
            },
        ];

        test_compiler_cases(&cases);
    }

    #[test]
    fn array_compilation() {
        use code::make;
        let cases = [
            CompilerCase {
                input: "[]",
                expected_constants: vec![],
                expected_instructions: &[make(BuildArray, Some(&[0])), make(Pop, None)],
            },
            CompilerCase {
                input: "[1, 2, 3]",
                expected_constants: vec![Integer(1), Integer(2), Integer(3)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(Constant, Some(&[2])),
                    make(BuildArray, Some(&[3])),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "[1 + 10, 2 + 20, 3 + 30]",
                expected_constants: vec![
                    Integer(1),
                    Integer(10),
                    Integer(2),
                    Integer(20),
                    Integer(3),
                    Integer(30),
                ],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(Add, None),
                    make(Constant, Some(&[2])),
                    make(Constant, Some(&[3])),
                    make(Add, None),
                    make(Constant, Some(&[4])),
                    make(Constant, Some(&[5])),
                    make(Add, None),
                    make(BuildArray, Some(&[3])),
                    make(Pop, None),
                ],
            },
        ];

        test_compiler_cases(&cases);
    }

    #[test]
    fn hashmap_compilation() {
        use code::make;
        let cases = [
            CompilerCase {
                input: "{}",
                expected_constants: vec![],
                expected_instructions: &[make(BuildHashMap, Some(&[0])), make(Pop, None)],
            },
            CompilerCase {
                input: "{1: 10, 2: 20, 3: 30}",
                expected_constants: vec![
                    Integer(1),
                    Integer(10),
                    Integer(2),
                    Integer(20),
                    Integer(3),
                    Integer(30),
                ],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(Constant, Some(&[2])),
                    make(Constant, Some(&[3])),
                    make(Constant, Some(&[4])),
                    make(Constant, Some(&[5])),
                    make(BuildHashMap, Some(&[6])),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "{1 + 2: 10 + 1, 2 + 2: 20 + 1}",
                expected_constants: vec![
                    Integer(1),
                    Integer(2),
                    Integer(10),
                    Integer(1),
                    Integer(2),
                    Integer(2),
                    Integer(20),
                    Integer(1),
                ],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(Add, None),
                    make(Constant, Some(&[2])),
                    make(Constant, Some(&[3])),
                    make(Add, None),
                    make(Constant, Some(&[4])),
                    make(Constant, Some(&[5])),
                    make(Add, None),
                    make(Constant, Some(&[6])),
                    make(Constant, Some(&[7])),
                    make(Add, None),
                    make(BuildHashMap, Some(&[4])),
                    make(Pop, None),
                ],
            },
        ];

        test_compiler_cases(&cases);
    }

    #[test]
    fn indexing_compilation() {
        use code::make;
        let cases = [
            CompilerCase {
                input: "[][0]",
                expected_constants: vec![Integer(0)],
                expected_instructions: &[
                    make(BuildArray, Some(&[0])),
                    make(Constant, Some(&[0])),
                    make(Op::Index, None),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "[1,2][0]",
                expected_constants: vec![Integer(1), Integer(2), Integer(0)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(BuildArray, Some(&[2])),
                    make(Constant, Some(&[2])),
                    make(Op::Index, None),
                    make(Pop, None),
                ],
            },
            CompilerCase {
                input: "{1: 10}[1]",
                expected_constants: vec![Integer(1), Integer(10), Integer(1)],
                expected_instructions: &[
                    make(Constant, Some(&[0])),
                    make(Constant, Some(&[1])),
                    make(BuildHashMap, Some(&[2])),
                    make(Constant, Some(&[2])),
                    make(Op::Index, None),
                    make(Pop, None),
                ],
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
                String(s) => test_string_object(a, e),
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

    fn test_string_object(a: &Object, e: &Object) {
        assert!(matches!(e, Object::String(..)));
        if let Object::String(i) = a {
            if let Object::String(j) = e {
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
