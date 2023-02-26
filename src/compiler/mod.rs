use crate::ast;
use crate::ast::{Expression, Statement};
use crate::code::{self, Instructions, Opcode};
use crate::object::Object;

/*
 * @COMPILER::COMPILER
 */
#[derive(Default)]
struct Compiler {
    instructions: code::Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new(instructions: code::Instructions, constants: Vec<Object>) -> Self {
        Self {
            instructions,
            constants,
        }
    }

    pub fn compile(&mut self, program: ast::Program) {
        for statement in program.statements {
            self.c_statement(&statement);
        }
    }

    fn c_statement(&mut self, stmt: &Statement) {
        use ast::Statement::*;
        match stmt {
            Expr(_, e) => self.c_expression(e),
            _ => todo!(),
        }
    }

    fn c_expression(&mut self, expr: &Expression) {
        use crate::object::Object::*;
        use ast::Expression::*;
        match expr {
            IntegerLiteral(_, i) => {
                let i = Integer(*i);
                let pos = [self.add_constant(i) as u32];
                println!("{}", self.emit(Opcode::Constant, &pos));
            }
            Infix(_, l, op, r) => {
                self.c_expression(l);
                self.c_expression(r);
            }
            _ => todo!(),
        }
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn emit(&mut self, op: code::Opcode, operands: &[u32]) -> usize {
        let mut ins = code::make(op, operands);
        self.add_instruction(&mut ins)
    }

    fn add_instruction(&mut self, ins: &mut Instructions) -> usize {
        let pos_new_ins = self.instructions.len();
        self.instructions.append(ins);
        pos_new_ins
    }

    // NOTE this consumes the Compiler
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
struct Bytecode {
    instructions: code::Instructions,
    constants: Vec<Object>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::code::{Instructions, Opcode::*};
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
        let cases = [CompilerCase {
            input: "1 + 2",
            expected_constants: vec![Integer(1), Integer(2)],
            expected_instructions: &[code::make(Constant, &[0]), code::make(Constant, &[1])],
        }];

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
        assert_eq!(actual.len(), expected.len());
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
                assert_eq!(i, j);
            }
        }
    }

    fn test_instructions(actual: code::Instructions, expected: &[code::Instructions]) {
        let concat = concat_instructions(expected);

        assert_eq!(actual.len(), concat.len());
        assert!(actual.iter().eq(concat.iter()));
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
