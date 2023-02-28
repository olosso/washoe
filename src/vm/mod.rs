use crate::{
    code::{read_uint16, Instructions, Op},
    compiler::Bytecode,
    object::Object,
};

/*
 * @VM::VM
 */
#[derive(Debug)]
pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Vec<Object>,
    sp: usize,
}

const STACK_SIZE: usize = 2usize.pow(10);

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        VM {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::with_capacity(STACK_SIZE),
            sp: 0,
        }
    }

    /*
     * The VM executes the bytecode by getting instructions and constants from the compiler,
     * and defining behaviour for the Opcodes that it supports. At runtime depending on the Opcode,
     * data is pushed on the stack, and popped off the stack.
     *
     * If the stack size (in bytes) exceeds a predetermined limit, the VM will crash with a "stack overflow".
     */
    pub fn run(&mut self) {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op = Op::try_from(self.instructions[ip]).unwrap();

            match op {
                Op::Constant => {
                    let const_index = read_uint16(&self.instructions, ip + 1);
                    ip += 2;
                    self.push(self.constants[const_index as usize].clone());
                }
                Op::Add => {
                    let b = self.pop();
                    let a = self.pop();

                    self.push(Object::add(a, b).unwrap());
                }
                _ => todo!(),
            }

            ip += 1;
        }
    }

    fn push(&mut self, o: Object) {
        if self.sp >= STACK_SIZE {
            panic!("Stack overflow.")
        }

        self.stack.push(o);
        self.sp += 1;
    }

    fn pop(&mut self) -> Object {
        let o = self.stack.pop().unwrap();
        self.sp -= 1;
        o
    }

    pub fn stack_top(&self) -> Option<&Object> {
        if self.sp == 0 {
            None
        } else {
            Some(&self.stack[self.sp - 1])
        }
    }
}

#[cfg(test)]
mod vm_tests {
    use super::*;
    use crate::compiler::Compiler;
    use crate::object::Object;
    use crate::object::Object::*;

    mod helpers {
        use super::*;

        pub fn parse(input: &str) -> crate::ast::Program {
            let lexer = crate::lexer::Lexer::new(input.to_string());
            let mut parser = crate::parser::Parser::new(lexer);
            parser.parse_program().unwrap()
        }

        pub fn test_integer_object(a: &Object, e: &Object) {
            assert!(matches!(e, Object::Integer(..)));
            if let Object::Integer(i) = a {
                if let Object::Integer(j) = e {
                    assert_eq!(i, j);
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
                let mut vm = VM::new(compiler.bytecode());
                vm.run();

                // Confirmation
                let top = vm.stack_top().unwrap();
                test_expected_object(top, &case.expected);
            }
        }

        pub fn test_expected_object(actual: &Object, expected: &Object) {
            use Object::*;
            match actual {
                Integer(i) => test_integer_object(actual, expected),
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
                    input: "2",
                    expected: Integer(2),
                },
                VMCase {
                    input: "7 + 8",
                    expected: Integer(15),
                },
                VMCase {
                    input: "7 + 8 + 3",
                    expected: Integer(18),
                },
            ];

            test_vm_run(&cases);
        }
    }
}
