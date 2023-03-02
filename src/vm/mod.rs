use std::mem::{self, MaybeUninit};
use std::ops::{Deref, DerefMut};

use crate::{
    code::{read_uint16, Instructions, Op},
    compiler::Bytecode,
    object::Object,
};

/*
 * @VM::STACK
 */
const STACK_SIZE: usize = 2usize.pow(10);
#[derive(Debug)]
pub struct Stack([Object; STACK_SIZE]);

impl Stack {
    pub fn new() -> Self {
        let nulls = {
            let mut data: [MaybeUninit<Object>; STACK_SIZE] =
                unsafe { MaybeUninit::uninit().assume_init() };

            // Dropping a `MaybeUninit` does nothing, so if there is a panic during this loop,
            // we have a memory leak, but there is no memory safety issue.
            for elem in &mut data[..] {
                elem.write(Object::Null);
            }

            // Everything is initialized. Transmute the array to the
            // initialized type.
            unsafe { mem::transmute::<_, [Object; STACK_SIZE]>(data) }
        };

        Stack(nulls)
    }
}

impl Deref for Stack {
    type Target = [Object];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Stack {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/*
 * @VM::VM
 */
#[derive(Debug)]
pub struct VM<'stack> {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: &'stack mut Stack,
    sp: usize,
}

impl<'stack> VM<'stack> {
    pub fn new(bytecode: Bytecode, stack: &'stack mut Stack) -> Self {
        VM {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack,
            sp: 0,
        }
    }

    /*
     * The VM executes the bytecode by getting instructions and constants from the compiler,
     * and defining behaviour for the Opcodes that it supports. At runtime depending on the Opcode,
     * data is pushed on the stack, or popped off the stack.
     *
     * If the stack size (in bytes) exceeds a predetermined limit, the VM will crash with a "stack overflow".
     */
    pub fn run(&mut self) {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op = Op::try_from(self.instructions[ip]).unwrap();

            match op {
                Op::Constant => {
                    // Read the index of the constant from the operand for Op::Constant.
                    let const_index = read_uint16(&self.instructions, ip + 1);
                    ip += 2;

                    // Push the Object corresponding to the index onto the stack.
                    self.push(self.constants[const_index as usize].clone());
                }
                Op::True | Op::False => {
                    if op == Op::True {
                        self.push(Object::Boolean(true))
                    } else {
                        self.push(Object::Boolean(false))
                    }
                }
                Op::Add | Op::Sub | Op::Mul | Op::Div => {
                    let right = self.pop().clone();
                    let left = self.pop().clone();

                    let operation = match op {
                        Op::Add => Object::add,
                        Op::Sub => Object::sub,
                        Op::Mul => Object::mul,
                        Op::Div => Object::div,
                        _ => unreachable!(),
                    };
                    self.push(operation(&left, &right));
                }
                Op::Equal | Op::NotEqual | Op::GT => {
                    let right = self.pop().clone();
                    let left = self.pop().clone();

                    let operation = match op {
                        Op::Equal => Object::eq,
                        Op::NotEqual => Object::eq,
                        Op::GT => Object::gt,
                        _ => unreachable!(),
                    };
                    let mut result = operation(&left, &right);
                    if op == Op::NotEqual {
                        result = !result;
                    }
                    self.push(Object::Boolean(result));
                }
                Op::Pop => {
                    self.pop();
                }
                _ => todo!(),
            }

            ip += 1;
        }
    }

    fn push(&mut self, obj: Object) {
        if self.sp >= STACK_SIZE {
            panic!("Stack overflow.")
        }

        self.stack[self.sp] = obj;
        self.sp += 1;
    }

    /// Objects aren't actually removed from the stack, the stack pointer is simply decremented.
    fn pop(&mut self) -> &Object {
        self.sp -= 1;
        &self.stack[self.sp]
    }

    pub fn last_popped_obj(&self) -> &Object {
        &self.stack[self.sp]
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

        pub fn test_boolean_object(a: &Object, e: &Object) {
            assert!(matches!(e, Object::Boolean(..)));
            if let Object::Boolean(i) = a {
                if let Object::Boolean(j) = e {
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
                let mut stack = Stack::new();
                let mut vm = VM::new(compiler.bytecode(), &mut stack);
                vm.run();

                // Confirmation
                let top = vm.last_popped_obj();
                test_expected_object(top, &case.expected);
            }
        }

        pub fn test_expected_object(actual: &Object, expected: &Object) {
            use Object::*;
            match actual {
                Integer(_) => test_integer_object(actual, expected),
                Boolean(_) => test_boolean_object(actual, expected),
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
            ];

            test_vm_run(&cases);
        }
    }
}
