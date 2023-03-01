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
