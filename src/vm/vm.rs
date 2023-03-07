use std::fmt;
use std::mem::{self, MaybeUninit};
use std::ops::{Deref, DerefMut};

use crate::{
    code::{read_uint16, Instructions, Op},
    compiler::Bytecode,
    object::Object,
};

use super::globals::{Globals, GLOBALS_SIZE};
use super::stack::{Stack, STACK_SIZE};

/*
 * @VM::VM
 */
#[derive(Debug)]
pub struct VM<'stack, 'globals> {
    constants: Vec<Object>,
    instructions: Instructions,
    globals: &'globals mut Globals,
    stack: &'stack mut Stack,
    sp: usize,
}

impl<'stack, 'globals> VM<'stack, 'globals> {
    pub fn new(
        bytecode: Bytecode,
        globals: &'globals mut Globals,
        stack: &'stack mut Stack,
    ) -> Self {
        VM {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            globals,
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
            println!("TOPLEVEL: Current instruction pointer {ip}");
            println!("INSTRUCTIONS:\n{}", self.instructions);

            let op = Op::try_from(self.instructions[ip]).unwrap();

            match op {
                Op::Pop => {
                    self.pop();
                }
                Op::SetGlobal => {
                    let global_index = read_uint16(&self.instructions, ip + 1) as usize;
                    ip += 2;

                    self.globals[global_index] = self.pop().clone();
                }
                Op::GetGlobal => {
                    let global_index = read_uint16(&self.instructions, ip + 1) as usize;
                    ip += 2;

                    self.push(self.globals[global_index].clone());
                }
                Op::Null => {
                    self.push(Object::Null);
                }
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
                Op::BuildArray => {
                    let array_size = read_uint16(&self.instructions, ip + 1) as usize;
                    ip += 2;

                    /*
                     * To create an Array, the stack is read from top to bottom (or bottom to top, since
                     * the stack grows upwards). Anyway, the point is that the elements aren't simply
                     * popped of the stack, since then the array elements would be in reverse order.
                     */
                    let mut array_objs = vec![];
                    for i in (self.sp - array_size)..self.sp {
                        array_objs.push(self.stack[i].clone());
                    }
                    // NOTE The stack pointer is adjusted since n elements were "popped" of the stack.
                    self.sp -= array_size;

                    let arr = Object::Array(array_objs);
                    self.push(arr);
                }
                Op::BuildHashMap => {
                    let hashmap_size = read_uint16(&self.instructions, ip + 1) as usize;
                    ip += 2;

                    let mut hm = std::collections::HashMap::new();
                    for i in ((self.sp - hashmap_size)..self.sp).step_by(2) {
                        let key = self.stack[i].clone();
                        let val = self.stack[i + 1].clone();
                        hm.insert(key, val);
                    }
                    // NOTE The stack pointer is adjusted since n elements were "popped" of the stack.
                    self.sp -= hashmap_size;

                    let hm = Object::HashMap(hm);
                    self.push(hm);
                }
                Op::Minus | Op::Bang => {
                    let right = self.pop();

                    let operation = match op {
                        Op::Minus => Object::minus,
                        Op::Bang => Object::bang,
                        _ => unreachable!(),
                    };

                    let result = operation(right);
                    self.push(result);
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
                Op::Jump | Op::JumpMaybe => {
                    // Get the operand of the Jump instruction.
                    let const_index = read_uint16(&self.instructions, ip + 1);
                    let next_ins = if let Some(Object::Integer(i)) =
                        self.constants.get(const_index as usize)
                    {
                        *i as usize
                    } else {
                        panic!("Failed to fetch operand of OpJump.")
                    };

                    match op {
                        Op::Jump => {
                            ip += next_ins;
                            // Short-circuit to skip over the default increment of 1 after every instruction.
                            continue;
                        }
                        Op::JumpMaybe => {
                            /*
                             * Interpret top of the Stack Object as a Boolean.
                             * NOTE This can never fail, since everything on the Stack
                             * is an Object, and every object can be interpreted as a Boolean.
                             */
                            let cond = self.pop().as_bool();
                            if cond {
                                // Need to jump over the operand bytes, if the condition evaluated to true.
                                ip += 2;
                            } else {
                                // Increment ip, if the condition evaluated to false.
                                ip += next_ins;
                                // Short-circuit to skip over the default increment of 1 after every instruction.
                                continue;
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                _ => todo!(),
            }

            // NOTE I'm here! Don't forget about me!
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
