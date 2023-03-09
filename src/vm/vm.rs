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
 * @VM::FRAME
 */
/// Struct representing a Stack Frame (AKA Call Frame or Activation Record)
#[derive(Debug)]
struct Frame {
    func: Object, // This is always a Compiled Function that is currently executing.
    ip: usize,    // The instruction pointer in the current frame.
}

impl Frame {
    fn new(obj: Object) -> Self {
        assert!(matches!(obj, Object::CompiledFn(..)));
        Self { func: obj, ip: 0 }
    }

    fn instructions(&self) -> &Instructions {
        // Thank God that Rust allows me to do this.
        if let Object::CompiledFn(ref instructions) = self.func {
            instructions
        } else {
            // I swear this should never happen.
            unreachable!()
        }
    }

    #[allow(non_snake_case)]
    pub const fn MAX_FRAMES() -> usize {
        2usize.pow(10)
    }
}

/*
 * @VM::VM
 */
#[derive(Debug)]
pub struct VM<'stack, 'globals> {
    constants: Vec<Object>,         // Precompiled constants
    globals: &'globals mut Globals, // All the global names.

    stack: &'stack mut Stack,
    sp: usize,

    frames: Vec<Frame>,  // A Stack of Frames
    frames_index: usize, // The index of the current Frame
}

impl<'stack, 'globals> VM<'stack, 'globals> {
    pub fn new(
        bytecode: Bytecode,
        globals: &'globals mut Globals,
        stack: &'stack mut Stack,
    ) -> Self {
        let main = Object::CompiledFn(bytecode.instructions);
        let main_frame = Frame::new(main);
        let mut frames = vec![main_frame];

        VM {
            constants: bytecode.constants,
            // instructions: bytecode.instructions,
            globals,
            stack,
            sp: 0,

            frames,
            frames_index: 0,
        }
    }

    fn current_frame(&self) -> &Frame {
        &self.frames[self.frames_index]
    }

    fn current_instructions(&self) -> &Instructions {
        self.frames[self.frames_index].instructions()
    }

    fn push_frame(&mut self, frame: Frame) {
        self.frames.push(frame);
        self.frames_index += 1;
    }

    fn pop_frame(&mut self) -> Frame {
        self.frames_index -= 1;
        self.frames.pop().unwrap()
    }

    fn update_ip(&mut self, ip: usize) {
        self.frames[self.frames_index].ip = ip;
    }

    /*
     * The VM executes the bytecode by getting instructions and constants from the compiler,
     * and defining behaviour for the Opcodes that it supports. At runtime depending on the Opcode,
     * data is pushed on the stack, or popped off the stack.
     *
     * If the stack size (in bytes) exceeds a predetermined limit, the VM will crash with a "stack overflow".
     */
    pub fn run(&mut self) {
        while self.current_frame().ip < self.current_instructions().len() {
            println!(
                "INST.PTR.: Current instruction pointer {}",
                self.current_frame().ip
            );
            println!("INSTRUCTIONS:\n{}", self.current_instructions());

            let mut ip = self.current_frame().ip;
            let op = Op::try_from(self.current_instructions()[ip]).unwrap();

            // The match statement that rules them all ☠
            match op {
                Op::Pop => {
                    self.pop();
                }
                Op::SetGlobal => {
                    let global_index = read_uint16(self.current_instructions(), ip + 1) as usize;
                    ip += 2;

                    self.globals[global_index] = self.pop().clone();
                }
                Op::GetGlobal => {
                    let global_index = read_uint16(self.current_instructions(), ip + 1) as usize;
                    ip += 2;

                    self.push(self.globals[global_index].clone());
                }
                Op::Null => {
                    self.push(Object::Null);
                }
                Op::Constant => {
                    // Read the index of the constant from the operand for Op::Constant.
                    let const_index = read_uint16(self.current_instructions(), ip + 1);
                    ip += 2;

                    // Push the Object corresponding to the index onto the stack.
                    self.push(self.constants[const_index as usize].clone());
                }
                Op::Call => {
                    // NOTE In the book the CompiledFunction isn't popped off the Stack. Why not?
                    // I guess I implemented a different "calling convention"?
                    let func = self.pop();
                    let frame = Frame::new(func.clone());
                    self.push_frame(frame);
                    /*
                     * REVIEW Short-circuit to skip over the default increment of 1 after every instruction.
                     * Is this necessary?
                     */
                    continue;
                }
                Op::Return | Op::Exit => {
                    // In case the Function is has no statements in it.
                    if let Op::Exit = op {
                        self.push(Object::Null);
                    };

                    // Get the current Frame of the Stack.
                    self.pop_frame();

                    /*
                     * The local variable ip inside this loop still has the ip value of the previous Frame,
                     * when the Frame is popped.
                     * For this reason, we need to update the value of the local variable ip back
                     * to where the previous Frame left off. The Frame remembers its own ip.
                     */
                    ip = self.current_frame().ip;
                }
                Op::True | Op::False => {
                    if op == Op::True {
                        self.push(Object::Boolean(true))
                    } else {
                        self.push(Object::Boolean(false))
                    }
                }
                Op::BuildArray => {
                    let array_size = read_uint16(self.current_instructions(), ip + 1) as usize;
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
                    let hashmap_size = read_uint16(self.current_instructions(), ip + 1) as usize;
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
                Op::Index => {
                    let index = self.pop().clone();
                    let container = self.pop().clone();

                    // REFACTOR Refactor this for the love of God. ✝
                    let val = match container {
                        Object::Array(a) => {
                            if let Object::Integer(i) = index {
                                if a.is_empty() || i < 0 || i as usize > (a.len() - 1) {
                                    Object::Null
                                } else {
                                    let i = i as usize;
                                    let val = a.get(i);
                                    if let Some(val) = val {
                                        val.clone()
                                    } else {
                                        Object::Null
                                    }
                                }
                            } else {
                                panic!(
                                    "Tried to index an array with something other than an Integer."
                                )
                            }
                        }
                        Object::HashMap(hm) => {
                            let val = hm.get(&index);
                            if let Some(val) = val {
                                val.clone()
                            } else {
                                Object::Null
                            }
                        }
                        // REVIEW Indexing causes is a runtime error, not a compile time error.
                        _ => panic!("Tried to index into something other than a Array or HashMap!"),
                    };

                    self.push(val);
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
                    let const_index = read_uint16(self.current_instructions(), ip + 1);
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
                            // REVIEW HACK Short-circuit to skip over the default increment of 1 after every instruction.
                            self.update_ip(ip);
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
                                // REVIEW HACK Short-circuit to skip over the default increment of 1 after every instruction.
                                self.update_ip(ip);
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
            self.update_ip(ip);
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
