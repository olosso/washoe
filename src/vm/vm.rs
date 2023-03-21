use std::fmt;
use std::mem::{self, MaybeUninit};
use std::ops::{Deref, DerefMut};

use crate::{
    code::{read_uint16, read_uint8, Instructions, Op},
    compiler::Bytecode,
    object::Object,
};

use super::globals::{Objects, GLOBALS_SIZE};
use super::stack::{Stack, STACK_SIZE};

/*
 * @VM::FRAME
 */
/// Struct representing a Stack Frame (AKA Call Frame or Activation Record)
#[derive(Debug)]
struct Frame {
    func: Object, // This is always a Compiled Function that is currently executing.
    ip: usize,    // The instruction pointer in the current frame.
    bp: usize,    // Base pointer for of current Frame
}

impl Frame {
    fn new(obj: Object, bp: usize) -> Self {
        assert!(matches!(obj, Object::CompiledFn(..)));
        Self {
            func: obj,
            ip: 0,
            bp,
        }
    }

    fn instructions(&self) -> &Instructions {
        // Thank God that Rust allows me to do this.
        if let Object::CompiledFn(ref instructions, _) = self.func {
            instructions
        } else {
            // I swear this should never happen.
            unreachable!()
        }
    }

    #[allow(non_snake_case)]
    pub const fn MAX_FRAMES() -> usize {
        2usize.pow(8)
    }
}

/*
 * @VM::VM
 */
#[derive(Debug)]
pub struct VM<'stack, 'objects> {
    constants: Vec<Object>,         // Precompiled constants
    globals: &'objects mut Objects, // All the global values.
    locals: &'objects mut Objects,  // All the local values.

    stack: &'stack mut Stack,
    sp: usize,

    frames: Vec<Frame>,  // A Stack of Frames
    frames_index: usize, // The index of the current Frame
    current_func: usize,
}

impl<'stack, 'objects> VM<'stack, 'objects> {
    pub fn new(
        bytecode: Bytecode,
        globals: &'objects mut Objects,
        locals: &'objects mut Objects,
        stack: &'stack mut Stack,
    ) -> Self {
        let main = Object::CompiledFn(bytecode.instructions, 0);
        let main_frame = Frame::new(main, 0);
        let mut frames = vec![main_frame];

        VM {
            constants: bytecode.constants,
            globals,
            locals,
            stack,
            sp: 0,

            frames,
            frames_index: 0,
            current_func: 0,
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

            // println!("INSTRUCTIONS:\n{}", self.current_instructions());
            // println!("STACK.PTR.: Current stack pointer {}", self.sp);
            // println!("STACK:\n{}", self.stack);

            // NOTE The instruction is relative to the current frame.
            // So each function starts with the ip set to 0.
            let mut ip = self.current_frame().ip;
            let op = Op::try_from(self.current_instructions()[ip]).unwrap();

            // The match statement that rules them all ☠
            match op {
                Op::Pop => {
                    self.pop();
                }
                Op::Null => {
                    self.push(Object::Null);
                }
                Op::True | Op::False => {
                    if op == Op::True {
                        self.push(Object::Boolean(true))
                    } else {
                        self.push(Object::Boolean(false))
                    }
                }
                Op::Constant => {
                    // Read the index of the constant from the operand for Op::Constant.
                    let const_index = read_uint16(self.current_instructions(), ip + 1);
                    ip += 2;

                    // Push the Object corresponding to the index onto the stack.
                    self.push(self.constants[const_index as usize].clone());
                }
                Op::SetLocal => {
                    let local_index = read_uint8(self.current_instructions(), ip + 1) as usize;
                    ip += 1;

                    /*
                     * bp = Where the current FunctionObject is on the stack.
                     * bp + 1 = Where the local variables start
                     * local_index = a unique index for each local variable
                     */
                    let stack_location = self.current_frame().bp + 1 + local_index;

                    // Update the local variables directly on the stack.
                    self.stack[stack_location] = self.pop().clone();
                }
                Op::GetLocal => {
                    // What index has the the compiler given to the object?
                    let local_index = read_uint8(self.current_instructions(), ip + 1) as usize;
                    ip += 1;

                    /*
                     * bp = Where the current FunctionObject is on the stack.
                     * bp + 1 = Where the local variables are located
                     * local_index = a unique index for each local variable
                     */
                    let stack_location = self.current_frame().bp + 1 + local_index;

                    // The Object requested.
                    let object = self.stack[stack_location].clone();

                    // Push the object to the working area of the Stack.
                    self.push(object);
                }
                Op::SetGlobal => {
                    let global_index = read_uint16(self.current_instructions(), ip + 1) as usize;
                    ip += 2;

                    self.globals[global_index] = self.pop().clone();
                }
                Op::GetGlobal => {
                    // What index has the the compiler given to the object?
                    let global_index = read_uint16(self.current_instructions(), ip + 1) as usize;
                    ip += 2;

                    // On the stack it goes.
                    let object = self.globals[global_index].clone();
                    // In case the object is a function, keep track of it.
                    if (matches!(object, Object::CompiledFn(..))) {
                        self.current_func = global_index;
                    };
                    self.push(object);
                }
                Op::GetBuiltin => {
                    /*
                     * 1. Resolve the right Builtin based on the operand.
                     * 2. Push Object::Builtin on the stack.
                     * 3. Op::Call takes care of the call.
                     */

                    let index = read_uint8(self.current_instructions(), ip + 1);
                    ip += 1;

                    // NOTE This unwrap should never fail, since the compiler was able to resolve the name.
                    let builtin = Object::get_builtin_by_index(index as u8).unwrap();
                    self.push(builtin);
                }
                /*
                 * Call will pop the Stack, assuming that the object there is a CompiledFunction.
                 * Then a new frame will be pushed on to the FrameStack, and instructions of that frame will be executed.
                 * The function should have a Return statement in it somewhere, in which case the frame will
                 * be popped of the FrameStack.
                 */
                Op::Call => {
                    // How many parameters does this function have?
                    let num_params = read_uint8(self.current_instructions(), ip + 1);

                    // Calculate the position of the FunctionObject on the stack.
                    let bp = self.sp - num_params as usize - 1;

                    // If the FunctionObject is a Builtin, then use the internal implementation.
                    // The rest of this Op::Call is unneccessary.
                    if matches!(self.stack[bp], Object::Builtin(..)) {
                        let arg = &self.stack[bp + 1];
                        let result = self.stack[bp].call(arg.clone());
                        self.pop(); // Get the parameter off the Stack.
                        self.pop(); // Get the builtin off the Stack.
                        self.push(result); // Chuch the result on the Stack.
                        self.update_ip(ip + 2); // Jump over the Op::Call.
                        continue; // Go to the next instruction.
                    }

                    // Get the FunctionObject
                    let func = self.stack[bp].clone();

                    // Get the number of local variables.
                    let local_count = match func {
                        Object::CompiledFn(_, count) => count,
                        _ => unreachable!(),
                    };

                    // Create a new Frame, which also stores the base pointer.
                    // The base pointer allows easy clean up of the stack when the function returns.
                    let frame = Frame::new(func.clone(), bp);

                    // Jump over the stack area of the locals.
                    // The area after the locals is used as the working area of the function.
                    self.sp = bp + local_count + num_params as usize + 1;

                    // Create a new Frame, which among other things, has its own instruction pointer.
                    self.push_frame(frame);

                    /*
                     * REVIEW Short-circuit to skip over the default increment of 1 after every instruction.
                     * Is this necessary?
                     */
                    continue;
                }
                /*
                 * Here the Frame is popped of the FrameStack.
                 */
                Op::Return | Op::Exit => {
                    // Get the Return value. The return value is Null for empty functions.
                    // For non-empty functions, it is the value of a ReturnStatement, or
                    // the last expression of the function.
                    let return_value = if let Op::Exit = op {
                        Object::Null
                    } else {
                        self.pop().clone()
                    };

                    // Adjust the stack pointer back to where it was when the function was called.
                    self.sp = self.current_frame().bp;

                    // Set the return value in the position of the original FunctionObject.
                    self.push(return_value);

                    // Get the current Frame off the Stack.
                    self.pop_frame();

                    /*
                     * The local variable ip inside this loop still has the ip value of the previous Frame,
                     * when the Frame is popped.
                     * For this reason, we need to update the value of the local variable ip back
                     * to where the previous Frame left off. The Frame remembers its own ip.
                     * The +1 is there to jump over the operand of the OpCall.
                     */
                    ip = self.current_frame().ip + 1;
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
