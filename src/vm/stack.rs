use crate::object::Object;
use core::fmt;
use std::{
    mem::{self, MaybeUninit},
    ops::{Deref, DerefMut},
};

/*
 * @VM::STACK
 */
pub const STACK_SIZE: usize = 2usize.pow(10);
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

impl fmt::Display for Stack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        for (i, o) in self
            .0
            .iter()
            .take_while(|x| x != &&Object::Null)
            .enumerate()
        {
            s = s + format!("{i:0>3}").as_str() + " " + &o.inspect() + "\n";
        }
        write!(f, "{s}")
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
