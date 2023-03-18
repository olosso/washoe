use crate::object::Object;
use core::fmt;
use std::{
    mem::{self, MaybeUninit},
    ops::{Deref, DerefMut},
};

/*
 * @VM::GLOBALS
 */
// REVIEW This was originally 2**16, but that resulted in a Rust throwing a stack overflow!
pub const GLOBALS_SIZE: usize = 2usize.pow(8);
#[derive(Debug)]
pub struct Objects([Object; GLOBALS_SIZE]);

impl Objects {
    pub fn new() -> Self {
        let nulls = {
            let mut data: [MaybeUninit<Object>; GLOBALS_SIZE] =
                unsafe { MaybeUninit::uninit().assume_init() };

            // Dropping a `MaybeUninit` does nothing, so if there is a panic during this loop,
            // we have a memory leak, but there is no memory safety issue.
            for elem in &mut data[..] {
                elem.write(Object::Null);
            }

            // Everything is initialized. Transmute the array to the
            // initialized type.
            unsafe { mem::transmute::<_, [Object; GLOBALS_SIZE]>(data) }
        };

        Objects(nulls)
    }
}

impl fmt::Display for Objects {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        for o in self.0.iter().take_while(|x| x != &&Object::Null) {
            s = s + &o.inspect() + "\n";
        }
        write!(f, "{s}")
    }
}

impl Deref for Objects {
    type Target = [Object];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Objects {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
