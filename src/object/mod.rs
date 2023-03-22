#![allow(non_camel_case_types)]
use crate::ast::{Body, Expression, Node, Params, Statement};
use crate::code::Instructions;
use crate::evaluator::EvalError;
use core::cmp::PartialEq;
use std::default::Default;
use std::hash::{Hash, Hasher};
use std::{collections::HashMap, ops::Deref};

/*
 * @TYPE
 */
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Type {
    NULL,
    INTEGER,
    STRING,
    BOOLEAN,
    ARRAY,
    HASHMAP,
    RETURN,
    FUNCTION,
    BUILTIN,
    COMPILEDFN,
    CLOSURE,
}

/*
 * @OBJECT
 */
#[derive(Debug)]
pub enum Object {
    Null,
    Integer(i32),
    String(String),
    Boolean(bool),
    Array(Vec<Self>),
    Return(Box<Self>),
    HashMap(HashMap<Self, Self>),
    Function(Vec<Expression>, Statement, Environment),
    Builtin(&'static str, fn(Self) -> Result<Self, EvalError>),
    CompiledFn(crate::code::Instructions, usize),
    Closure(Box<Self>, Vec<Self>),
}

impl Default for Object {
    fn default() -> Self {
        Object::Null
    }
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Integer(i) => {
                Type::INTEGER.hash(state);
                i.hash(state);
            }
            Self::Boolean(b) => {
                Type::BOOLEAN.hash(state);
                b.hash(state);
            }
            Self::String(s) => {
                Type::STRING.hash(state);
                s.hash(state);
            }
            _ => unreachable!("Only Ints, Bools and Strings should appear as keys."),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Object::Null => {
                if let Object::Null = other {
                    true
                } else {
                    false
                }
            }
            Object::Integer(a) => {
                if let Object::Integer(b) = other {
                    a == b
                } else {
                    false
                }
            }
            Object::String(a) => {
                if let Object::String(b) = other {
                    a == b
                } else {
                    false
                }
            }
            Object::Boolean(a) => {
                if let Object::Boolean(b) = other {
                    a == b
                } else {
                    false
                }
            }
            Object::Return(a) => {
                if let Object::Return(b) = other {
                    **a == **b
                } else {
                    false
                }
            }
            Object::Function(f1, _, _) => {
                // TODO
                // if let Object::Function(f2, _, _) = other {
                //     f1.token_literal() == f2.token_literal()
                // } else {
                //     false
                // }
                false
            }
            Object::Builtin(a, _) => {
                if let Object::Builtin(b, _) = other {
                    a == b
                } else {
                    false
                }
            }
            Object::Array(a) => {
                if let Object::Array(b) = other {
                    a.iter().zip(b).all(|(a_, b_)| a_ == b_)
                } else {
                    false
                }
            }
            Object::HashMap(a) => {
                if let Object::HashMap(b) = other {
                    a.iter().zip(b).all(|(a_, b_)| a_ == b_)
                } else {
                    false
                }
            }
            Object::CompiledFn(ins_a, count_a) => {
                if let Object::CompiledFn(ins_b, count_b) = other {
                    ins_a == ins_b && count_a == count_b
                } else {
                    false
                }
            }
            Object::Closure(func, free_vars) => {
                todo!()
            }
        }
    }
}

impl Clone for Object {
    fn clone(&self) -> Object {
        match self {
            Self::Null => Self::Null,
            Self::Integer(i) => Self::Integer(*i),
            Self::String(s) => Self::String(s.to_owned()),
            Self::Boolean(b) => Self::Boolean(*b),
            Self::Return(obj) => Self::Null, // TODO This is a quick fix. Not sure what to do.
            Self::Function(params, body, env) => {
                Self::Function(params.clone(), body.clone(), env.clone())
            }
            Self::Builtin(s, f) => panic!("Not allowed to copy Builtins"),
            Self::Array(a) => {
                let mut b = vec![];
                for elem in a {
                    b.push(elem.clone())
                }
                Self::Array(b)
            }
            Self::HashMap(a) => {
                let mut b = HashMap::new();
                for (k, v) in a {
                    b.insert(k.clone(), v.clone());
                }
                Self::HashMap(b)
            }
            Self::CompiledFn(ins, count) => Self::CompiledFn(ins.clone(), *count),
            Self::Closure(ins, free_vars) => Self::Closure(ins.clone(), free_vars.clone()),
        }
    }
}

impl Eq for Object {}

impl Object {
    pub fn obtype(&self) -> Type {
        match self {
            Object::Null => Type::NULL,
            Object::Integer(_) => Type::INTEGER,
            Object::String(_) => Type::STRING,
            Object::Boolean(_) => Type::BOOLEAN,
            Object::Array(_) => Type::ARRAY,
            Object::HashMap(_) => Type::HASHMAP,
            Object::Return(_) => Type::RETURN,
            Object::Function(..) => Type::FUNCTION,
            Object::Builtin(..) => Type::BUILTIN,
            Object::CompiledFn(..) => Type::COMPILEDFN,
            Object::Closure(..) => Type::CLOSURE,
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Null => "null".to_string(),
            Object::Integer(i) => i.to_string(),
            Object::String(s) => format!("\"{}\"", s.to_owned()),
            Object::Boolean(b) => b.to_string(),
            Object::Array(a) => {
                format!(
                    "[{}]",
                    a.iter()
                        .map(|x| x.inspect())
                        .collect::<Vec<String>>()
                        .join(", "),
                )
            }
            Object::Return(v) => v.inspect(),
            Object::Function(params, body, env) => {
                format!(
                    "func({}) {{ {} }}",
                    params
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    body.to_string(),
                )
            }
            Object::Builtin(s, f) => s.to_string(),
            Object::HashMap(m) => {
                let mut sv = vec![];
                for (k, v) in m {
                    sv.push(format!("{}: {}", k.inspect(), v.inspect()))
                }
                let s = sv.join(", ");
                format!("{{ {s} }}")
            }
            Object::CompiledFn(ins, count) => {
                format!("{ins}")
            }
            Object::Closure(func, free_vars) => {
                format!("{}\n{:?}", func.inspect(), free_vars)
            }
        }
    }

    fn unwrap_ints(left: &Object, right: &Object) -> Option<(i32, i32)> {
        if let Object::Integer(a) = left {
            if let Object::Integer(b) = right {
                Some((*a, *b))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn unwrap_strings(left: &Object, right: &Object) -> Option<(String, String)> {
        if let Object::String(a) = left {
            if let Object::String(b) = right {
                Some((a.clone(), b.clone()))
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn add(left: &Object, right: &Object) -> Object {
        let objs = Self::unwrap_ints(left, right);
        if objs.is_none() {
            let (l, r) = Self::unwrap_strings(left, right)
                .expect("+ operator only supported for Integers and Strings.");
            Object::String(l + &r)
        } else {
            let (l, r) = Self::unwrap_ints(left, right).unwrap();
            Object::Integer(l + r)
        }
    }

    pub fn sub(left: &Object, right: &Object) -> Object {
        let (l, r) = Self::unwrap_ints(left, right)
            .expect("Non-numeric operands to given to numeric operation.");

        Object::Integer(l - r)
    }
    pub fn mul(left: &Object, right: &Object) -> Object {
        let (l, r) = Self::unwrap_ints(left, right)
            .expect("Non-numeric operands to given to numeric operation.");

        Object::Integer(l * r)
    }
    pub fn div(left: &Object, right: &Object) -> Object {
        let (l, r) = Self::unwrap_ints(left, right)
            .expect("Non-numeric operands to given to numeric operation.");

        Object::Integer(l / r)
    }
    pub fn gt(left: &Object, right: &Object) -> bool {
        let (l, r) = Self::unwrap_ints(left, right)
            .expect("Non-numeric operands to given to numeric operation.");

        l > r
    }
    pub fn minus(right: &Object) -> Object {
        if let Self::Integer(i) = right {
            Self::Integer(-i)
        } else {
            panic!("Non-numeric operand given to minus.")
        }
    }
    pub fn bang(right: &Object) -> Object {
        Self::Boolean(!right.as_bool())
    }

    /*
     * @FUNCTION_HELPERS
     */
    pub fn body(&self) -> Option<String> {
        match self {
            Object::Function(_, b, _) => Some(format!("{{ {} }}", b.to_string())),
            _ => None,
        }
    }

    pub fn params(&self) -> Option<String> {
        match self {
            Object::Function(p, _, _) => Some(format!(
                "({})",
                p.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
            )),
            _ => None,
        }
    }

    pub fn env(&self) -> Option<String> {
        match self {
            Object::Function(_, _, e) => Some(e.inspect()),
            _ => None,
        }
    }

    pub fn locals_count(&self) -> Option<usize> {
        match self {
            Object::CompiledFn(_, count) => Some(*count),
            _ => None,
        }
    }

    pub fn instructions(&self) -> Option<&Instructions> {
        match self {
            Object::Closure(compiled_fn, _) => Some(compiled_fn._instructions().unwrap()),
            _ => None,
        }
    }

    pub fn _instructions(&self) -> Option<&Instructions> {
        match self {
            Object::CompiledFn(instructions, _) => Some(instructions),
            _ => None,
        }
    }

    /*
     * @NULL_HELPERS
     */
    pub fn is_null(&self) -> bool {
        *self == Object::Null
    }

    /*
     * Integers
     */
    pub fn as_int(&self) -> Option<i32> {
        match self {
            Object::Integer(i) => Some(*i),
            _ => None,
        }
    }

    /// Every value can be evaluated as a bool.
    ///
    /// These values evaluate to false:
    /// - Boolean(false)
    /// - Integer(0)
    /// - None
    ///
    /// Everything else is true.
    ///
    pub fn as_bool(&self) -> bool {
        !matches!(
            self,
            Object::Null | Object::Integer(0) | Object::Boolean(false)
        )
    }

    /*
     * @BUILTIN_HELPERS
     */
    pub fn get_builtin_by_index(index: u8) -> Option<Object> {
        match index {
            0 => Self::get_builtin_by_name("len"),
            1 => Self::get_builtin_by_name("puts"),
            _ => None,
        }
    }

    pub fn get_builtin_by_name(name: &str) -> Option<Object> {
        match name {
            "len" => Some(Object::Builtin(Builtins::LEN, Builtins::len)),
            "puts" => Some(Object::Builtin(Builtins::PUTS, Builtins::puts)),
            _ => None,
        }
    }

    pub fn call(&self, arg: Self) -> Object {
        match self {
            Object::Builtin(_, f) => {
                let res = f(arg);
                match res {
                    Ok(res) => res,
                    Err(err) => panic!("{err}"),
                }
            }
            _ => panic!(),
        }
    }
}

pub struct Builtins;
impl Builtins {
    const LEN: &'static str = "len";
    const PUTS: &'static str = "puts";

    pub fn len(object: Object) -> Result<Object, EvalError> {
        dbg!(&object);
        let len = match object {
            Object::Array(ref array) => array.len(),
            Object::String(ref string) => string.len(),
            _ => {
                return Err(EvalError::new(
                    "len only supports Arrays and Strings.".to_string(),
                ))
            }
        };

        Ok(Object::Integer(len as i32))
    }

    pub fn puts(object: Object) -> Result<Object, EvalError> {
        println!("{}", object.inspect());
        Ok(Object::Null)
    }

    pub fn names() -> Vec<&'static str> {
        vec![Builtins::LEN, Builtins::PUTS]
    }
}

use std::cell::RefCell;
use std::rc::Rc;
#[derive(Debug, Clone)]
pub struct Environment(
    Rc<RefCell<HashMap<String, Object>>>,
    Option<Rc<Environment>>,
);

impl Environment {
    pub fn global() -> Self {
        Environment(Rc::new(RefCell::new(HashMap::new())), None)
    }

    pub fn local(other: &Rc<Self>) -> Self {
        Environment(
            Rc::new(RefCell::new(HashMap::new())),
            Some(Rc::clone(other)),
        )
    }

    pub fn inspect(&self) -> String {
        let mut msg = String::new();
        for (name, obj) in self.0.borrow().iter() {
            msg.push_str(
                format!("{}: {}\n", name.as_str(), obj.inspect())
                    .to_owned()
                    .as_str(),
            )
        }
        msg
    }

    pub fn add(&self, name: &str, obj: &Object) {
        self.0.borrow_mut().insert(name.to_string(), obj.clone());
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        let binding = self.0.borrow();
        let obj = binding.get(name);
        match obj {
            Some(x) => Some(x.clone()),
            None => match &self.1 {
                Some(env) => env.get(name),
                None => None,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn size_of_obj() {
        // Checked this out of curiosity.
        assert_eq!(std::mem::size_of::<Object>(), 248);
    }
}
