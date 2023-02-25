use crate::ast::{Body, Expression, Node, Params, Statement};
use crate::evaluator::EvalError;
use core::cmp::PartialEq;
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
    Builtin(&'static str, fn(Vec<Self>) -> Result<Self, EvalError>),
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
                    // env.to_str()
                )
            }
            Object::Builtin(s, f) => s.to_string(),
            Object::HashMap(m) => {
                let mut sv = vec![];
                for (k, v) in m {
                    sv.push(format!("{}: {}", k.inspect(), v.inspect()))
                }
                let s = sv.join(", ");
                format!("{{ {} }}", s)
            }
        }
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

    /*
     * Booleans in this language are truthy: Every value can be coerced into
     * a bool, and values are false:
     * Boolean(false)
     * None
     * Integer(0)
     *
     * Everything else is true.
     */
    pub fn as_bool(&self) -> bool {
        match &self {
            Object::Boolean(b) => *b,
            Object::Null | Object::Integer(0) => false,
            _ => true,
        }
    }

    pub fn copy(&self) -> Object {
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
                    b.push(elem.copy())
                }
                Self::Array(b)
            }
            Self::HashMap(a) => {
                let mut b = HashMap::new();
                for (k, v) in a {
                    b.insert(k.copy(), v.copy());
                }
                Self::HashMap(b)
            }
        }
    }

    pub fn builtins(name: &str) -> Option<Object> {
        match name {
            "len" => Some(Object::Builtin(Builtins::LEN, Builtins::len)),
            "puts" => Some(Object::Builtin(Builtins::LEN, Builtins::puts)),
            _ => None,
        }
    }
}

pub struct Builtins;
impl Builtins {
    const LEN: &str = "len";
    const PUTS: &str = "puts";

    pub fn len(os: Vec<Object>) -> Result<Object, EvalError> {
        if !(os.len() == 1 && matches!(os[0], Object::String(..))) {
            return Err(EvalError::new("Bad arguments to len".to_string()));
        };
        let o = &os[0];

        if let Object::String(s) = o {
            Ok(Object::Integer(s.len() as i32))
        } else {
            panic!("This shouldn't happen.")
        }
    }

    pub fn puts(os: Vec<Object>) -> Result<Object, EvalError> {
        for o in os {
            println!("{}", o.inspect())
        }
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
        self.0.borrow_mut().insert(name.to_string(), obj.copy());
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        let binding = self.0.borrow();
        let obj = binding.get(name);
        match obj {
            Some(x) => Some(x.copy()),
            None => match &self.1 {
                Some(env) => env.get(name),
                None => None,
            },
        }
    }
}
