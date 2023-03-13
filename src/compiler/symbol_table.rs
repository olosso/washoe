use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

/*
 * @SYMBOL_TABLE::SCOPE
 */
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scope {
    Global,
    Local,
}

/*
 * @SYMBOL_TABLE::SYMBOL
 */
// Can't derive Copy, because String doesn't implement Copy
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
    scope: Scope,
    pub index: u32,
}

/*
 * @SYMBOL_TABLE::SYMBOL_TABLE
 */
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: u32,
    pub parent: Option<Rc<RefCell<Self>>>,
}

impl SymbolTable {
    pub fn new(parent: Option<Rc<RefCell<Self>>>) -> Self {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
            parent,
        }
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol {
            name: name.to_string(),
            scope: if self.parent.is_none() {
                Scope::Global
            } else {
                Scope::Local
            },
            index: self.num_definitions,
        };
        self.store.insert(name.to_string(), symbol.clone());
        self.num_definitions += 1;
        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        match self.store.get(name) {
            None => match self.parent {
                None => None,
                Some(ref parent) => parent.borrow().store.get(name).cloned(),
            },
            symbol => symbol.cloned(),
        }
    }

    pub fn p(&self) -> Option<&Self> {
        match self.parent {
            None => None,
            Some(p) => p.borrow(),
        }
    }
}

impl Deref for SymbolTable {
    type Target = HashMap<String, Symbol>;
    fn deref(&self) -> &Self::Target {
        &self.store
    }
}

impl DerefMut for SymbolTable {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.store
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define() {
        let expected: HashMap<String, Symbol> = HashMap::from([
            (
                "a".to_string(),
                Symbol {
                    name: "a".to_string(),
                    scope: Scope::Global,
                    index: 0,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    name: "b".to_string(),
                    scope: Scope::Global,
                    index: 1,
                },
            ),
        ]);

        let mut global = SymbolTable::new(None);
        global.define("a");
        global.define("b");

        assert_eq!(global.store, expected)
    }

    #[test]
    fn test_resolve() {
        let expected: HashMap<String, Symbol> = HashMap::from([
            (
                "a".to_string(),
                Symbol {
                    name: "a".to_string(),
                    scope: Scope::Global,
                    index: 0,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    name: "b".to_string(),
                    scope: Scope::Global,
                    index: 1,
                },
            ),
        ]);

        let mut global = SymbolTable::new(None);
        global.define("a");
        global.define("b");

        assert_eq!(global.resolve("a").unwrap(), *expected.get("a").unwrap());
        assert_eq!(global.resolve("b").unwrap(), *expected.get("b").unwrap());
    }

    #[test]
    fn local_bindings() {
        let expected: HashMap<String, Symbol> = HashMap::from([
            (
                "a".to_string(),
                Symbol {
                    name: "a".to_string(),
                    scope: Scope::Global,
                    index: 0,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    name: "b".to_string(),
                    scope: Scope::Global,
                    index: 1,
                },
            ),
            (
                "c".to_string(),
                Symbol {
                    name: "c".to_string(),
                    scope: Scope::Local,
                    index: 0,
                },
            ),
            (
                "d".to_string(),
                Symbol {
                    name: "d".to_string(),
                    scope: Scope::Local,
                    index: 1,
                },
            ),
        ]);

        let mut global = Rc::new(RefCell::new(SymbolTable::new(None)));
        global.borrow_mut().define("a");
        global.borrow_mut().define("b");

        let mut local = Rc::new(RefCell::new(SymbolTable::new(Some(Rc::clone(&global)))));
        local.borrow_mut().define("c");
        local.borrow_mut().define("d");

        for (key, val) in global.borrow().iter() {
            assert_eq!(
                global.borrow().resolve(key).unwrap(),
                *expected.get(key).unwrap()
            );
        }

        for (actual_key, actual_val) in local.borrow().iter() {
            assert_eq!(
                local.borrow().resolve(actual_key).unwrap(),
                *expected.get(actual_key).unwrap()
            );
        }
    }

    #[test]
    fn local_nested_bindings() {
        let expected: HashMap<String, Symbol> = HashMap::from([
            (
                "a".to_string(),
                Symbol {
                    name: "a".to_string(),
                    scope: Scope::Global,
                    index: 0,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    name: "b".to_string(),
                    scope: Scope::Global,
                    index: 1,
                },
            ),
            (
                "c".to_string(),
                Symbol {
                    name: "c".to_string(),
                    scope: Scope::Local,
                    index: 0,
                },
            ),
            (
                "d".to_string(),
                Symbol {
                    name: "d".to_string(),
                    scope: Scope::Local,
                    index: 1,
                },
            ),
        ]);

        let expected_nested: HashMap<String, Symbol> = HashMap::from([
            (
                "a".to_string(),
                Symbol {
                    name: "a".to_string(),
                    scope: Scope::Global,
                    index: 0,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    name: "b".to_string(),
                    scope: Scope::Global,
                    index: 1,
                },
            ),
            (
                "e".to_string(),
                Symbol {
                    name: "e".to_string(),
                    scope: Scope::Local,
                    index: 0,
                },
            ),
            (
                "f".to_string(),
                Symbol {
                    name: "f".to_string(),
                    scope: Scope::Local,
                    index: 1,
                },
            ),
        ]);

        let mut global = Rc::new(RefCell::new(SymbolTable::new(None)));
        global.borrow_mut().define("a");
        global.borrow_mut().define("b");

        let mut local = Rc::new(RefCell::new(SymbolTable::new(Some(Rc::clone(&global)))));
        local.borrow_mut().define("c");
        local.borrow_mut().define("d");

        let mut nested_local = Rc::new(RefCell::new(SymbolTable::new(Some(Rc::clone(&local)))));
        nested_local.borrow_mut().define("e");
        nested_local.borrow_mut().define("f");

        for (key, val) in local.borrow().iter() {
            assert_eq!(
                local.borrow().resolve(key).unwrap(),
                *expected.get(key).unwrap()
            );
        }

        for (key, val) in nested_local.borrow().iter() {
            assert_eq!(
                nested_local.borrow().resolve(key).unwrap(),
                *expected_nested.get(key).unwrap()
            );
        }
    }
}
