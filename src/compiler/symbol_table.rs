use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

/*
 * @SYMBOL_TABLE::SCOPE
 */
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Scope {
    Global,
}

/*
 * @SYMBOL_TABLE::SYMBOL
 */
// Can't derive Copy, because String doesn't implement Copy
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub name: String, // REVIEW Maybe this should be an Rc?
    scope: Scope,     // REVIEW Maybe this should be an Rc?
    pub index: u32,
}

/*
 * @SYMBOL_TABLE::SYMBOL_TABLE
 */
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>, // REVIEW Maybe this should be an Rc HashMap<String, Rc<Symbol>>?
    num_definitions: u32,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol {
            name: name.to_string(),
            scope: Scope::Global,
            index: self.num_definitions,
        };
        self.store.insert(name.to_string(), symbol.clone());
        self.num_definitions += 1;
        symbol
    }

    pub fn resolve(&mut self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
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

        let mut global = SymbolTable::new();
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

        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        assert_eq!(global.resolve("a").unwrap(), expected.get("a").unwrap());
        assert_eq!(global.resolve("b").unwrap(), expected.get("b").unwrap());
    }
}
