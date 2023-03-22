use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

/*
 * @SYMBOL_TABLE::SCOPE
 */
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Global;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Builtin;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Local;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Free;

/*
 * @SYMBOL_TABLE::SYMBOL
 */
// Can't derive Copy, because String doesn't implement Copy
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
    pub index: u32,
    pub global: bool,
    pub builtin: bool,
    pub free: bool,
}

/*
 * @SYMBOL_TABLE::SYMBOL_TABLE
 */
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolTable<Scope = Global> {
    store: HashMap<String, Symbol>,
    pub num_definitions: u32,
    pub num_params: u32,
    scope: PhantomData<Scope>,
}

impl SymbolTable<Global> {
    pub fn global() -> Self {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
            num_params: 0,
            scope: PhantomData,
        }
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol {
            name: name.to_string(),
            index: self.num_definitions,
            global: true,
            builtin: false,
            free: false,
        };
        self.store.insert(name.to_string(), symbol.clone());
        self.num_definitions += 1;
        symbol
    }
}

impl SymbolTable<Local> {
    pub fn local(n_locals: u32) -> Self {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: n_locals,
            num_params: 0,
            scope: PhantomData,
        }
    }

    pub fn define(&mut self, name: &str, param: bool) -> Symbol {
        let symbol = Symbol {
            name: name.to_string(),
            index: self.num_definitions,
            global: false,
            builtin: false,
            free: false,
        };
        self.store.insert(name.to_string(), symbol.clone());
        self.num_definitions += 1;
        if param {
            self.num_params += 1;
        }
        symbol
    }
}

/// Note that SymbolTable<Builtin> does not implement define unlike the other two SymbolTables!
/// The type system allows for this type of code structures.
impl SymbolTable<Builtin> {
    pub fn builtins() -> Self {
        let mut store = HashMap::new();
        for (i, builtin) in crate::object::Builtins::names().iter().enumerate() {
            store.insert(
                builtin.to_string(),
                Symbol {
                    name: builtin.to_string(),
                    index: i as u32, // This is the index that should appear in Op::GetBuiltin as an operand.
                    global: false,
                    builtin: true,
                    free: false,
                },
            );
        }

        SymbolTable {
            store,
            num_definitions: 0,
            num_params: 0,
            scope: PhantomData,
        }
    }
}

impl SymbolTable<Free> {
    pub fn free() -> Self {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
            num_params: 0,
            scope: PhantomData,
        }
    }

    pub fn define(&mut self, name: &str, param: bool) -> Symbol {
        let symbol = Symbol {
            name: name.to_string(),
            index: self.num_definitions,
            global: false,
            builtin: false,
            free: true,
        };
        self.store.insert(name.to_string(), symbol.clone());
        self.num_definitions += 1;
        if param {
            self.num_params += 1;
        }
        symbol
    }
}

impl<T> SymbolTable<T> {
    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
    }
}

impl<T> Deref for SymbolTable<T> {
    type Target = HashMap<String, Symbol>;
    fn deref(&self) -> &Self::Target {
        &self.store
    }
}

impl<T> DerefMut for SymbolTable<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.store
    }
}

/*
 * @SYMBOL_TABLE::PROG_SYMBOLS
 */
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProgSymbols {
    globals: SymbolTable<Global>,
    builtins: SymbolTable<Builtin>,
    locals: Vec<Vec<SymbolTable<Local>>>,
    free: Vec<Vec<SymbolTable<Free>>>,
    local_stack_index: usize,
    local_frame_index: usize,
    locals_count: u32,
    globals_count: u32,
    free_count: u32,
}

impl ProgSymbols {
    pub fn new() -> Self {
        ProgSymbols {
            globals: SymbolTable::global(),
            builtins: SymbolTable::builtins(),
            locals: Vec::new(),
            free: Vec::new(),
            local_stack_index: 0,
            local_frame_index: 0,
            locals_count: 0,
            globals_count: 0,
            free_count: 0,
        }
    }

    pub fn new_stack(&mut self) {
        self.locals.push(vec![]);
        self.free.push(vec![]);
        self.local_stack_index = self.locals.len() - 1;
    }

    pub fn new_frame(&mut self) {
        self.locals_count = 0;
        if self.locals[self.local_stack_index].is_empty() {
            // Add first local SymbolTable to the SymbolTable Stack.
            self.locals[self.local_stack_index].push(SymbolTable::local(0));
            self.free[self.local_stack_index].push(SymbolTable::free());
        } else {
            // Add new local SymbolTable to existing Stack.
            let n_locals =
                self.locals[self.local_stack_index][self.local_frame_index].num_definitions;
            self.locals[self.local_stack_index].push(SymbolTable::local(0));
            self.free[self.local_stack_index].push(SymbolTable::free());
        }
        // Keep track of how deep into the stack we are.
        self.local_frame_index = self.locals[self.local_stack_index].len() - 1;
    }

    pub fn pop_frame(&mut self) {
        let n_locals = self.locals[self.local_stack_index][self.local_frame_index].num_definitions;
        /*
         * Keeping track of local variables in the function stack.
         */
        self.local_frame_index -= 1;
        self.locals[self.local_stack_index][self.local_frame_index].num_definitions = n_locals;
    }

    fn current_local_stack(&self) -> Option<&Vec<SymbolTable<Local>>> {
        self.locals.get(self.local_stack_index)
    }

    pub fn current_local_frame(&mut self) -> &mut SymbolTable<Local> {
        &mut self.locals[self.local_stack_index][self.local_frame_index]
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        self.resolve_builtins(name)
            .or(self.resolve_locals(name))
            .or(self.resolve_frees(name))
            .or(self.resolve_globals(name))
    }

    fn resolve_locals(&self, name: &str) -> Option<Symbol> {
        self.locals
            .get(self.local_stack_index)?
            .get(self.local_frame_index)?
            .resolve(name)
            .cloned()
    }

    fn resolve_frees(&self, name: &str) -> Option<Symbol> {
        for stack in self
            .current_local_stack()?
            .iter()
            .take(self.local_frame_index + 1)
            .rev()
        {
            match stack.resolve(name) {
                None => continue,
                symbol => {
                    let symbol = symbol.cloned().map(|mut x| {
                        x.index = self.num_frees();
                        x.free = true;
                        x
                    });
                    return symbol;
                }
            }
        }
        None
    }

    fn resolve_globals(&self, name: &str) -> Option<Symbol> {
        match self.globals.resolve(name) {
            None => None,
            symbol => symbol.cloned(),
        }
    }

    fn resolve_builtins(&self, name: &str) -> Option<Symbol> {
        match self.builtins.resolve(name) {
            None => None,
            symbol => symbol.cloned(),
        }
    }

    pub fn globals(&self) -> &SymbolTable<Global> {
        &self.globals
    }

    pub fn local_stack(&self) -> &Vec<SymbolTable<Local>> {
        &self.locals[self.local_stack_index]
    }

    pub fn local_frame(&self) -> &SymbolTable<Local> {
        &self.locals[self.local_stack_index][self.local_frame_index]
    }

    pub fn free_variables(&self) -> SymbolTable<Local> {
        let mut st = SymbolTable::local(0);
        for (key, val) in self.locals[self.local_stack_index][self.local_frame_index].iter() {
            if val.free {
                let mut val = val.clone();
                val.free = false;
                st.insert(key.to_owned(), val);
            }
        }
        st
    }

    pub fn num_locals(&self) -> usize {
        let n: usize = self.locals[self.local_stack_index][self.local_frame_index]
            .iter()
            .map(|(key, val)| if !val.free { 1usize } else { 0usize })
            .sum();

        n - self.local_frame().num_params as usize
    }

    pub fn num_frees(&self) -> u32 {
        self.locals[self.local_stack_index][self.local_frame_index]
            .iter()
            .map(|(key, val)| if val.free { 1u32 } else { 0u32 })
            .sum()
    }

    pub fn define_global(&mut self, name: &str) -> u32 {
        let count = self.globals_count;
        let symbol = self.globals.define(name);
        self.globals_count += 1;
        symbol.index
    }

    pub fn define_local(&mut self, name: &str) -> u32 {
        let count = self.locals_count;
        let symbol =
            self.locals[self.local_stack_index][self.local_frame_index].define(name, false);
        self.locals_count += 1;
        symbol.index
    }

    pub fn define_param(&mut self, name: &str) -> u32 {
        let symbol = self.locals[self.local_stack_index][self.local_frame_index].define(name, true);
        symbol.index
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
                    index: 0,
                    global: true,
                    builtin: false,
                    free: false,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    name: "b".to_string(),
                    index: 1,
                    global: true,
                    builtin: false,
                    free: false,
                },
            ),
        ]);

        let mut symbols = ProgSymbols::new();
        symbols.define_global("a");
        symbols.define_global("b");

        assert_eq!(symbols.globals().deref(), &expected)
    }

    #[test]
    fn test_resolve() {
        let expected: HashMap<String, Symbol> = HashMap::from([
            (
                "a".to_string(),
                Symbol {
                    name: "a".to_string(),
                    index: 0,
                    global: true,
                    builtin: false,
                    free: false,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    name: "b".to_string(),
                    index: 1,
                    global: true,
                    builtin: false,
                    free: false,
                },
            ),
        ]);

        let mut symbols = ProgSymbols::new();
        symbols.define_global("a");
        symbols.define_global("b");

        assert_eq!(symbols.resolve("a").unwrap(), *expected.get("a").unwrap());
        assert_eq!(symbols.resolve("b").unwrap(), *expected.get("b").unwrap());
    }

    #[ignore]
    #[test]
    fn local_bindings() {
        let expected: HashMap<String, Symbol> = HashMap::from([
            (
                "a".to_string(),
                Symbol {
                    name: "a".to_string(),
                    index: 0,
                    global: true,
                    builtin: false,
                    free: false,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    name: "b".to_string(),
                    index: 1,
                    global: true,
                    builtin: false,
                    free: false,
                },
            ),
            (
                "c".to_string(),
                Symbol {
                    name: "c".to_string(),
                    index: 0,
                    global: false,
                    builtin: false,
                    free: false,
                },
            ),
            (
                "d".to_string(),
                Symbol {
                    name: "d".to_string(),
                    index: 1,
                    global: false,
                    builtin: false,
                    free: false,
                },
            ),
        ]);

        let mut symbols = ProgSymbols::new();
        symbols.define_global("a");
        symbols.define_global("b");

        symbols.define_local("c");
        symbols.define_local("d");

        for (key, val) in symbols.globals().iter() {
            assert_eq!(symbols.resolve(key).unwrap(), *expected.get(key).unwrap());
        }

        for (key, val) in symbols.local_frame().iter() {
            assert_eq!(symbols.resolve(key).unwrap(), *expected.get(key).unwrap());
        }
    }

    #[test]
    fn local_nested_bindings() {
        let expected: HashMap<String, Symbol> = HashMap::from([
            (
                "a".to_string(),
                Symbol {
                    name: "a".to_string(),
                    index: 0,
                    global: true,
                    builtin: false,
                    free: false,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    name: "b".to_string(),
                    index: 1,
                    global: true,
                    builtin: false,
                    free: false,
                },
            ),
            (
                "c".to_string(),
                Symbol {
                    name: "c".to_string(),
                    index: 0,
                    global: false,
                    builtin: false,
                    free: false,
                },
            ),
            (
                "d".to_string(),
                Symbol {
                    name: "d".to_string(),
                    index: 1,
                    global: false,
                    builtin: false,
                    free: false,
                },
            ),
        ]);

        let expected_nested: HashMap<String, Symbol> = HashMap::from([
            (
                "a".to_string(),
                Symbol {
                    name: "a".to_string(),
                    index: 0,
                    global: true,
                    builtin: false,
                    free: false,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    name: "b".to_string(),
                    index: 1,
                    global: true,
                    builtin: false,
                    free: false,
                },
            ),
            (
                "e".to_string(),
                Symbol {
                    name: "e".to_string(),
                    index: 0,
                    global: true,
                    builtin: false,
                    free: false,
                },
            ),
            (
                "f".to_string(),
                Symbol {
                    name: "f".to_string(),
                    index: 1,
                    global: true,
                    builtin: false,
                    free: false,
                },
            ),
        ]);

        // let mut global = Rc::new(RefCell::new(SymbolTable::new(None)));
        // global.borrow_mut().define("a");
        // global.borrow_mut().define("b");

        // let mut local = Rc::new(RefCell::new(SymbolTable::new(Some(Rc::clone(&global)))));
        // local.borrow_mut().define("c");
        // local.borrow_mut().define("d");

        // let mut nested_local = Rc::new(RefCell::new(SymbolTable::new(Some(Rc::clone(&local)))));
        // nested_local.borrow_mut().define("e");
        // nested_local.borrow_mut().define("f");

        // for (key, val) in local.borrow().iter() {
        //     assert_eq!(
        //         local.borrow().resolve(key).unwrap(),
        //         *expected.get(key).unwrap()
        //     );
        // }

        // for (key, val) in nested_local.borrow().iter() {
        //     assert_eq!(
        //         nested_local.borrow().resolve(key).unwrap(),
        //         *expected_nested.get(key).unwrap()
        //     );
        // }
    }
}
