use ast::{Name, Type};
use im::hashmap::HashMap;
use std::sync::Arc;

#[derive(Debug, PartialEq, Clone)]
pub struct Scope {
    pub symbols: HashMap<Name, SymbolTableEntry>,
    pub parent: Option<usize>,
}

pub struct SymbolTable {
    scopes: Vec<Scope>,
    function_index: usize,
    current_scope: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolTableEntry {
    Function {
        // Index into function array
        index: usize,
        params_type: Arc<Type>,
        return_type: Arc<Type>,
    },
    Var {
        var_type: Arc<Type>,
    },
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![Scope {
                symbols: HashMap::new(),
                parent: None,
            }],
            function_index: 0,
            current_scope: 0,
        }
    }

    pub fn get_function_index(&self) -> usize {
        self.function_index
    }

    pub fn push_scope(&mut self) -> usize {
        let new_scope = Scope {
            symbols: HashMap::new(),
            parent: Some(self.current_scope),
        };
        self.scopes.push(new_scope);
        let previous_scope = self.current_scope;
        self.current_scope = self.scopes.len() - 1;
        previous_scope
    }

    pub fn set_scope(&mut self, previous_scope: usize) -> usize {
        let old_scope = self.current_scope;
        self.current_scope = previous_scope;
        old_scope
    }

    pub fn lookup_name(&self, name: usize) -> Option<&SymbolTableEntry> {
        self.lookup_name_in_scope(name, self.current_scope)
    }

    // Looks up name in scope
    pub fn lookup_name_in_scope(&self, name: usize, scope: usize) -> Option<&SymbolTableEntry> {
        let mut index = Some(scope);
        while let Some(i) = index {
            if let Some(entry) = self.scopes[i].symbols.get(&name) {
                return Some(entry);
            }
            index = self.scopes[i].parent;
        }
        None
    }

    pub fn insert_var(&mut self, name: Name, var_type: Arc<Type>) {
        self.scopes[self.current_scope]
            .symbols
            .insert(name, SymbolTableEntry::Var { var_type });
    }

    pub fn insert_function(&mut self, name: Name, params_type: Arc<Type>, return_type: Arc<Type>) {
        if self.lookup_name(name).is_none() {
            println!("INDEX: {}, NAME: {}", self.function_index, name);
            self.scopes[self.current_scope].symbols.insert(
                name,
                SymbolTableEntry::Function {
                    index: self.function_index,
                    params_type,
                    return_type,
                },
            );
            self.function_index += 1;
        }
    }
}
