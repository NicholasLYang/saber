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
    // As we collect variables, we insert
    // them into a vec, then when we finish
    // typechecking the function, we reset and spit out
    // the variable types
    var_types: Vec<Arc<Type>>,
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
        index: usize,
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
            var_types: Vec::new(),
            current_scope: 0,
        }
    }

    pub fn get_function_index(&self) -> usize {
        self.function_index
    }

    pub fn reset_vars(&mut self) -> Vec<Arc<Type>> {
        let mut var_types = Vec::new();
        std::mem::swap(&mut var_types, &mut self.var_types);
        var_types
    }

    pub fn restore_vars(&mut self, var_types: Vec<Arc<Type>>) -> Vec<Arc<Type>> {
        let mut var_types = var_types;
        std::mem::swap(&mut self.var_types, &mut var_types);
        var_types
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

    pub fn restore_scope(&mut self, previous_scope: usize) -> usize {
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
        self.var_types.push(var_type.clone());
        self.scopes[self.current_scope].symbols.insert(
            name,
            SymbolTableEntry::Var {
                var_type,
                index: self.var_types.len() - 1,
            },
        );
    }

    pub fn insert_function(&mut self, name: Name, params_type: Arc<Type>, return_type: Arc<Type>) {
        if self.lookup_name(name).is_none() {
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
