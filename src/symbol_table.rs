use crate::ast::{Name, TypeId};
use im::hashmap::HashMap;

pub type ScopeId = usize;

#[derive(Debug, PartialEq, Clone)]
pub struct Scope {
    pub symbols: HashMap<Name, SymbolEntry>,
    pub scope_type: ScopeType,
    pub parent: Option<ScopeId>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ScopeType {
    Function {
        captures: Vec<(Name, ScopeId)>,
        previous_func_scope: Option<ScopeId>,
    },
    Regular {
        func_scope: Option<ScopeId>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct SymbolEntry {
    // Is the variable captured by a function?
    pub is_enclosed_var: bool,
    pub entry_type: EntryType,
}

#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
    function_index: usize,
    // As we collect variables, we insert
    // them into a vec, then when we finish
    // typechecking the function, we reset and spit out
    // the variable types
    var_types: Vec<TypeId>,
    current_scope: ScopeId,
    // Variables that are accessed from outside of your scope
    captures: Vec<(Name, ScopeId)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum EntryType {
    Function {
        // Index into function array
        index: usize,
        params_type: TypeId,
        return_type: TypeId,
        type_: TypeId,
    },
    Var {
        var_type: TypeId,
        index: usize,
    },
}

pub static ALLOC_INDEX: u32 = 0;
pub static DEALLOC_INDEX: u32 = 1;
pub static CLONE_INDEX: u32 = 2;
pub static STREQ_INDEX: u32 = 3;

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![Scope {
                symbols: HashMap::new(),
                scope_type: ScopeType::Regular { func_scope: None },
                parent: None,
            }],
            function_index: 4,
            var_types: Vec::new(),
            current_scope: 0,
            captures: Vec::new(),
        }
    }

    pub fn get_function_index(&self) -> usize {
        self.function_index
    }

    pub fn reset_vars(&mut self) -> Vec<TypeId> {
        let mut var_types = Vec::new();
        std::mem::swap(&mut var_types, &mut self.var_types);
        var_types
    }

    pub fn restore_vars(&mut self, var_types: Vec<TypeId>) -> Vec<TypeId> {
        let mut var_types = var_types;
        std::mem::swap(&mut self.var_types, &mut var_types);
        var_types
    }

    pub fn push_scope(&mut self, is_func_scope: bool) -> usize {
        let func_scope = match self.scopes[self.current_scope].scope_type {
            ScopeType::Function {
                captures: _,
                previous_func_scope: _,
            } => Some(self.current_scope),
            ScopeType::Regular { func_scope } => func_scope,
        };
        let scope_type = if is_func_scope {
            ScopeType::Function {
                captures: Vec::new(),
                previous_func_scope: func_scope,
            }
        } else {
            ScopeType::Regular { func_scope }
        };
        let new_scope = Scope {
            symbols: HashMap::new(),
            scope_type,
            parent: Some(self.current_scope),
        };
        self.scopes.push(new_scope);
        let previous_scope = self.current_scope;
        self.current_scope = self.scopes.len() - 1;
        previous_scope
    }

    pub fn restore_scope(&mut self, previous_scope: usize) -> usize {
        let scope = self.current_scope;
        self.current_scope = previous_scope;
        let mut long_range_captures = Vec::new();
        // If we're finished checking a function scope...
        let previous_func_scope = if let ScopeType::Function {
            captures,
            previous_func_scope,
        } = &self.scopes[scope].scope_type
        {
            // Loop through the captures
            for (name, scope_id) in captures {
                // If they aren't captured from the previous function scope, we add them to the
                // captures list of the previous function
                if let Some(previous_func_scope) = previous_func_scope {
                    if scope_id != previous_func_scope {
                        long_range_captures.push((*name, *scope_id));
                    }
                }
            }
            *previous_func_scope
        } else {
            None
        };
        if let Some(previous_func_scope) = previous_func_scope {
            if let ScopeType::Function {
                captures,
                previous_func_scope: _,
            } = &mut self.scopes[previous_func_scope].scope_type
            {
                for capture in long_range_captures {
                    captures.push(capture);
                }
            }
        }
        scope
    }

    pub fn lookup_name(&mut self, name: usize) -> Option<&SymbolEntry> {
        self.lookup_name_in_scope(name, self.current_scope)
    }

    pub fn get_scope_entries(&self, scope: usize) -> impl Iterator<Item = &(Name, SymbolEntry)> {
        self.scopes[scope].symbols.iter()
    }

    // Looks up name in scope
    pub fn lookup_name_in_scope(&mut self, name: usize, scope: usize) -> Option<&SymbolEntry> {
        let mut index = Some(scope);
        // If we cross a function boundary, i.e. we find the var in a scope outside the current
        // function scope, then we modify the symbol table entry to say that it's
        // an enclosed var and therefore must be boxed
        let mut is_enclosed_var = false;
        // The function scope of the variable usage
        let usage_func_scope = self.get_func_scope(scope);
        while let Some(i) = index {
            if let ScopeType::Function {
                captures: _,
                previous_func_scope: _,
            } = self.scopes[i].scope_type
            {
                // If usage_func_scope is None, we're dealing with a global usage
                // No captures here
                usage_func_scope.map(|usage_func_scope| {
                    if usage_func_scope != i {
                        is_enclosed_var = true;
                    }
                });
            }
            if self.scopes[i].symbols.contains_key(&name) {
                {
                    let def_func_scope = self.get_func_scope(i);
                    if is_enclosed_var {
                        if let Some(usage_func_scope) = usage_func_scope {
                            if let ScopeType::Function {
                                captures,
                                previous_func_scope: _,
                            } = &mut self.scopes[usage_func_scope].scope_type
                            {
                                def_func_scope.map(|func_scope| {
                                    captures.push((name, func_scope));
                                });
                            }
                        }
                    }
                    let entry = self.scopes[i].symbols.get_mut(&name).unwrap();
                    entry.is_enclosed_var = is_enclosed_var;
                }
                return self.scopes[i].symbols.get(&name);
            }
            index = self.scopes[i].parent;
        }
        None
    }

    fn get_func_scope(&self, scope: ScopeId) -> Option<ScopeId> {
        match self.scopes[scope].scope_type {
            ScopeType::Function {
                captures: _,
                previous_func_scope,
            } => previous_func_scope,
            ScopeType::Regular { func_scope } => func_scope,
        }
    }

    pub fn insert_var(&mut self, name: Name, var_type: TypeId) {
        self.var_types.push(var_type.clone());
        self.scopes[self.current_scope].symbols.insert(
            name,
            SymbolEntry {
                is_enclosed_var: false,
                entry_type: EntryType::Var {
                    var_type,
                    index: self.var_types.len() - 1,
                },
            },
        );
    }

    pub fn insert_function(
        &mut self,
        name: Name,
        params_type: TypeId,
        return_type: TypeId,
        type_: TypeId,
    ) {
        if self.lookup_name(name).is_none() {
            self.scopes[self.current_scope].symbols.insert(
                name,
                SymbolEntry {
                    is_enclosed_var: false,
                    entry_type: EntryType::Function {
                        index: self.function_index,
                        params_type,
                        return_type,
                        type_,
                    },
                },
            );
            self.function_index += 1;
        }
    }
}
