use crate::ast::{Name, TypeId};
use im::hashmap::HashMap;

pub type ScopeId = usize;

#[derive(Debug, PartialEq, Clone)]
pub struct Scope {
    pub symbols: HashMap<Name, SymbolEntry>,
    pub scope_type: ScopeType,
    pub parent: Option<ScopeId>,
    pub parent_func_scope: Option<ScopeId>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ScopeType {
    Function {
        func_index: usize,
        captures: Vec<(Name, ScopeId, TypeId)>,
    },
    Regular,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SymbolEntry {
    // Is the variable captured by a function?
    pub is_enclosed_var: bool,
    pub var_index: usize,
    pub var_type: TypeId,
    pub function_info: Option<FunctionInfo>,
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
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionInfo {
    // Index into function array
    pub func_index: usize,
    pub func_scope: ScopeId,
    pub params_type: TypeId,
    pub return_type: TypeId,
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
                scope_type: ScopeType::Regular,
                parent: None,
                parent_func_scope: None,
            }],
            function_index: 4,
            var_types: Vec::new(),
            current_scope: 0,
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

    pub fn push_scope(&mut self) -> usize {
        let parent_func_scope = self.get_func_scope(self.current_scope);
        let new_scope = Scope {
            symbols: HashMap::new(),
            scope_type: ScopeType::Regular,
            parent: Some(self.current_scope),
            parent_func_scope,
        };
        self.scopes.push(new_scope);
        let previous_scope = self.current_scope;
        self.current_scope = self.scopes.len() - 1;
        previous_scope
    }

    pub fn swap_scope(&mut self, scope: ScopeId) -> ScopeId {
        let old_scope = self.current_scope;
        self.current_scope = scope;
        old_scope
    }

    pub fn restore_scope(&mut self) -> usize {
        let scope = self.current_scope;
        self.current_scope = self.scopes[scope].parent.unwrap();
        let mut long_range_captures = Vec::new();
        // If we're finished checking a function scope...
        if let ScopeType::Function {
            captures,
            func_index: _,
        } = &self.scopes[scope].scope_type
        {
            // Loop through the captures
            for (name, scope_id, type_id) in captures {
                // If they aren't captured from the previous function scope, we add them to the
                // captures list of the previous function
                if let Some(parent_func_scope) = self.scopes[scope].parent_func_scope {
                    if *scope_id != parent_func_scope {
                        long_range_captures.push((*name, *scope_id, *type_id));
                    }
                }
            }
        }
        if let Some(parent_func_scope) = self.scopes[scope].parent_func_scope {
            if let ScopeType::Function {
                captures,
                func_index: _,
            } = &mut self.scopes[parent_func_scope].scope_type
            {
                for capture in long_range_captures {
                    captures.push(capture);
                }
            }
        }
        scope
    }

    pub fn get_scope_entries(&self, scope: usize) -> impl Iterator<Item = &(Name, SymbolEntry)> {
        self.scopes[scope].symbols.iter()
    }

    pub fn get_captures(&self, scope: usize) -> Option<&Vec<(Name, ScopeId, TypeId)>> {
        match &self.scopes[scope].scope_type {
            ScopeType::Function {
                captures,
                func_index: _,
            } => Some(captures),
            ScopeType::Regular => None,
        }
    }

    pub fn lookup_name(&mut self, name: usize) -> Option<&SymbolEntry> {
        self.lookup_name_in_scope(name, self.current_scope)
    }

    // Looks up name in scope
    pub fn lookup_name_in_scope(&mut self, name: usize, scope: usize) -> Option<&SymbolEntry> {
        let mut index = Some(scope);
        // If we cross a function boundary, i.e. we find the var in a scope outside the current
        // function scope, then we modify the symbol table entry to say that it's
        // an enclosed var and therefore must be boxed
        // The function scope of the variable usage
        let usage_func_scope = self.get_func_scope(scope);
        while let Some(i) = index {
            if self.scopes[i].symbols.contains_key(&name) {
                {
                    let def_func_scope = self.get_func_scope(i);
                    let entry_type = self.scopes[i].symbols.get(&name).unwrap().var_type;
                    if def_func_scope != usage_func_scope {
                        if let Some(usage_func_scope) = usage_func_scope {
                            if let ScopeType::Function {
                                captures,
                                func_index: _,
                            } = &mut self.scopes[usage_func_scope].scope_type
                            {
                                def_func_scope.map(|func_scope| {
                                    captures.push((name, func_scope, entry_type));
                                });
                            }
                        }
                        let mut entry = self.scopes[i].symbols.get_mut(&name).unwrap();
                        entry.is_enclosed_var = true;
                    }
                }
                return self.scopes[i].symbols.get(&name);
            }
            index = self.scopes[i].parent;
        }
        None
    }

    // Looks up name for code gen and returns index and whether the var is captured
    // or just local.
    // Only goes up to the next function barrier
    // NOTE: This API sucks (unmarked bools are confusing) so don't keep it
    pub fn codegen_lookup(&self, name: usize) -> Option<(usize, bool)> {
        let mut index = Some(self.current_scope);
        while let Some(i) = index {
            if let Some(entry) = self.scopes[i].symbols.get(&name) {
                let var_index = entry.var_index;
                let offset = self
                    .get_func_scope(i)
                    .map(|func_scope| match &self.scopes[func_scope].scope_type {
                        ScopeType::Function {
                            captures,
                            func_index: _,
                        } => captures.len(),
                        ScopeType::Regular => panic!("Not reachable"),
                    })
                    .unwrap_or(0);
                return Some((var_index + offset, false));
            }
            if let ScopeType::Function {
                captures,
                func_index: _,
            } = &self.scopes[i].scope_type
            {
                return captures
                    .iter()
                    .position(|(n, _, _)| name == *n)
                    .map(|index| (index, true));
            }
            index = self.scopes[i].parent;
        }
        None
    }

    fn get_func_scope(&self, scope: ScopeId) -> Option<ScopeId> {
        match self.scopes[scope].scope_type {
            ScopeType::Function {
                captures: _,
                func_index: _,
            } => Some(scope),
            ScopeType::Regular => self.scopes[scope].parent_func_scope,
        }
    }

    pub fn get_parent_scope(&self, scope: ScopeId) -> Option<ScopeId> {
        self.scopes[scope].parent
    }

    pub fn insert_var(&mut self, name: Name, var_type: TypeId) {
        self.var_types.push(var_type);
        self.scopes[self.current_scope].symbols.insert(
            name,
            SymbolEntry {
                is_enclosed_var: false,
                var_type,
                var_index: self.var_types.len() - 1,
                function_info: None,
            },
        );
    }

    // There's a lot of bookkeeping here
    // We're adding it as a variable
    // Then we insert it into the outer scope
    // Then we add the function scope
    // Why do this all at once?
    // We need to link all of these together
    pub fn insert_function(
        &mut self,
        name: Name,
        params_type: TypeId,
        return_type: TypeId,
        type_: TypeId,
    ) -> ScopeId {
        self.var_types.push(type_);
        let func_scope = self.scopes.len();
        self.scopes[self.current_scope].symbols.insert(
            name,
            SymbolEntry {
                is_enclosed_var: false,
                var_index: self.var_types.len() - 1,
                var_type: type_,
                function_info: Some(FunctionInfo {
                    func_index: self.function_index,
                    func_scope,
                    params_type,
                    return_type,
                }),
            },
        );
        self.scopes.push(Scope {
            symbols: HashMap::new(),
            scope_type: ScopeType::Function {
                func_index: self.function_index,
                captures: Vec::new(),
            },
            parent_func_scope: self.get_func_scope(self.current_scope),
            parent: Some(self.current_scope),
        });
        self.current_scope = func_scope;
        self.function_index += 1;
        func_scope
    }
}
