use crate::ast::{Name, TypeId};
use im::hashmap::HashMap;
use std::error::Error;

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
    pub var_type: TypeId,
    pub var_index: usize,
    pub func_index: Option<usize>,
}

#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
    pub functions: Vec<FunctionInfo>,
    pub current_scope: ScopeId,
    pub current_function_idx: Option<usize>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionInfo {
    pub func_scope: ScopeId,
    pub local_variables: Vec<TypeId>,
    pub param_types: Vec<TypeId>,
    pub return_types: Vec<TypeId>,
    pub is_top_level: bool,
}

pub enum VarIndex {
    Local(usize),
    Capture(usize),
}

#[allow(dead_code)]
pub static CLONE_INDEX: u32 = 2;
pub static STREQ_INDEX: u32 = 3;
pub static PRINT_HEAP_INDEX: u32 = 4;
#[allow(dead_code)]
pub static PRINT_INT_INDEX: u32 = 5;
#[allow(dead_code)]
pub static PRINT_FLOAT_INDEX: u32 = 6;
#[allow(dead_code)]
pub static PRINT_STRING_INDEX: u32 = 7;
pub static PRINT_CHAR_INDEX: u32 = 8;

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![Scope {
                symbols: HashMap::new(),
                scope_type: ScopeType::Regular,
                parent: None,
                parent_func_scope: None,
            }],
            functions: Vec::new(),
            current_scope: 0,
            current_function_idx: None,
        }
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
        self.current_scope = self.scopes.len() - 1;
        self.current_scope
    }

    pub fn execute_in_scope<F, E: Error>(&mut self, scope: ScopeId, fun: F) -> Result<(), E>
    where
        F: Fn() -> Result<(), E>,
    {
        let old_scope = self.current_scope;
        self.current_scope = scope;
        fun()?;
        self.current_scope = old_scope;
        Ok(())
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

    pub fn get_captures(&self) -> Option<&Vec<(Name, ScopeId, TypeId)>> {
        self.get_captures_in_scope(self.current_scope)
    }

    pub fn get_captures_in_scope(&self, scope: usize) -> Option<&Vec<(Name, ScopeId, TypeId)>> {
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
                                if let Some(func_scope) = def_func_scope {
                                    captures.push((name, func_scope, entry_type));
                                };
                            }
                        }
                    }
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
                func_index: _,
            } => Some(scope),
            ScopeType::Regular => self.scopes[scope].parent_func_scope,
        }
    }

    #[allow(dead_code)]
    pub fn get_parent_scope(&self, scope: ScopeId) -> Option<ScopeId> {
        self.scopes[scope].parent
    }

    pub fn insert_var(&mut self, name: Name, var_type: TypeId) -> usize {
        let func_scope = self.current_function_idx.expect("TODO: Handle global case");
        self.functions[func_scope].local_variables.push(var_type);
        let var_index = self.functions[func_scope].local_variables.len() - 1;

        self.scopes[self.current_scope].symbols.insert(
            name,
            SymbolEntry {
                var_type,
                var_index,
                func_index: None,
            },
        );

        var_index
    }

    // There's a lot of bookkeeping here
    // We're adding it as a variable
    // Then we insert it into the outer scope
    // Then we add the function scope
    // Why do this all at once?
    // We need to link all of these together
    // Returns the var index
    pub fn insert_function(
        &mut self,
        name: Name,
        param_types: Vec<TypeId>,
        return_types: Vec<TypeId>,
        type_: TypeId,
    ) -> usize {
        let is_top_level = self.scopes[self.current_scope].parent_func_scope.is_none();

        self.functions.push(FunctionInfo {
            func_scope,
            local_variables: Vec::new(),
            param_types,
            return_types,
            is_top_level,
        });
        let func_index = self.functions.len() - 1;

        // Adding function as a variable
        let outer_func_scope = self.current_function_idx.expect("TODO: Handle global case");
        self.functions[outer_func_scope].local_variables.push(type_);
        let var_index = self.functions[func_scope].local_variables.len() - 1;

        self.scopes[self.current_scope].symbols.insert(
            name,
            SymbolEntry {
                var_type,
                var_index,
                func_index: Some(func_index),
            },
        );

        self.current_function_idx = Some(func_index);
        // Add the function scope
        self.scopes.push(Scope {
            symbols: HashMap::new(),
            scope_type: ScopeType::Function {
                func_index,
                captures: Vec::new(),
            },
            parent_func_scope: self.get_func_scope(self.current_scope),
            parent: Some(self.current_scope),
        });

        var_index
    }

    #[allow(dead_code)]
    pub fn print_scope_chain(&self) {
        let scope = self.current_scope;
        print!("{}", scope);
        let mut scope = self.scopes[scope].parent;
        while let Some(s) = scope {
            print!(" -> {}", s);
            scope = self.scopes[s].parent;
        }
        println!();
    }
}
