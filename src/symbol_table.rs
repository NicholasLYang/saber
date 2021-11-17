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
        var_types: Vec<TypeId>,
    },
    Regular,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SymbolEntry {
    pub var_index: usize,
    pub var_type: TypeId,
    pub function_info: Option<FunctionInfo>,
}

#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
    function_index: usize,
    current_func_scope: Option<ScopeId>,
    pub current_scope: ScopeId,
    global_var_index: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionInfo {
    // Index into function array
    pub func_index: usize,
    pub func_scope: ScopeId,
    pub params_type: TypeId,
    pub return_type: TypeId,
    pub is_top_level: bool,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![Scope {
                symbols: HashMap::new(),
                scope_type: ScopeType::Regular,
                parent: None,
                parent_func_scope: None,
            }],
            function_index: 0,
            current_func_scope: None,
            current_scope: 0,
            global_var_index: 0,
        }
    }

    pub fn get_function_index(&self) -> usize {
        self.function_index
    }

    pub fn push_scope(&mut self) -> usize {
        let new_scope = Scope {
            symbols: HashMap::new(),
            scope_type: ScopeType::Regular,
            parent: Some(self.current_scope),
            parent_func_scope: self.current_func_scope,
        };
        self.scopes.push(new_scope);
        self.current_scope = self.scopes.len() - 1;
        self.current_scope
    }

    pub fn swap_scope(&mut self, scope: ScopeId) -> ScopeId {
        let old_scope = self.current_scope;
        self.current_scope = scope;
        old_scope
    }

    pub fn restore_scope(&mut self) -> usize {
        let scope = self.current_scope;
        self.current_scope = self.scopes[scope].parent.unwrap();
        scope
    }

    pub fn get_scope_entries(&self, scope: usize) -> impl Iterator<Item = &(Name, SymbolEntry)> {
        self.scopes[scope].symbols.iter()
    }

    // Gets the entry and the list of functions that we pass in the lookup process
    pub fn lookup_name_with_function_hierarchy(
        &self,
        name: Name,
    ) -> Option<(&SymbolEntry, Vec<usize>)> {
        let mut functions = Vec::new();
        let mut index = Some(self.current_scope);
        while let Some(i) = index {
            if let Some(entry) = self.scopes[i].symbols.get(&name) {
                return Some((entry, functions));
            }
            if let ScopeType::Function {
                func_index,
                var_types: _,
            } = self.scopes[i].scope_type
            {
                functions.push(func_index);
            }
            index = self.scopes[i].parent;
        }
        None
    }

    pub fn lookup_name(&mut self, name: Name) -> Option<&SymbolEntry> {
        self.lookup_name_in_scope(name, self.current_scope)
    }

    // Looks up name in scope
    pub fn lookup_name_in_scope(&mut self, name: Name, scope: usize) -> Option<&SymbolEntry> {
        let mut index = Some(scope);
        while let Some(i) = index {
            if let Some(entry) = self.scopes[i].symbols.get(&name) {
                return Some(entry);
            }
            index = self.scopes[i].parent;
        }
        None
    }

    #[allow(dead_code)]
    pub fn get_parent_scope(&self, scope: ScopeId) -> Option<ScopeId> {
        self.scopes[scope].parent
    }

    pub fn insert_var(&mut self, name: Name, var_type: TypeId) {
        self.insert_var_with_function_info(name, var_type, None)
    }

    pub fn insert_var_with_function_info(
        &mut self,
        name: Name,
        var_type: TypeId,
        function_info: Option<FunctionInfo>,
    ) {
        // If we're inside a function, then insert var into function's local variables
        let var_index = if let Some(func_scope) = self.current_func_scope {
            if let ScopeType::Function {
                func_index: _,
                var_types,
            } = &mut self.scopes[func_scope].scope_type
            {
                var_types.push(var_type);
                var_types.len()
            } else {
                unreachable!()
            }
        } else {
            let var_index = self.global_var_index;
            self.global_var_index += 1;
            var_index
        };

        self.scopes[self.current_scope].symbols.insert(
            name,
            SymbolEntry {
                var_type,
                var_index,
                function_info,
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
    ) -> usize {
        let func_scope = self.scopes.len();
        let is_top_level = self.scopes[self.current_scope].parent_func_scope.is_none();
        let function_info = FunctionInfo {
            func_index: self.function_index,
            func_scope,
            params_type,
            return_type,
            is_top_level,
        };
        self.insert_var_with_function_info(name, type_, Some(function_info));

        self.scopes.push(Scope {
            symbols: HashMap::new(),
            scope_type: ScopeType::Function {
                var_types: Vec::new(),
                func_index: self.function_index,
            },
            parent_func_scope: self.current_func_scope,
            parent: Some(self.current_scope),
        });
        let func_index = self.function_index;
        self.function_index += 1;
        func_index
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
