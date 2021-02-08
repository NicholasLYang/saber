use crate::ast::{Type, TypeId};
use bimap::BiMap;
use std::collections::HashMap;

pub struct SaberProgram {
    pub wasm_bytes: Vec<u8>,
    pub runtime_types: HashMap<TypeId, Vec<bool>>,
}

#[derive(Debug)]
pub struct NameTable(BiMap<String, usize>, usize);

impl NameTable {
    pub fn new() -> Self {
        NameTable(BiMap::new(), 0)
    }
    pub fn insert(&mut self, sym: String) -> usize {
        if let Some(id) = self.0.get_by_left(&sym) {
            *id
        } else {
            let id = self.1;
            self.0.insert(sym, id);
            self.1 += 1;
            id
        }
    }

    pub fn get_id(&self, sym: &String) -> Option<&usize> {
        self.0.get_by_left(sym)
    }

    pub fn get_str(&self, id: &usize) -> &str {
        self.0.get_by_right(id).unwrap()
    }

    pub fn get_fresh_name(&mut self) -> usize {
        let ident = format!("var({})", self.1);
        self.insert(ident)
    }
}

// "Table" is a loose term here
pub struct TypeTable {
    table: Vec<Type>,
}
