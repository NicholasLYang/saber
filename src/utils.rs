use bimap::BiMap;

pub struct SymbolTable(BiMap<String, usize>, usize);

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable(BiMap::new(), 0)
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

    pub fn get_id(&mut self, sym: &String) -> Option<&usize> {
        self.0.get_by_left(sym)
    }

    pub fn get_str(&mut self, id: &usize) -> &str {
        self.0.get_by_right(id).unwrap()
    }

    pub fn get_fresh_name(&mut self) -> usize {
        let ident = format!("var({})", self.1);
        self.insert(ident)
    }
}
