use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Symbol(String);

impl Symbol {
    pub fn reveal(&self) -> String {
        self.0.clone()
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    val_count: usize,
    ano_count: usize,
    base: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            val_count: 0,
            ano_count: 0,
            base: HashMap::new(),
        }
    }

    pub fn variable_symbol(&mut self, name: &str) -> Symbol {
        self.val_count += 1;
        let s = format!("%val_{}_{}", self.val_count, name);
        Symbol(s)
    }

    pub fn anonymous_symbol(&mut self) -> Symbol {
        self.ano_count += 1;
        let symbol = format!("%ano_{}", self.ano_count);
        Symbol(symbol)
    }

    pub fn get_symbol(&mut self, name: &String) -> Option<Symbol> {
        self.base.get(name).map(|sym| sym.clone())
    }

    pub fn register_name(&mut self, name: String) -> Symbol {
        let symbol = self.variable_symbol(name.as_str());
        self.base.insert(name, symbol.clone());
        symbol
    }

    pub fn overwrite_name_and_symbol(&mut self, name: String, sym: Symbol) {
        self.base.insert(name, sym);
    }
}
