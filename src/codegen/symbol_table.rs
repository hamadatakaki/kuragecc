use super::super::ast::types::Type;
use super::super::ast::ASTIdentifier;
use super::expression::Symbol;

use std::collections::HashMap;

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

    pub fn variable_symbol(&mut self, name: &str, ty: Type) -> Symbol {
        self.val_count += 1;
        let sym = format!("%val_{}_{}", name, self.val_count);
        Symbol::new(sym, ty)
    }

    pub fn anonymous_symbol(&mut self, ty: Type) -> Symbol {
        self.ano_count += 1;
        let sym = format!("%ano_{}", self.ano_count);

        match ty {
            Type::None => {
                println!("{}", sym);
            }
            _ => {}
        }

        Symbol::new(sym, ty)
    }

    pub fn get_symbol(&mut self, name: &String) -> Option<Symbol> {
        self.base.get(name).map(|sym| sym.clone())
    }

    pub fn register_id(&mut self, id: ASTIdentifier) -> Symbol {
        let name = id.get_name();
        let sym = self.variable_symbol(name.as_str(), id.get_type());
        self.base.insert(name, sym.clone());
        sym
    }

    pub fn overwrite_name_and_symbol(&mut self, name: String, sym: Symbol) {
        self.base.insert(name, sym);
    }
}

pub struct FunctionTable {
    base: HashMap<String, Symbol>,
}

impl FunctionTable {
    pub fn new() -> Self {
        Self {
            base: HashMap::new(),
        }
    }

    pub fn register_func(&mut self, name: String, ty: Type) -> Symbol {
        let sym = Symbol::new(name.clone(), ty);
        self.base.insert(name, sym.clone());
        sym
    }

    pub fn get_symbol(&mut self, name: &String) -> Option<Symbol> {
        self.base.get(name).map(|sym| sym.clone())
    }
}
