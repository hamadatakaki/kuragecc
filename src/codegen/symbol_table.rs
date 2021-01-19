use super::super::ast::types::Type;
use super::super::ast::ASTIdentifier;
use super::expression::Symbol;

use std::collections::HashMap;

pub trait SymbolicTable {
    type Item;
    fn search_symbol(&self, name: &String) -> Option<Self::Item>;
    fn register(&mut self, id: ASTIdentifier) -> Self::Item;
}

#[derive(Debug, Clone)]
pub struct VariableTable {
    val_count: usize,
    ano_count: usize,
    base: HashMap<String, Symbol>,
}

impl VariableTable {
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
        Symbol::new(format!("%ano_{}", self.ano_count), ty)
    }

    pub fn overwrite_name_and_symbol(&mut self, name: String, sym: Symbol) {
        self.base.insert(name, sym);
    }
}

impl SymbolicTable for VariableTable {
    type Item = Symbol;

    fn search_symbol(&self, name: &String) -> Option<Symbol> {
        self.base.get(name).map(|sym| sym.clone())
    }

    fn register(&mut self, id: ASTIdentifier) -> Symbol {
        let name = id.get_name();
        let sym = self.variable_symbol(name.as_str(), id.get_type());
        self.base.insert(name, sym.clone());
        sym
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
}

impl SymbolicTable for FunctionTable {
    type Item = Symbol;

    fn search_symbol(&self, name: &String) -> Option<Symbol> {
        self.base.get(name).map(|sym| sym.clone())
    }

    fn register(&mut self, id: ASTIdentifier) -> Symbol {
        let sym = Symbol::from_identifier(id.clone());
        self.base.insert(id.get_name(), sym.clone());
        sym
    }
}
