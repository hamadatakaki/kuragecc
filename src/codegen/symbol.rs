use super::code::{CodeType, Expression, ExpressionKind};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    code_type: CodeType,
}

impl Symbol {
    pub fn new(name: String) -> Self {
        Self {
            name,
            code_type: CodeType::Int,
        }
    }

    pub fn reveal(&self) -> String {
        self.name.clone()
    }

    pub fn as_func_param(&self) -> String {
        self.code_type.to_string()
    }

    pub fn to_expr(&self) -> Expression {
        let kind = ExpressionKind::Symbol(self.clone());
        Expression::new(kind, self.code_type.clone())
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
        let sym = format!("%val_{}_{}", name, self.val_count);
        Symbol::new(sym)
    }

    pub fn anonymous_symbol(&mut self) -> Symbol {
        self.ano_count += 1;
        let sym = format!("%ano_{}", self.ano_count);
        Symbol::new(sym)
    }

    pub fn get_symbol(&mut self, name: &String) -> Option<Symbol> {
        self.base.get(name).map(|sym| sym.clone())
    }

    pub fn register_name(&mut self, name: String) -> Symbol {
        let sym = self.variable_symbol(name.as_str());
        self.base.insert(name, sym.clone());
        sym
    }

    pub fn overwrite_name_and_symbol(&mut self, name: String, sym: Symbol) {
        self.base.insert(name, sym);
    }
}
