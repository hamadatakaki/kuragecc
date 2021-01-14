use super::code::CodeType;

#[derive(Debug, Clone)]
pub enum ValueKind {
    Int(i32),
}

#[derive(Debug, Clone)]
pub struct Value {
    pub kind: ValueKind,
    code_type: CodeType,
}

impl Value {
    pub fn new(kind: ValueKind) -> Self {
        Self {
            kind,
            code_type: CodeType::Int,
        }
    }

    pub fn to_string(&self) -> String {
        match self.kind {
            ValueKind::Int(n) => format!("{}", n),
        }
    }

    pub fn to_expr(&self) -> Expression {
        let kind = ExpressionKind::Value(self.clone());
        Expression::new(kind, self.code_type.clone())
    }
}

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
pub enum ExpressionKind {
    Value(Value),
    Symbol(Symbol),
}

impl ExpressionKind {
    pub fn to_string(&self) -> String {
        match self {
            ExpressionKind::Value(kind) => kind.to_string(),
            ExpressionKind::Symbol(sym) => sym.reveal(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    code_type: CodeType,
}

impl Expression {
    pub fn new(kind: ExpressionKind, code_type: CodeType) -> Self {
        Self { kind, code_type }
    }

    pub fn to_symbol(&self) -> Option<Symbol> {
        match &self.kind {
            ExpressionKind::Symbol(sym) => Some(sym.clone()),
            _ => None,
        }
    }

    pub fn to_string(&self) -> String {
        self.kind.to_string()
    }

    pub fn as_func_arg(&self) -> String {
        format!("{} {}", self.code_type.to_string(), self.to_string())
    }
}