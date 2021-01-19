use super::super::ast::types::Type;
use super::super::ast::ASTIdentifier;

#[derive(Debug, Clone)]
pub enum ValueKind {
    Int(i32),
}

#[derive(Debug, Clone)]
pub struct Value {
    pub kind: ValueKind,
    code_type: Type,
}

impl Value {
    pub fn new(kind: ValueKind) -> Self {
        Self {
            kind,
            code_type: Type::int(),
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
    code_type: Type,
}

impl Symbol {
    pub fn new(name: String) -> Self {
        Self {
            name,
            code_type: Type::int(), // TODO: CodeType を引数で指定できるようにする
        }
    }

    pub fn to_expr(&self) -> Expression {
        let kind = ExpressionKind::Symbol(self.clone());
        Expression::new(kind, self.code_type.clone())
    }

    pub fn from_identifier(id: ASTIdentifier) -> Self {
        Self::new(id.get_name())
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Value(Value),
    Symbol(Symbol),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    code_type: Type,
}

impl Expression {
    pub fn new(kind: ExpressionKind, code_type: Type) -> Self {
        Self { kind, code_type }
    }

    pub fn to_symbol(&self) -> Option<Symbol> {
        match &self.kind {
            ExpressionKind::Symbol(sym) => Some(sym.clone()),
            _ => None,
        }
    }

    pub fn as_func_arg(&self) -> String {
        format!("{} {}", self.code_type.as_code(), self.as_code())
    }
}

pub trait CodeExpression {
    fn as_code(&self) -> String;
    fn get_type(&self) -> Type;
}

impl CodeExpression for Value {
    fn as_code(&self) -> String {
        match self.kind {
            ValueKind::Int(n) => format!("{}", n),
        }
    }

    fn get_type(&self) -> Type {
        self.code_type.clone()
    }
}

impl CodeExpression for Symbol {
    fn as_code(&self) -> String {
        self.name.clone()
    }

    fn get_type(&self) -> Type {
        self.code_type.clone()
    }
}

impl CodeExpression for Expression {
    fn as_code(&self) -> String {
        match &self.kind {
            ExpressionKind::Value(kind) => kind.as_code(),
            ExpressionKind::Symbol(sym) => sym.as_code(),
        }
    }

    fn get_type(&self) -> Type {
        self.code_type.clone()
    }
}
