use super::super::token::literal::{PrimitiveType, TerminalSymbol};

#[derive(Debug, Clone, PartialEq)]
pub struct CustomType {
    name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Custom(CustomType),
    InvalidTypeError,
    None,
}

impl Type {
    pub fn int() -> Self {
        Type::Primitive(PrimitiveType::Int)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Primitive(primitive) => {
                write!(f, "{}", primitive.to_literal())
            }
            Type::Custom(custom) => {
                write!(f, "{}", custom.name)
            }
            Type::InvalidTypeError => write!(f, "<invalid-type-error-occured>"),
            Type::None => write!(f, "<none>"),
        }
    }
}
