use super::super::token::literal::{PrimitiveType, TerminalSymbol};

#[derive(Debug, Clone, PartialEq)]
pub struct CustomType {
    name: String,
}

#[derive(Debug, Clone)]
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

    pub fn as_code(&self) -> String {
        match self {
            Type::Primitive(prime) => prime.as_code(),
            _ => unimplemented!(),
        }
    }

    fn invalid(&self) -> bool {
        match self {
            Type::InvalidTypeError => true,
            _ => false,
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        if self.invalid() || other.invalid() {
            return false;
        }

        use Type::*;

        match (self, other) {
            (&Primitive(ref p), &Primitive(ref q)) => p == q,
            (&Custom(ref c), &Custom(ref d)) => c == d,
            (&None, &None) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Type::*;

        match self {
            Primitive(primitive) => {
                write!(f, "{}", primitive.to_literal())
            }
            Custom(custom) => {
                write!(f, "{}", custom.name)
            }
            InvalidTypeError => write!(f, "<invalid-type-error-occured>"),
            None => write!(f, "<none>"),
        }
    }
}
