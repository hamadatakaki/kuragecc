pub mod literal;

use super::Location;
use literal::{
    DelimiterKind, OperatorKind, ParenKind, PrimitiveType, ReservedLiteral, TerminalSymbol,
};

#[derive(Debug, Clone)]
pub enum TokenKind {
    Delimiter(DelimiterKind),
    Identifier(String),
    Integer(u32),
    Operator(OperatorKind),
    Paren(ParenKind),
    Reserved(ReservedLiteral),
    Primitive(PrimitiveType),
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use TokenKind::*;

        match self {
            Delimiter(_) => write!(f, "delim"),
            Identifier(_) => write!(f, "ident"),
            Integer(_) => write!(f, "integ"),
            Operator(_) => write!(f, "opera"),
            Paren(_) => write!(f, "paren"),
            Reserved(_) => write!(f, "reser"),
            Primitive(_) => write!(f, "prime"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Location,
}

impl Token {
    pub fn new(kind: TokenKind, location: Location) -> Self {
        Self { kind, location }
    }

    fn to_string(&self) -> String {
        use TokenKind::*;

        match self.kind.clone() {
            Delimiter(del_kind) => format!("{}", del_kind.to_literal()),
            Identifier(name) => name,
            Integer(n) => format!("{}", n),
            Operator(ope_kind) => format!("{}", ope_kind.to_literal()),
            Paren(paren_kind) => format!("{}", paren_kind.to_literal()),
            Reserved(reserved) => format!("{}", reserved.to_literal()),
            Primitive(primitive_type) => format!("{}", primitive_type.to_literal()),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if std::env::var("RUST_BACKTRACE").is_ok() {
            write!(
                f,
                "Token<`{}`: `{}`, [{}]>",
                self.kind,
                self.to_string(),
                self.location
            )
        } else {
            write!(f, "Token<`{}`>", self.to_string())
        }
    }
}
