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
        match self {
            TokenKind::Delimiter(_) => write!(f, "delim"),
            TokenKind::Identifier(_) => write!(f, "ident"),
            TokenKind::Integer(_) => write!(f, "integ"),
            TokenKind::Operator(_) => write!(f, "opera"),
            TokenKind::Paren(_) => write!(f, "paren"),
            TokenKind::Reserved(_) => write!(f, "reser"),
            TokenKind::Primitive(_) => write!(f, "prime"),
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
        match self.kind.clone() {
            TokenKind::Delimiter(del_kind) => format!("{}", del_kind.to_literal()),
            TokenKind::Identifier(name) => name,
            TokenKind::Integer(n) => format!("{}", n),
            TokenKind::Operator(ope_kind) => format!("{}", ope_kind.to_literal()),
            TokenKind::Paren(paren_kind) => format!("{}", paren_kind.to_literal()),
            TokenKind::Reserved(reserved) => format!("{}", reserved.to_literal()),
            TokenKind::Primitive(primitive_type) => format!("{}", primitive_type.to_literal()),
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
