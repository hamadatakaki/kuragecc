pub mod literal;

use super::Location;
use literal::{DelimiterKind, OperatorKind, ParenKind, ReservedLiteral, TerminalSymbol};

#[derive(Debug, Clone)]
pub enum TokenKind {
    Delimiter(DelimiterKind),
    Identifier(String),
    Integer(u32),
    Operator(OperatorKind),
    Paren(ParenKind),
    Reserved(ReservedLiteral),
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
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if std::env::var("RUST_BACKTRACE").is_ok() {
            write!(f, "Token<`{}`, [{}]>", self.to_string(), self.location)
        } else {
            write!(f, "Token<`{}`>", self.to_string())
        }
    }
}
