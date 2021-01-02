pub mod literal;

use super::Location;
use literal::{DelimiterKind, OperatorKind, ParenKind, ReservedLiteral};

#[derive(Debug, Clone)]
pub enum TokenKind {
    Reserved(ReservedLiteral),
    Integer(u32),
    Identifier(String),
    Delimiter(DelimiterKind),
    Paren(ParenKind),
    Operator(OperatorKind),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Location,
    expression: Vec<char>,
}

impl Token {
    pub fn new(kind: TokenKind, location: Location, expression: Vec<char>) -> Self {
        Self {
            kind,
            location,
            expression,
        }
    }

    // TODO: expressionではなくkindを用いた実装に帰る.
    fn to_string(&self) -> String {
        self.expression.iter().collect::<String>()
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
