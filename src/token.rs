pub mod literal;

use super::Position;
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
    pub pos: Position,
    expression: Vec<char>,
}

impl Token {
    pub fn new(kind: TokenKind, expression: Vec<char>, pos: Position) -> Self {
        Self {
            kind,
            pos,
            expression,
        }
    }

    fn to_string(&self) -> String {
        self.expression.iter().collect::<String>()
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if std::env::var("RUST_BACKTRACE").is_ok() {
            write!(f, "Token<`{}`, {:?}>", self.to_string(), self.pos)
        } else {
            write!(f, "Token<`{}`>", self.to_string())
        }
    }
}
