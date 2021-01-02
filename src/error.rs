use super::Location;

#[derive(Debug, Clone)]
pub enum LexerErrorKind {
    InvalidCharError(char),
}

#[derive(Debug, Clone)]
pub struct LexerError {
    kind: LexerErrorKind,
    loc: Location,
}

impl LexerError {
    pub fn new(kind: LexerErrorKind, loc: Location) -> Self {
        Self { kind, loc }
    }

    pub fn invalid_char(c: char, loc: Location) -> Self {
        let kind = LexerErrorKind::InvalidCharError(c);
        Self::new(kind, loc)
    }
}

#[derive(Debug, Clone)]
pub enum ParserErrorKind {
    BlockDoesNotEndAtFirstReturn,
}

#[derive(Debug, Clone)]
pub struct ParserError {
    kind: ParserErrorKind,
    loc: Location,
}

impl ParserError {
    pub fn new(kind: ParserErrorKind, loc: Location) -> Self {
        Self { kind, loc }
    }
}
