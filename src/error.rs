use super::Position;

#[derive(Debug, Clone)]
pub enum LexerErrorKind {
    InvalidCharError(char),
}

#[derive(Debug, Clone)]
pub struct LexerError {
    kind: LexerErrorKind,
    pos: Position,
}

impl LexerError {
    pub fn new(kind: LexerErrorKind, pos: Position) -> Self {
        Self { kind, pos }
    }

    pub fn invalid_char(c: char, pos: Position) -> Self {
        let kind = LexerErrorKind::InvalidCharError(c);
        Self::new(kind, pos)
    }
}
