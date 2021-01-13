use super::super::Location;
use super::{CompileTimeError, HasReason, VisualizeError};

pub type LexerErrors = Vec<LexerError>;
pub type LexerResult<T> = Result<T, LexerErrors>;

#[derive(Debug, Clone)]
pub enum LexerErrorKind {
    InvalidCharError(char),
}

impl HasReason for LexerErrorKind {
    fn reason(&self) -> String {
        match self {
            LexerErrorKind::InvalidCharError(c) => format!("Invalid character is discovered {}", c),
        }
    }
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

impl CompileTimeError for LexerError {
    type ErrorKind = LexerErrorKind;

    fn error_kind(&self) -> LexerErrorKind {
        self.kind.clone()
    }

    fn location(&self) -> Location {
        self.loc
    }

    fn reason(&self) -> String {
        format!("Lexer Error: {}", self.error_kind().reason())
    }
}

impl VisualizeError for LexerError {}
