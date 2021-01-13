use super::super::Location;
use super::{CompileTimeError, HasReason, VisualizeError};

pub type SemanticErrors = Vec<SemanticError>;
pub type SemanticResult<T> = Result<T, SemanticErrors>;

#[derive(Debug, Clone)]
pub enum SemanticErrorKind {
    BlockMustEndAtFirstReturn,
    IdentifierIsNotDeclared(String),
    FunctionIsNotDefined(String),
    DifferentNumbersArgsTaken(String, usize, usize),
}

impl HasReason for SemanticErrorKind {
    fn reason(&self) -> String {
        match self {
            SemanticErrorKind::BlockMustEndAtFirstReturn => {
                format!("Block must end at 1st return.")
            }
            SemanticErrorKind::IdentifierIsNotDeclared(name) => {
                format!("Identifier `{}` is not declared.", name)
            }
            SemanticErrorKind::FunctionIsNotDefined(name) => {
                format!("Function `{}` is not defined.", name)
            }
            SemanticErrorKind::DifferentNumbersArgsTaken(name, actual, expected) => {
                let s_actual = if *actual == 1 { "" } else { "s" };
                let s_expected = if *expected == 1 { "" } else { "s" };
                format!(
                    "Function `{}` expected to be taken {} arg{}, but given {} arg{}.",
                    name, expected, s_expected, actual, s_actual
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticError {
    kind: SemanticErrorKind,
    loc: Location,
}

impl SemanticError {
    pub fn new(kind: SemanticErrorKind, loc: Location) -> Self {
        Self { kind, loc }
    }
}

impl CompileTimeError for SemanticError {
    type ErrorKind = SemanticErrorKind;

    fn error_kind(&self) -> SemanticErrorKind {
        self.kind.clone()
    }

    fn location(&self) -> Location {
        self.loc
    }

    fn reason(&self) -> String {
        format!("Semantics Error: {}", self.error_kind().reason())
    }
}

impl VisualizeError for SemanticError {}
