use super::super::ast::ASTIdentifier;
use super::super::Location;
use super::{CompileTimeError, HasReason, VisualizeError};

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug, Clone)]
pub enum ParserErrorKind {
    NotEndWithSemicolon,
    AssignStartsWithIdentifier,
    AssignHasEqualOnSecondToken(ASTIdentifier),
    StatementEndsInTheMiddle,
    ExpectedToken(String, String), // actual, expected
}

impl HasReason for ParserErrorKind {
    fn reason(&self) -> String {
        match self {
            ParserErrorKind::NotEndWithSemicolon => format!("Statement must end with `;`"),
            ParserErrorKind::AssignStartsWithIdentifier => {
                format!("Assign must start with Identifier.")
            }
            ParserErrorKind::AssignHasEqualOnSecondToken(id) => {
                format!("Assign must have `=` next to Identifier `{}`.", id)
            }
            ParserErrorKind::StatementEndsInTheMiddle => {
                format!("End of File appeared in the middle of analyzing statement.")
            }
            ParserErrorKind::ExpectedToken(actual, expected) => {
                format!(
                    "Expected token is `{}`, actually the token is `{}`.",
                    expected, actual
                )
            }
        }
    }
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

    pub fn expected_token(actual: String, expected: String, loc: Location) -> Self {
        let kind = ParserErrorKind::ExpectedToken(actual, expected);
        ParserError::new(kind, loc)
    }
}

impl CompileTimeError for ParserError {
    type ErrorKind = ParserErrorKind;

    fn error_kind(&self) -> ParserErrorKind {
        self.kind.clone()
    }

    fn location(&self) -> Location {
        self.loc
    }

    fn reason(&self) -> String {
        format!("Parser Error: {}", self.error_kind().reason())
    }
}

impl VisualizeError for ParserError {}
