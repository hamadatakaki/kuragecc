use super::Location;

pub type LexerErrors = Vec<LexerError>;
pub type SemanticErrors = Vec<SemanticError>;
pub type LexerResult<T> = Result<T, LexerErrors>;
pub type ParserResult<T> = Result<T, ParserError>;
pub type SemanticResult<T> = Result<T, SemanticErrors>;

pub trait HasReason {
    fn reason(&self) -> String;
}

pub trait CompileTimeError {
    type ErrorKind: HasReason;

    fn error_kind(&self) -> Self::ErrorKind;
    fn location(&self) -> Location;
    fn reason(&self) -> String;
}

pub trait VisualizeError: CompileTimeError {
    fn visualize_error(&self, source: &str) {
        println!("{}", self.reason());
        let lines: Vec<&str> = source.split('\n').collect();
        let (from, to) = self.location().target_area();
        let showing_lines = lines.as_slice()[from..to + 1].to_vec();
        for (index, line) in showing_lines.iter().enumerate() {
            println!("{}: {}", index + from + 1, line);
        }
    }
}

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

#[derive(Debug, Clone)]
pub enum ParserErrorKind {
    NotEndWithSemicolon,
    AssignStartsWithIdentifier,
    AssignHasEqualOnSecondToken(String),
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

#[derive(Debug, Clone)]
pub enum SemanticErrorKind {
    BlockMustEndAtFirstReturn,
    IdentifierIsNotDeclared(String),
    FunctionIsNotDefined(String),
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
