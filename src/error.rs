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
    NotEndWithSemicolon,
    AssignStartsWithIdentifier,
    AssignHasEqualOnSecondToken(String),
    StatementEndsInTheMiddle,
    ExpectedToken(String, String),
}

impl ParserErrorKind {
    fn reason(&self) -> String {
        match self {
            ParserErrorKind::NotEndWithSemicolon => {
                format!("Statement must end with `;`")
            }
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
}

pub type ParserResult<Ok> = Result<Ok, ParserError>;

pub fn visualize_parser_error(error: ParserError, source: &str) {
    println!("Parser Error: {}", error.kind.reason());
    let lines: Vec<&str> = source.split('\n').collect();
    let (from, to) = error.loc.target_area();
    let showing_lines = lines.as_slice()[from..to + 1].to_vec();
    for (index, line) in showing_lines.iter().enumerate() {
        println!("{}: {}", index + from + 1, line);
    }
}
