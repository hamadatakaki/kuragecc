pub mod lexer;
pub mod parser;
pub mod semantics;

use super::Location;

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
