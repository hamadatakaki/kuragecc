pub mod ast;
pub mod codegen;
pub mod error;
pub mod identifier;
pub mod lexer;
pub mod parser;
pub mod semantics;
pub mod token;

trait Inspector {
    type Yielded;

    fn forward(&mut self);
    fn at_end(&self) -> bool;
    fn look_at(&self) -> Option<Self::Yielded>;
    fn look_next(&self) -> Option<Self::Yielded>;
    fn look_prev(&self) -> Option<Self::Yielded>;
    fn look_and_forward(&mut self) -> Option<Self::Yielded>;
    fn look_head(&self) -> Option<Self::Yielded>;
    fn look_tail(&self) -> Option<Self::Yielded>;
}

#[derive(Debug, Clone, Copy)]
pub struct Position {
    line: usize,
    offset: usize,
}

impl Position {
    fn new(line: usize, offset: usize) -> Self {
        Self { line, offset }
    }

    pub fn initialize() -> Self {
        Self::new(0, 0)
    }

    pub fn forward(&mut self) {
        self.offset += 1;
    }

    pub fn indention(&mut self) {
        self.line += 1;
        self.offset = 0;
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}: {}", self.line, self.offset)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    start: Position,
    end: Position,
}

impl Location {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn extend_to(&self, right: Location) -> Location {
        Location::new(self.start.clone(), right.end.clone())
    }

    pub fn target_area(&self) -> (usize, usize) {
        (self.start.line, self.end.line)
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.start.line == self.end.line {
            write!(f, "{} -> {}", self.start, self.end.offset)
        } else {
            write!(f, "({}) -> ({})", self.start, self.end)
        }
    }
}
