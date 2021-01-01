pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod token;

trait Inspector {
    type Yielded;

    fn forward(&mut self);
    fn backward(&mut self);
    fn at_end(&self) -> bool;
    fn look_at(&self) -> Self::Yielded;
    fn look_next(&self) -> Self::Yielded;
    fn look_prev(&self) -> Self::Yielded;
    fn look_and_forward(&mut self) -> Self::Yielded;
}

trait Place {
    fn extend_to(&self, right: Self) -> Self;
}

type Position = (usize, usize);

impl Place for Position {
    fn extend_to(&self, right: Position) -> Position {
        (self.0, right.0 - self.0 + right.1)
    }
}