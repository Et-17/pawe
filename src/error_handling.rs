use std::fmt::{Debug, Display};

pub trait ErrorType: Display + PartialEq {}

// 0:0 means that the error doesn't have a specific location
#[derive(Copy, Clone, PartialEq)]
pub struct Position {
    pub line: usize,
    pub char: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.char)
    }
}

impl Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(PartialEq)]
pub struct Error<T: ErrorType> {
    pub pos: Position,
    pub error: T,
}

impl<T: ErrorType> Display for Error<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\x1b[31;49;1m[{}]\x1b[39;49;1m  {}\x1b[0m",
            self.pos, self.error
        )
    }
}

impl<T: ErrorType> Debug for Error<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
