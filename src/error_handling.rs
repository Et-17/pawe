use std::fmt::{Debug, Display};

pub trait ErrorType: Display {}

// 0:0 means that the error doesn't have a specific location
#[derive(Copy, Clone, PartialEq)]
pub struct FilePosition {
    pub line: usize,
    pub char: usize,
}

impl Display for FilePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.char)
    }
}

impl Debug for FilePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone, PartialEq)]
pub enum Origin {
    File(FilePosition),
    Module(String),
}

impl Display for Origin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::File(fp) => write!(f, "{fp}"),
            Self::Module(module) => write!(f, "{module}"),
        }
    }
}

// #[derive(PartialEq)]
pub struct Error {
    pub pos: Origin,
    pub error: Box<dyn ErrorType>,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\x1b[31;49;1m[{}]\x1b[39;49;1m  {}\x1b[0m",
            self.pos, self.error
        )
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

// This lets us use ? when we want to fully fail on single errors in a function
// that returns a vector of errors, especially in parsing
impl From<Error> for Vec<Error> {
    fn from(value: Error) -> Self {
        let mut errors = Vec::with_capacity(1);
        errors.push(value);
        return errors;
    }
}
