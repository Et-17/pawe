use std::fmt::{Debug, Display};

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
        write!(f, "[{}]", self)
    }
}

pub trait ErrorType: Display + Debug {
    fn module(&self) -> String;

    fn at(self, pos: FilePosition) -> Error
    where
        Self: Sized + 'static,
    {
        Error {
            pos: Some(pos),
            module: self.module(),
            error: Box::new(self),
        }
    }

    fn sign(self) -> Error
    where
        Self: Sized + 'static,
    {
        Error {
            pos: None,
            module: self.module(),
            error: Box::new(self),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub pos: Option<FilePosition>,
    pub module: String,
    pub error: Box<dyn ErrorType>,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\x1b[31;49;1m[")?;
        if let Some(pos) = self.pos {
            write!(f, "{}", pos)?;
        } else {
            write!(f, "{}", self.module)?;
        }
        write!(f, "]\x1b[39;49;1m  {}\x1b[0m", self.error)
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

// Turns `errors` into a vector of type `E`. If it contains anything, it will
// return `Err` with that vector. If it doesn't, it will return `Ok(ok)`
pub fn check_errors<T, E, V: Into<Vec<E>>>(ok: T, errors: V) -> std::result::Result<T, Vec<E>> {
    let conv_vec = errors.into();
    if conv_vec.is_empty() {
        Ok(ok)
    } else {
        Err(conv_vec)
    }
}

pub type Result<T> = std::result::Result<T, Error>;
pub type ResultV<T> = std::result::Result<T, Vec<Error>>;
