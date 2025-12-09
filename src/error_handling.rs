use std::fmt::{Debug, Display};
use std::path::Path;
use std::rc::Rc;

#[derive(Copy, Clone, PartialEq, Debug)]
struct InternalFilePosition {
    line: usize,
    char: Option<usize>,
}

impl Display for InternalFilePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.char {
            Some(char) => write!(f, "{}:{}", self.line, char),
            None => write!(f, "{}", self.line),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct FilePosition {
    path: Option<Rc<Path>>,
    internal: Option<InternalFilePosition>,
}

impl Display for FilePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.path {
            Some(ref path) => match self.internal {
                Some(ref internal) => write!(f, "{}:{internal}", path.display()),
                None => write!(f, "{}", path.display()),
            },
            None => match self.internal {
                Some(ref internal) => write!(f, "{internal}"),
                None => write!(f, "EMPTY=FP"),
            },
        }
    }
}

impl FilePosition {
    pub fn new(path: Option<&Rc<Path>>, line: Option<usize>, char: Option<usize>) -> Self {
        Self {
            path: path.map(Clone::clone),
            internal: line.map(|l| InternalFilePosition { line: l, char }),
        }
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
            internal: Box::new(self),
        }
    }

    fn sign(self) -> Error
    where
        Self: Sized + 'static,
    {
        Error {
            pos: None,
            module: self.module(),
            internal: Box::new(self),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pos: Option<FilePosition>,
    module: String,
    internal: Box<dyn ErrorType>,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\x1b[31;1m[{} error]\x1b[0m", self.module)?;
        if let Some(ref pos) = self.pos {
            write!(f, " {pos}")?;
        }
        write!(f, ": {}", self.internal)
    }
}

// This lets us use ? when we want to fully fail on single errors in a function
// that returns a vector of errors, especially in parsing
impl From<Error> for Vec<Error> {
    fn from(value: Error) -> Self {
        vec![value]
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

// For wrapping IO errors, so they can be handled in a unified manner
#[derive(Debug)]
struct IOError(std::io::Error);

impl ErrorType for IOError {
    fn module(&self) -> String {
        String::new()
    }
}

impl Display for IOError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// Returns a closure that wraps IO errors as IOError
pub fn wrap_io_error(
    module: impl ToString,
    pos: Option<&FilePosition>,
) -> impl Fn(std::io::Error) -> Error {
    move |error: std::io::Error| Error {
        pos: pos.cloned(),
        module: module.to_string(),
        internal: Box::new(IOError(error)),
    }
}

pub type Result<T> = std::result::Result<T, Error>;
pub type ResultV<T> = std::result::Result<T, Vec<Error>>;
