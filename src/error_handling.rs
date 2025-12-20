use std::fmt::{Debug, Display};
use std::path::Path;
use std::rc::Rc;

#[derive(Copy, Clone, PartialEq, Debug, Default)]
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

#[derive(Clone, PartialEq, Debug, Default)]
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

    fn at<T: From<Error<Self>>>(self, pos: FilePosition) -> T
    where
        Self: Sized + 'static,
    {
        T::from(Error::<Self> {
            pos: Some(pos),
            module: self.module(),
            internal: Box::new(self),
        })
    }

    fn sign<T: From<Error<Self>>>(self) -> T
    where
        Self: Sized + 'static,
    {
        T::from(Error::<Self> {
            pos: None,
            module: self.module(),
            internal: Box::new(self),
        })
    }
}

#[derive(Debug)]
pub struct Error<T: ErrorType + ?Sized = dyn ErrorType> {
    pub pos: Option<FilePosition>,
    module: String,
    internal: Box<T>,
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

impl<T: ErrorType + ?Sized + PartialEq> PartialEq for Error<T> {
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos && self.module == other.module && self.internal == other.internal
    }
}

// You can pass a concrete type to something that expects a dyn anywhere, but
// the compiler won't recognize we're trying to do that if it's a member of a
// struct. Instead, we have to reconstruct the struct to convert it.
impl<T: ErrorType + 'static> From<Error<T>> for Error<dyn ErrorType> {
    fn from(value: Error<T>) -> Self {
        Self {
            pos: value.pos,
            module: value.module,
            internal: value.internal,
        }
    }
}

// These let us use ? when we want to fully fail on single errors in a function
// that returns a vector of errors, especially in parsing.
// We define this separately so that we can don't need 'static and Sized if we
// aren't converting to dyn.
impl<T: ErrorType + ?Sized> From<Error<T>> for Vec<Error<T>> {
    fn from(value: Error<T>) -> Self {
        vec![value]
    }
}

impl<T: ErrorType + 'static> From<Error<T>> for Vec<Error<dyn ErrorType>> {
    fn from(value: Error<T>) -> Self {
        vec![Into::<Error<dyn ErrorType>>::into(value)]
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

pub type Result<T, E = dyn ErrorType> = std::result::Result<T, Error<E>>;
pub type ResultV<T, E = dyn ErrorType> = std::result::Result<T, Vec<Error<E>>>;
