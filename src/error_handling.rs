use std::fmt::Display;

pub trait ErrorType: Display + PartialEq {}

#[derive(Debug, PartialEq)]
pub struct Location {
    // In the future this will contain file information too, but I'm just doing
    // this for now until I implement multi-file stuff
    pub line: usize // line 0 means the error doesn't have a specific line
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.line > 0 {
            write!(f, "{}", self.line)
        } else {
            write!(f, "")
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Error<T: ErrorType> {
    pub location: Location,
    pub error: T
}

impl<T: ErrorType> Display for Error<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\x1b[31;49;1m[{}]\x1b[39;49;1m  {}\x1b[0m", self.location, self.error)
    }
}
