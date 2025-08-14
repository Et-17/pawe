pub mod lexer;
pub mod parser;

use std::fmt::Display;

use crate::error_handling::{Error, ErrorType, Position};

use lexer::Token;

#[derive(Debug)]
pub enum ParseErrorType {
    ExpectedBlockIdentifier,
    ExpectedBlock,
    UnexpectedBlock,
    ExpectedIdentifier, // empty line in languages or features block
    ExpectedEOL,
    UnexpectedEOF,
    // In case we need to note that a token has not been explicitly handled yet
    // but it should be. This is an issue with PAWE, not the code
    UnhandledToken(Token),
    FileError(std::io::Error),
}

impl ErrorType for ParseErrorType {}

impl Display for ParseErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedBlockIdentifier => write!(f, "Expected a block identifier keyword"),
            Self::ExpectedBlock => write!(f, "Expected a block"),
            Self::UnexpectedBlock => write!(f, "Unexpected block"),
            Self::ExpectedIdentifier => write!(f, "Expected identifier"),
            Self::ExpectedEOL => write!(f, "Expected end-of-line"),
            Self::UnexpectedEOF => write!(f, "File ended unexpectedly"),
            Self::UnhandledToken(token) => write!(f, "Unhandled token: {:?}", token),
            Self::FileError(e) => write!(f, "File error: {}", e),
        }
    }
}

impl ParseErrorType {
    fn at(self, pos: Position) -> Error<ParseErrorType> {
        Error { pos, error: self }
    }
}

impl PartialEq for ParseErrorType {
    fn eq(&self, other: &Self) -> bool {
        return std::mem::discriminant(self) == std::mem::discriminant(other);
    }
}

pub type PResult<T> = std::result::Result<T, Error<ParseErrorType>>;
pub type PResultV<T> = std::result::Result<T, Vec<Error<ParseErrorType>>>;
