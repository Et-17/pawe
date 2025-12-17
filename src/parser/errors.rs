use std::fmt::{Debug, Display};

use crate::error_handling::{Error, ErrorType, FilePosition};

use crate::lexer::{RawToken, Token};

#[derive(Debug)]
pub enum Expectation {
    Attribute,
    DefinitionKeyword,
    OutputAtom,
    EnvironmentAtom,
    InputAtom,
    WordAtom,
    Identifier,
    Token(RawToken),
}

impl Display for Expectation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Attribute => write!(f, "attribute"),
            Self::DefinitionKeyword => write!(f, "definition keyword"),
            Self::OutputAtom => write!(f, "output atom"),
            Self::EnvironmentAtom => write!(f, "environment atom"),
            Self::InputAtom => write!(f, "input atom"),
            Self::WordAtom => write!(f, "word atom"),
            Self::Identifier => write!(f, "identifier"),
            Self::Token(token) => write!(f, "{token}"),
        }
    }
}

impl From<RawToken> for Expectation {
    fn from(value: RawToken) -> Self {
        Self::Token(value)
    }
}

#[derive(Debug)]
pub enum ParseErrorType {
    Unexpected(RawToken, Expectation),
    Eof(Expectation),

    NegativeParameterInPhoneme,
    InvalidSpecialAtom(RawToken),
    MisplacedWordBoundary,
    ExcessTargets,
}

impl ErrorType for ParseErrorType {
    fn module(&self) -> String {
        String::from("parser")
    }
}

impl Display for ParseErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unexpected(found, expected) => write!(f, "expected {expected}, found {found}"),
            Self::Eof(expected) => write!(f, "expected {expected}, found end-of-file"),

            Self::NegativeParameterInPhoneme => {
                write!(f, "negative parameters can not be used in a phoneme")
            }
            Self::InvalidSpecialAtom(atom) => {
                write!(f, "{atom} must be preceeded by an environment atom")
            }
            Self::MisplacedWordBoundary => write!(
                f,
                "word boundaries may only be used at the start and end of the environment"
            ),
            Self::ExcessTargets => write!(f, "found multiple targets in environment"),
        }
    }
}

pub fn unexpect<T: Into<Expectation>, O: From<Error>>(found: Token, expected: T) -> O {
    ParseErrorType::Unexpected(found.token, expected.into()).at(found.pos)
}

pub fn eof<T: Into<Expectation>, O: From<Error>>(pos: FilePosition, expected: T) -> O {
    ParseErrorType::Eof(expected.into()).at(pos)
}
