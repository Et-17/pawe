use std::fmt::{Debug, Display};

use crate::error_handling::{Error, ErrorType, FilePosition};

use crate::lexer::{RawToken, Token};

#[derive(Debug)]
pub enum Defineable {
    Language(String),
    Feature(String),
    Parameter(String),
    ParameterVariant(String, String),
    Character(String),
    Diacritic(char),
    Evolution(String, String),
}

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

impl Display for Defineable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Language(lang) => write!(f, "language `{lang}`"),
            Self::Feature(feat) => write!(f, "feature `{feat}`"),
            Self::Parameter(param) => write!(f, "parameter `{param}`"),
            Self::ParameterVariant(param, var) => {
                write!(f, "variant `{var}` in parameter `{param}`")
            }
            Self::Character(character) => write!(f, "character `{character}`"),
            Self::Diacritic(dia) => write!(f, "diacritic `◌{dia}`"),
            Self::Evolution(start, end) => write!(f, "evolution from `{start}` to `{end}`"),
        }
    }
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
pub enum CompileErrorType {
    Unexpected(Option<RawToken>, Expectation),
    Undefined(Defineable),
    Redefinition(Defineable),

    NegativeParameterInPhoneme,
    InvalidSpecialAtom(RawToken),
    MisplacedWordBoundary,
    ExcessTargets,
}

impl ErrorType for CompileErrorType {
    fn module(&self) -> String {
        String::from("compiler")
    }
}

impl Display for CompileErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unexpected(found, expected) => match found {
                Some(token) => write!(f, "expected {expected}, found {token}"),
                None => write!(f, "expected {expected}"),
            },
            Self::Undefined(def) => write!(f, "could not find {def}"),
            Self::Redefinition(def) => write!(f, "{def} is already defined"),

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
    O::from(CompileErrorType::Unexpected(Some(found.token), expected.into()).at(found.pos))
}

pub fn eof_error<T: Into<Expectation>, O: From<Error>>(pos: FilePosition, expected: T) -> O {
    O::from(CompileErrorType::Unexpected(None, expected.into()).at(pos))
}
