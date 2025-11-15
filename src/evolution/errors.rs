use std::fmt::Display;

use crate::config::Label;
use crate::error_handling::{Error, ErrorType, Origin};

#[derive(Debug)]
pub enum EvolutionErrorType {
    UndefinedLanguage(String),
    NoConnection(Label, Label),
}

impl ErrorType for EvolutionErrorType {}

impl Display for EvolutionErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UndefinedLanguage(lang) => write!(f, "Could not find language `{lang}`"),
            Self::NoConnection(a, b) => write!(
                f,
                "Could not find a connection between languages `{a}` and `{b}`"
            ),
        }
    }
}

impl PartialEq for EvolutionErrorType {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

impl EvolutionErrorType {
    pub fn by(self, module: &str) -> Error {
        Error {
            pos: Origin::Module(module.into()),
            error: Box::new(self),
        }
    }
}

pub type EResult<T> = std::result::Result<T, Error>;
