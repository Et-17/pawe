use std::collections::LinkedList;
use std::fmt::Display;

use crate::{
    error_handling::{Error, ErrorType},
    parser::{ParseError, ParseErrorType},
};

#[derive(Debug, PartialEq)]
pub enum Defineable {
    Language(String),
    Feature(String),
    Parameter(String),
    ParameterVariant(String, String),
    Character(String),
    Diacritic(char),
    Evolution(String, String),
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

#[derive(Debug, PartialEq)]
pub enum CompileErrorType {
    Undefined(Defineable),
    Redefinition(Defineable),

    PassThrough(ParseErrorType),
}

pub type CompileError = Error<CompileErrorType>;

impl ErrorType for CompileErrorType {
    fn module(&self) -> String {
        String::from("compiler")
    }
}

impl Display for CompileErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Undefined(def) => write!(f, "could not find {def}"),
            Self::Redefinition(def) => write!(f, "{def} is already defined"),

            Self::PassThrough(error) => Display::fmt(error, f),
        }
    }
}

impl From<ParseError> for CompileError {
    fn from(value: ParseError) -> Self {
        CompileError {
            pos: value.pos,
            module: value.module,
            internal: Box::new(CompileErrorType::PassThrough(*value.internal)),
        }
    }
}

pub struct Logger {
    // We are using a linked list because we only really need to iterate it once
    // but we're going to be adding to it a lot, so the guarantee against large
    // memory allocations and movements in a hot loop is good.
    #[allow(clippy::linkedlist)]
    pub errors: LinkedList<CompileError>,
}

impl Logger {
    pub fn new() -> Self {
        Self {
            errors: LinkedList::new(),
        }
    }

    // Emit a single error
    pub fn emit<E: Into<CompileError>>(&mut self, error: E) {
        self.errors.push_back(error.into());
    }

    // Emit an IntoIter of Errors
    pub fn emit_i<E: Into<CompileError>, I: IntoIterator<Item = E>>(&mut self, errors: I) {
        for error in errors {
            self.emit(error);
        }
    }

    // If a given Result is Err, emit the error. This will destroy whatever is
    // in an Ok.
    pub fn emit_r<E: Into<CompileError>, T>(&mut self, result: Result<T, E>) {
        if let Err(error) = result {
            self.emit(error);
        }
    }

    // Filters the Errs from an IntoIter<Result> and returns an iterator over
    // the Ok values
    pub fn extract_ok<E: Into<CompileError>, T, I: IntoIterator<Item = Result<T, E>>>(
        &mut self,
        iter: I,
    ) -> impl Iterator<Item = T> {
        iter.into_iter().filter_map(|result| match result {
            Ok(value) => Some(value),
            Err(error) => {
                self.emit(error);
                None
            }
        })
    }
}
