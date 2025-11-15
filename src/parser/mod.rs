pub mod lexer;
pub mod parser;

use std::fmt::Display;

use crate::error_handling::{Error, ErrorType, Position};

use lexer::RawToken;

#[derive(Debug)]
pub enum ParseErrorType {
    ExpectedBlockIdentifier,
    ExpectedBlock,
    ExpectedIdentifier, // empty line in languages or features block
    ExpectedEOL,
    ExpectedPhoneme,
    ExpectedLanguage,
    ExpectedTo,
    MissingTarget,

    UndefinedFeature(String),
    UndefinedParameter(String),
    UndefinedParameterVariant(String, String),
    UndefinedCharacter(String),
    UndefinedLanguage(String),

    Redefinition, // attempted to redefine something
    AlreadyDefinedEvolution(String, String),
    NegativeParameterInPhoneme,
    MisplacedWordBoundary,
    MisplacedOptional,
    MisplacedZeroOrMore,
    MisplacedNot,
    MultipleTargets,
    UnexpectedToken(RawToken),

    FileError(std::io::Error),
}

impl ErrorType for ParseErrorType {}

impl Display for ParseErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedBlockIdentifier => write!(f, "Expected a block identifier keyword"),
            Self::ExpectedBlock => write!(f, "Expected a block"),
            Self::ExpectedIdentifier => write!(f, "Expected identifier"),
            Self::ExpectedEOL => write!(f, "Expected end-of-line"),
            Self::ExpectedPhoneme => write!(f, "Expected a phoneme"),
            Self::ExpectedLanguage => write!(f, "Expected a language name"),
            Self::ExpectedTo => write!(f, "Expected `to`"),
            Self::MissingTarget => write!(f, "There is no target in the environment"),

            Self::UndefinedFeature(feat) => write!(f, "Could not find feature `{}`", feat),
            Self::UndefinedParameter(param) => write!(f, "Could not find parameter `{}`", param),
            Self::UndefinedParameterVariant(param, var) => write!(
                f,
                "Could not find variant `{}` in parameter `{}`",
                var, param
            ),
            Self::UndefinedCharacter(c) => write!(f, "Could not find character `{}`", c),
            Self::UndefinedLanguage(lang) => write!(f, "Could not find language `{}`", lang),

            Self::Redefinition => write!(f, "Attempted to redefine something"),
            Self::AlreadyDefinedEvolution(input, output) => write!(
                f,
                "An evolution from `{}` to `{}` has already been defined",
                input, output
            ),
            Self::NegativeParameterInPhoneme => write!(
                f,
                "Negative parameters are only allowed in filters and selectors, not phonemes"
            ),
            Self::MisplacedWordBoundary => write!(
                f,
                "You can only match word boundaries at the start or end of environments"
            ),
            Self::MisplacedOptional => {
                write!(f, "There is no matcher for this optional to be applied to")
            }
            Self::MisplacedZeroOrMore => write!(
                f,
                "There is no matcher for this zero-or-more to be applied to"
            ),
            Self::MisplacedNot => write!(f, "There is no matcher for this not to be applied to"),
            Self::MultipleTargets => write!(f, "There are multiple targets in the environment"),
            Self::UnexpectedToken(token) => write!(f, "Unexpected token {:?}", token),

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
