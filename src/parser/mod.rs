mod errors;
#[cfg(test)]
mod tests;

use crate::error_handling::{Error, ErrorType, FilePosition};
use crate::lexer::{RawToken, Token};
use crate::parser::errors::ParseError;
use crate::phonemes::SelectorCode;
use errors::{Expectation, eof, unexpect};

use errors::ParseErrorType::*;

trait Parse<E>: Sized {
    fn try_parse(lexer: &mut impl Iterator<Item = Token>, pos: &FilePosition) -> Result<Self, E>;
}

#[derive(Debug, PartialEq)]
struct Identifier {
    text: String,
    pos: FilePosition,
}

impl Parse<ParseError> for Identifier {
    fn try_parse(
        lexer: &mut impl Iterator<Item = Token>,
        pos: &FilePosition,
    ) -> Result<Self, ParseError> {
        let token = read_token(lexer, Expectation::Identifier, pos)?;

        let RawToken::UnmarkedIdentifier(text) = token.token else {
            return Err(unexpect(token, Expectation::Identifier));
        };

        assert!(!text.is_empty(), "empty identifier");

        Ok(Identifier {
            text,
            pos: token.pos,
        })
    }
}

#[derive(Debug, PartialEq)]
enum AttributeKind {
    Feature(bool, String),
    Parameter(bool, String, String),
    Character(String),
}

#[derive(Debug, PartialEq)]
struct Attribute {
    kind: AttributeKind,
    pos: FilePosition,
}

impl Parse<ParseError> for Attribute {
    fn try_parse(
        lexer: &mut impl Iterator<Item = Token>,
        pos: &FilePosition,
    ) -> Result<Self, ParseError> {
        let token = read_token(lexer, Expectation::Attribute, pos)?;

        let kind = match token.token {
            RawToken::MarkedFeature(mark, feat) => AttributeKind::Feature(mark, feat),
            RawToken::MarkedParameter(mark, param, variant) => {
                AttributeKind::Parameter(mark, param, variant)
            }
            RawToken::UnmarkedIdentifier(character) => AttributeKind::Character(character),
            _ => return Err(unexpect(token, Expectation::Attribute)),
        };

        Ok(Self {
            kind,
            pos: token.pos,
        })
    }
}

#[derive(Debug, PartialEq)]
enum OutputAttributeKind {
    Feature(bool, String),
    Parameter(bool, String, String),
    Character(String),
    SelectorCode(SelectorCode),
}

#[derive(Debug, PartialEq)]
struct OutputAttribute {
    kind: OutputAttributeKind,
    pos: FilePosition,
}

impl Parse<ParseError> for OutputAttribute {
    fn try_parse(
        lexer: &mut impl Iterator<Item = Token>,
        pos: &FilePosition,
    ) -> Result<Self, ParseError> {
        let token = read_token(lexer, Expectation::Attribute, pos)?;

        let kind = match token.token {
            RawToken::MarkedFeature(mark, feat) => OutputAttributeKind::Feature(mark, feat),
            RawToken::MarkedParameter(mark, param, variant) => {
                OutputAttributeKind::Parameter(mark, param, variant)
            }
            RawToken::UnmarkedIdentifier(character) => OutputAttributeKind::Character(character),
            RawToken::SelectorCode(code) => OutputAttributeKind::SelectorCode(code),
            _ => return Err(unexpect(token, Expectation::Attribute)),
        };

        Ok(Self {
            kind,
            pos: token.pos,
        })
    }
}

struct Phoneme {
    attributes: Vec<Result<Attribute, ParseError>>,
    pos: FilePosition,
}

struct Filter {
    attributes: Vec<Attribute>,
    pos: FilePosition,
}

struct Selector {
    attributes: Vec<Attribute>,
    code: SelectorCode,
    pos: FilePosition,
}

struct OutputPhoneme {
    attributes: Vec<Result<Attribute, ParseError>>,
}

enum InputAtom {
    Phoneme(Phoneme),
    Filter(Filter),
    Selector(Selector),
}

enum EnvironmentAtom {
    Phoneme(Phoneme),
    Filter(Filter),
    Optional(Box<EnvironmentAtom>),
    ZeroOrMore(Box<EnvironmentAtom>),
    Not(Box<EnvironmentAtom>),
}

struct Environment {
    start: bool,
    end: bool,
    pre: Vec<EnvironmentAtom>,
    post: Vec<EnvironmentAtom>,
}

struct Rule {
    inputs: Vec<InputAtom>,
    outputs: Vec<Phoneme>,
    env: Environment,
}

#[derive(Debug, PartialEq)]
struct Diacritic {
    char: char,
    pos: FilePosition,
}

impl Parse<ParseError> for Diacritic {
    fn try_parse(
        lexer: &mut impl Iterator<Item = Token>,
        pos: &FilePosition,
    ) -> Result<Self, ParseError> {
        let identifier = Identifier::try_parse(lexer, pos)?;

        let char = identifier.text.chars().last().expect("empty identifier");

        Ok(Diacritic {
            char,
            pos: identifier.pos,
        })
    }
}

#[derive(Debug, PartialEq)]
struct DiacriticDefinition {
    diacritic: Diacritic,
    definition: Result<Attribute, ParseError>,
    excess_tokens: Vec<ParseError>,
}

impl Parse<Vec<ParseError>> for DiacriticDefinition {
    fn try_parse(
        lexer: &mut impl Iterator<Item = Token>,
        pos: &FilePosition,
    ) -> Result<Self, Vec<ParseError>> {
        let diacritic = match Diacritic::try_parse(lexer, pos) {
            Ok(dia) => dia,
            Err(e) => {
                let mut errors = vec![e];
                errors.extend(end_line(lexer));
                return Err(errors);
            }
        };

        let definition = Attribute::try_parse(lexer, &diacritic.pos);

        let excess_tokens = end_line(lexer).collect();

        Ok(DiacriticDefinition {
            diacritic,
            definition,
            excess_tokens,
        })
    }
}

enum DefinitionBlock {
    Languages(Vec<Identifier>),
    Parameters(Vec<(Identifier, Vec<Identifier>)>),
    Features(Vec<Identifier>),
    Characters(Vec<(Identifier, Phoneme)>),
    Diacritics(Vec<(Diacritic, Attribute)>),
    Evolve(Identifier, Identifier, Vec<Rule>),
}

struct Configuration {
    definitions: Vec<DefinitionBlock>,
}

fn end_line(lexer: &mut impl Iterator<Item = Token>) -> impl Iterator<Item = ParseError> {
    lexer
        .take_while(|token| token.token != RawToken::Eol)
        .map(|token| unexpect(token, RawToken::Eol))
}

fn read_token<T, E: Into<Expectation>>(
    lexer: &mut impl Iterator<Item = T>,
    expectation: E,
    pos: &FilePosition,
) -> Result<T, ParseError> {
    lexer.next().ok_or_else(|| eof(pos.clone(), expectation))
}
