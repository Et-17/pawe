mod errors;
#[cfg(test)]
mod tests;

use crate::error_handling::{ErrorType, FilePosition};
use crate::lexer::{RawToken, Token};
use crate::parser::errors::ParseError;
use crate::phonemes::SelectorCode;
use errors::{Expectation, eof, unexpect};

use errors::ParseErrorType::*;
use itertools::Itertools;

trait Parse<E>: Sized {
    fn try_parse(lexer: &mut impl Iterator<Item = Token>, pos: &FilePosition) -> Result<Self, E>;

    fn parse_iter(lexer: &mut impl Iterator<Item = Token>) -> Vec<Result<Self, E>> {
        let mut peekable = lexer.peekable();
        let mut results = Vec::new();

        while peekable.peek().is_some() {
            results.push(Self::try_parse(&mut peekable, &FilePosition::default()));
        }

        results
    }
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
enum FilterAttributeKind {
    Feature(bool, String),
    Parameter(bool, String, String),
    Character(String),
}

impl TryFrom<Token> for FilterAttributeKind {
    type Error = ParseError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.token {
            RawToken::MarkedFeature(mark, feat) => Ok(Self::Feature(mark, feat)),
            RawToken::MarkedParameter(mark, param, variant) => {
                Ok(Self::Parameter(mark, param, variant))
            }
            RawToken::UnmarkedIdentifier(character) => Ok(Self::Character(character)),
            RawToken::SelectorCode(_) => Err(InvalidSelectorCode.at(value.pos)),
            _ => Err(unexpect(value, Expectation::Attribute)),
        }
    }
}

#[derive(Debug, PartialEq)]
enum PhonemeAttributeKind {
    Feature(bool, String),
    Parameter(String, String),
    Character(String),
}

impl TryFrom<Token> for PhonemeAttributeKind {
    type Error = ParseError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.token {
            RawToken::MarkedFeature(mark, feat) => Ok(Self::Feature(mark, feat)),
            RawToken::MarkedParameter(true, param, variant) => Ok(Self::Parameter(param, variant)),
            RawToken::MarkedParameter(false, _, _) => Err(InvalidNegativeParameter.at(value.pos)),
            RawToken::UnmarkedIdentifier(character) => Ok(Self::Character(character)),
            RawToken::SelectorCode(_) => Err(InvalidSelectorCode.at(value.pos)),
            _ => Err(unexpect(value, Expectation::Attribute)),
        }
    }
}

#[derive(Debug, PartialEq)]
enum OutputAttributeKind {
    Feature(bool, String),
    Parameter(String, String),
    Character(String),
    SelectorCode(SelectorCode),
}

impl TryFrom<Token> for OutputAttributeKind {
    type Error = ParseError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.token {
            RawToken::MarkedFeature(mark, feat) => Ok(Self::Feature(mark, feat)),
            RawToken::MarkedParameter(true, param, variant) => Ok(Self::Parameter(param, variant)),
            RawToken::MarkedParameter(false, _, _) => Err(InvalidNegativeParameter.at(value.pos)),
            RawToken::UnmarkedIdentifier(character) => Ok(Self::Character(character)),
            RawToken::SelectorCode(code) => Ok(Self::SelectorCode(code)),
            _ => Err(unexpect(value, Expectation::Attribute)),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Attribute<T: TryFrom<Token, Error = ParseError>> {
    kind: T,
    pos: FilePosition,
}

impl<T: TryFrom<Token, Error = ParseError>> Parse<ParseError> for Attribute<T> {
    fn try_parse(
        lexer: &mut impl Iterator<Item = Token>,
        pos: &FilePosition,
    ) -> Result<Self, ParseError> {
        let token = read_token(lexer, Expectation::Attribute, pos)?;
        let attr_pos = token.pos.clone();

        let kind = token.try_into()?;

        Ok(Self {
            kind,
            pos: attr_pos,
        })
    }
}

type FilterAttribute = Attribute<FilterAttributeKind>;
type PhonemeAttribute = Attribute<PhonemeAttributeKind>;
type OutputAttribute = Attribute<OutputAttributeKind>;

#[derive(Debug, PartialEq)]
struct Phoneme {
    attributes: Vec<Result<PhonemeAttribute, ParseError>>,
    pos: FilePosition,
}

impl Parse<ParseError> for Phoneme {
    // This assumes that the opening character has already been consumed, so
    // that the caller can decide to parse a phoneme. It will take the passed
    // pos as the pos for the returned Phoneme.
    fn try_parse(
        lexer: &mut impl Iterator<Item = Token>,
        pos: &FilePosition,
    ) -> Result<Self, ParseError> {
        let mut tokens = read_until_closed(lexer, RawToken::PhonemeClose, pos)?;

        Ok(Phoneme {
            attributes: Attribute::parse_iter(&mut tokens),
            pos: pos.clone(),
        })
    }
}

#[derive(Debug, PartialEq)]
struct Filter {
    attributes: Vec<Result<FilterAttribute, ParseError>>,
    pos: FilePosition,
}

impl Parse<ParseError> for Filter {
    fn try_parse(
        lexer: &mut impl Iterator<Item = Token>,
        pos: &FilePosition,
    ) -> Result<Self, ParseError> {
        let mut tokens = read_until_closed(lexer, RawToken::FilterSelectorClose, pos)?;

        Ok(Filter {
            attributes: Attribute::parse_iter(&mut tokens),
            pos: pos.clone(),
        })
    }
}

#[derive(Debug, PartialEq)]
struct Selector {
    attributes: Vec<Result<FilterAttribute, ParseError>>,
    code: SelectorCode,
    pos: FilePosition,
}

// Selector needs to know its code, despite not being able to see its opening
// tokens, so it can't implement Parse
impl Selector {
    fn try_parse(
        lexer: &mut impl Iterator<Item = Token>,
        pos: &FilePosition,
        code: SelectorCode,
    ) -> Result<Self, ParseError> {
        let filter = Filter::try_parse(lexer, pos)?;

        Ok(Self {
            attributes: filter.attributes,
            code,
            pos: filter.pos,
        })
    }
}

#[derive(Debug, PartialEq)]
struct OutputPhoneme {
    attributes: Vec<Result<OutputAttribute, ParseError>>,
    pos: FilePosition,
}

impl Parse<ParseError> for OutputPhoneme {
    // Unlike normal phonemes, this expects to have its opening token
    fn try_parse(
        lexer: &mut impl Iterator<Item = Token>,
        pos: &FilePosition,
    ) -> Result<Self, ParseError> {
        confirm_token(lexer, RawToken::PhonemeOpen, pos)?;

        let mut tokens = read_until_closed(lexer, RawToken::PhonemeClose, pos)?;

        Ok(OutputPhoneme {
            attributes: Attribute::parse_iter(&mut tokens),
            pos: pos.clone(),
        })
    }
}

#[derive(Debug, PartialEq)]
enum InputAtom {
    Phoneme(Result<Phoneme, ParseError>),
    Filter(Result<Filter, ParseError>),
    Selector(Result<Selector, ParseError>),
    Identifier(Result<Identifier, ParseError>),
}

impl Parse<ParseError> for InputAtom {
    fn try_parse(
        lexer: &mut impl Iterator<Item = Token>,
        pos: &FilePosition,
    ) -> Result<Self, ParseError> {
        let open_token = read_token(lexer, Expectation::InputAtom, pos)?;

        let atom = match open_token.token {
            RawToken::PhonemeOpen => Self::Phoneme(Phoneme::try_parse(lexer, &open_token.pos)),
            RawToken::FilterOpen => Self::Filter(Filter::try_parse(lexer, &open_token.pos)),
            RawToken::SelectorOpen(code) => {
                Self::Selector(Selector::try_parse(lexer, &open_token.pos, code))
            }
            RawToken::UnmarkedIdentifier(text) => Self::Identifier(Ok(Identifier {
                text,
                pos: open_token.pos,
            })),
            _ => return Err(unexpect(open_token, Expectation::InputAtom)),
        };

        Ok(atom)
    }
}

#[derive(Debug, PartialEq)]
enum EnvironmentAtom {
    Phoneme(Result<Phoneme, ParseError>),
    Filter(Result<Filter, ParseError>),
    Identifier(Result<Identifier, ParseError>),
    Optional(Box<Result<EnvironmentAtom, ParseError>>),
    ZeroOrMore(Box<Result<EnvironmentAtom, ParseError>>),
    Not(Box<Result<EnvironmentAtom, ParseError>>),
}

impl Parse<ParseError> for EnvironmentAtom {
    fn try_parse(
        lexer: &mut impl Iterator<Item = Token>,
        pos: &FilePosition,
    ) -> Result<Self, ParseError> {
        let open_token = read_token(lexer, Expectation::EnvironmentAtom, pos)?;

        let atom = match open_token.token {
            RawToken::PhonemeOpen => Self::Phoneme(Phoneme::try_parse(lexer, &open_token.pos)),
            RawToken::FilterOpen => Self::Filter(Filter::try_parse(lexer, &open_token.pos)),
            RawToken::UnmarkedIdentifier(text) => Self::Identifier(Ok(Identifier {
                text,
                pos: open_token.pos,
            })),
            RawToken::Optional => Self::Optional(Box::new(Self::try_parse(lexer, &open_token.pos))),
            RawToken::ZeroOrMore => {
                Self::ZeroOrMore(Box::new(Self::try_parse(lexer, &open_token.pos)))
            }
            RawToken::Not => Self::Not(Box::new(Self::try_parse(lexer, &open_token.pos))),
            RawToken::Target => return Err(ExcessTargets.at(open_token.pos)),
            RawToken::WordBoundry => return Err(MisplacedWordBoundary.at(open_token.pos)),
            _ => return Err(unexpect(open_token, Expectation::EnvironmentAtom)),
        };

        Ok(atom)
    }
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
    definition: Result<PhonemeAttribute, ParseError>,
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
    Diacritics(Vec<DiacriticDefinition>),
    Evolve(Identifier, Identifier, Vec<Rule>),
}

struct Configuration {
    definitions: Vec<DefinitionBlock>,
}

fn end_line(lexer: &mut impl Iterator<Item = Token>) -> impl Iterator<Item = ParseError> {
    read_until(lexer, &RawToken::Eol).map(|token| unexpect(token, RawToken::Eol))
}

fn read_token<T, E: Into<Expectation>>(
    lexer: &mut impl Iterator<Item = T>,
    expectation: E,
    pos: &FilePosition,
) -> Result<T, ParseError> {
    lexer.next().ok_or_else(|| eof(pos.clone(), expectation))
}

fn read_until(
    lexer: &mut impl Iterator<Item = Token>,
    token: &RawToken,
) -> impl Iterator<Item = Token> {
    lexer.take_while(|next| next.token != *token)
}

// This eagerly consumes all tokens until the end token or eof, and then
// verifies that it's present before converting the tokens back into an iter
// and returning it.
fn read_until_closed(
    lexer: &mut impl Iterator<Item = Token>,
    end: RawToken,
    pos: &FilePosition,
) -> Result<impl Iterator<Item = Token>, ParseError> {
    let mut peeking = lexer.peekable();
    let tokens = peeking
        .peeking_take_while(|next| next.token != end)
        .collect_vec();
    let last_pos = tokens.last().map_or(pos, |token| &token.pos);

    read_token(&mut peeking, end, last_pos)?;

    Ok(tokens.into_iter())
}

fn confirm_token(
    lexer: &mut impl Iterator<Item = Token>,
    token: RawToken,
    pos: &FilePosition,
) -> Result<(), ParseError> {
    match lexer.next() {
        Some(found_token) => {
            if found_token.token == token {
                Ok(())
            } else {
                Err(unexpect(found_token, token))
            }
        }
        None => Err(eof(pos.clone(), token)),
    }
}
