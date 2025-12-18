mod errors;

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

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use std::fmt::Debug;

    use super::*;
    use crate::lexer::Lexer;

    fn lex_str(input: &'static str) -> impl Iterator<Item = Token> {
        Lexer::lex(std::io::Cursor::new(input), None).map(Result::unwrap)
    }

    fn str_positions(input: &'static str) -> impl Iterator<Item = FilePosition> {
        lex_str(input).map(|token| token.pos)
    }

    fn test_good_try_parse<E: Debug, T: Parse<E> + Debug + PartialEq>(
        input: &'static str,
        expected: &T,
    ) {
        let actual = T::try_parse(&mut lex_str(input), &FilePosition::default()).unwrap();

        assert_eq!(actual, *expected);
    }

    #[test]
    fn end_line() {
        let input = "alpha bravo; charlie delta";
        let no_end_input = "alpha bravo";

        let expected: Vec<Error> = lex_str(no_end_input)
            .map(|token| unexpect(token, Expectation::Token(RawToken::Eol)))
            .collect_vec();
        let expected_tail = lex_str(input).skip(3).collect_vec();

        let lexer = &mut lex_str(input);
        let actual = super::end_line(lexer).collect_vec();
        let actual_tail = lexer.collect_vec();

        assert!(actual.len() == expected.len());
        assert_eq!(actual_tail, expected_tail);
    }

    #[test]
    fn eof() {
        assert!(
            read_token(
                &mut lex_str(""),
                Expectation::Identifier,
                &FilePosition::default()
            )
            .is_err()
        );
    }

    #[test]
    fn identifier() {
        let input = "alpha";
        let expected_pos = str_positions(input).next().unwrap();
        let expected = Identifier {
            text: input.to_string(),
            pos: expected_pos,
        };

        test_good_try_parse(input, &expected);
    }

    #[test]
    fn invalid_identifier() {
        let input = "+alpha";

        let actual = Identifier::try_parse(&mut lex_str(input), &FilePosition::default());

        assert!(actual.is_err());
    }

    #[test]
    #[should_panic(expected = "empty identifier")]
    fn empty_identifier() {
        let token = Token {
            token: RawToken::UnmarkedIdentifier(String::new()),
            pos: FilePosition::default(),
        };
        let mut input = std::iter::once(token);

        let _ = Identifier::try_parse(&mut input, &FilePosition::default());
    }

    #[test]
    fn attributes() {
        let input = "+alpha -bravo.charlie delta";
        let input_tokens: Vec<_> = str_positions(input).collect();
        let expected_kinds = vec![
            AttributeKind::Feature(true, "alpha".to_string()),
            AttributeKind::Parameter(false, "bravo".to_string(), "charlie".to_string()),
            AttributeKind::Character("delta".to_string()),
        ];
        let expected = expected_kinds
            .into_iter()
            .zip(input_tokens)
            .map(|(kind, pos)| Attribute { kind, pos })
            .collect_vec();

        let actual = lex_str(input)
            .batching(|it| Attribute::try_parse(it, &FilePosition::default()).ok())
            .collect_vec();

        assert_eq!(actual, expected);
    }

    #[test]
    fn invalid_attribute() {
        let input = "languages";
        let expected_token = lex_str(input).next().unwrap();
        let expected: ParseError = unexpect(expected_token, Expectation::Attribute);

        let actual = Attribute::try_parse(&mut lex_str(input), &FilePosition::default());

        assert_eq!(actual.unwrap_err(), expected);
    }

    #[test]
    fn output_attributes() {
        let input = "+alpha -bravo.charlie delta 5";
        let input_tokens: Vec<_> = str_positions(input).collect();
        let expected_kinds = vec![
            OutputAttributeKind::Feature(true, "alpha".to_string()),
            OutputAttributeKind::Parameter(false, "bravo".to_string(), "charlie".to_string()),
            OutputAttributeKind::Character("delta".to_string()),
            OutputAttributeKind::SelectorCode(5),
        ];
        let expected = expected_kinds
            .into_iter()
            .zip(input_tokens)
            .map(|(kind, pos)| OutputAttribute { kind, pos })
            .collect_vec();

        let actual = lex_str(input)
            .batching(|it| OutputAttribute::try_parse(it, &FilePosition::default()).ok())
            .collect_vec();

        assert_eq!(actual, expected);
    }

    #[test]
    fn invalid_output_attribute() {
        let input = "languages";
        let expected_token = lex_str(input).next().unwrap();
        let expected: ParseError = unexpect(expected_token, Expectation::Attribute);

        let actual = OutputAttribute::try_parse(&mut lex_str(input), &FilePosition::default());

        assert_eq!(actual.unwrap_err(), expected);
    }

    #[test]
    fn diacritic() {
        let input = "aé";
        let char = '\u{0301}';
        let pos = str_positions(input).next().unwrap();
        let expected = Diacritic { char, pos };

        test_good_try_parse(input, &expected);
    }

    #[test]
    fn diacritic_definition() {
        let input = "aé +alpha bravo charlie;";
        let char = '\u{0301}';
        let (dia_pos, attr_pos) = str_positions(input).next_tuple().unwrap();
        let mut tail = lex_str(input).skip(2).take(2);
        let expected = DiacriticDefinition {
            diacritic: Diacritic { char, pos: dia_pos },
            definition: Ok(Attribute {
                kind: AttributeKind::Feature(true, "alpha".to_string()),
                pos: attr_pos,
            }),
            excess_tokens: super::end_line(&mut tail).collect_vec(),
        };

        let actual =
            DiacriticDefinition::try_parse(&mut lex_str(input), &FilePosition::default()).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn invalid_diacritic_definition() {
        let input = "+alpha bravo charlie;";

        let actual = DiacriticDefinition::try_parse(&mut lex_str(input), &FilePosition::default())
            .unwrap_err();

        assert_eq!(actual.len(), 3);
    }
}
