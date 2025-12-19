use itertools::Itertools;
use std::fmt::Debug;

use super::errors::ParseErrorType;
use super::*;
use crate::lexer::Lexer;

fn lex_str(input: &'static str) -> impl Iterator<Item = Token> {
    Lexer::lex(std::io::Cursor::new(input), None).map(Result::unwrap)
}

fn str_positions(input: &'static str) -> impl Iterator<Item = FilePosition> {
    lex_str(input).map(|token| token.pos)
}

fn test_good_try_parse<E: Debug + PartialEq, T: Parse<E> + Debug + PartialEq>(
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

    let expected: Vec<ParseError> = lex_str(no_end_input)
        .map(|token| unexpect(token, Expectation::Token(RawToken::Eol)))
        .collect_vec();
    let expected_tail = lex_str(input).skip(3).collect_vec();

    let lexer = &mut lex_str(input);
    let actual = super::end_line(lexer).collect_vec();
    let actual_tail = lexer.collect_vec();

    assert_eq!(actual, expected);
    assert_eq!(actual_tail, expected_tail);
}

#[test]
fn eof() {
    let expected = Err(super::errors::eof(
        FilePosition::default(),
        Expectation::Identifier,
    ));

    let actual = read_token(
        &mut lex_str(""),
        Expectation::Identifier,
        &FilePosition::default(),
    );

    assert_eq!(actual, expected);
}

#[test]
fn read_until_closed() {
    let input = "alpha bravo } charlie delta";
    let expected = lex_str(input).take(2).collect_vec();
    let expected_tail = lex_str(input).skip(3).collect_vec();

    let lexer = &mut lex_str(input);
    let actual = super::read_until_closed(lexer, RawToken::BlockClose, &FilePosition::default())
        .unwrap()
        .collect_vec();
    let actual_tail = lexer.collect_vec();

    assert_eq!(actual, expected);
    assert_eq!(actual_tail, expected_tail);
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
    let token = lex_str(input).next().unwrap();
    let expected = Err(unexpect(token, Expectation::Identifier));

    let actual = Identifier::try_parse(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
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

fn test_attribute_kind<T: TryFrom<Token, Error = ParseError> + Debug + PartialEq>(
    input: &'static str,
    expected: Vec<Result<T, ParseErrorType>>,
) {
    let expected_results = expected
        .into_iter()
        .zip_eq(str_positions(input))
        .map(|(res, pos)| match res {
            Ok(kind) => Ok(Attribute::<T> { kind, pos }),
            Err(e) => Err(e.at::<ParseError>(pos)),
        })
        .collect_vec();

    let actual = Attribute::<T>::parse_iter(&mut lex_str(input));

    assert_eq!(actual, expected_results);
}

#[test]
fn filter_attribute() {
    let input = "+alpha +bravo.charlie -delta.echo foxtrot 5 #";
    let expected = vec![
        Ok(FilterAttributeKind::Feature(true, "alpha".to_string())),
        Ok(FilterAttributeKind::Parameter(
            true,
            "bravo".to_string(),
            "charlie".to_string(),
        )),
        Ok(FilterAttributeKind::Parameter(
            false,
            "delta".to_string(),
            "echo".to_string(),
        )),
        Ok(FilterAttributeKind::Character("foxtrot".to_string())),
        Err(InvalidSelectorCode),
        Err(Unexpected(RawToken::WordBoundry, Expectation::Attribute)),
    ];

    test_attribute_kind(input, expected);
}

#[test]
fn phoneme_attribute() {
    let input = "+alpha +bravo.charlie -delta.echo foxtrot 5 #";
    let expected = vec![
        Ok(PhonemeAttributeKind::Feature(true, "alpha".to_string())),
        Ok(PhonemeAttributeKind::Parameter(
            "bravo".to_string(),
            "charlie".to_string(),
        )),
        Err(InvalidNegativeParameter),
        Ok(PhonemeAttributeKind::Character("foxtrot".to_string())),
        Err(InvalidSelectorCode),
        Err(Unexpected(RawToken::WordBoundry, Expectation::Attribute)),
    ];

    test_attribute_kind(input, expected);
}

#[test]
fn output_attribute() {
    let input = "+alpha +bravo.charlie -delta.echo foxtrot 5 #";
    let expected = vec![
        Ok(OutputAttributeKind::Feature(true, "alpha".to_string())),
        Ok(OutputAttributeKind::Parameter(
            "bravo".to_string(),
            "charlie".to_string(),
        )),
        Err(InvalidNegativeParameter),
        Ok(OutputAttributeKind::Character("foxtrot".to_string())),
        Ok(OutputAttributeKind::SelectorCode(5)),
        Err(Unexpected(RawToken::WordBoundry, Expectation::Attribute)),
    ];

    test_attribute_kind(input, expected);
}

#[test]
fn phoneme() {
    let input = "+alpha +bravo.charlie -delta.echo foxtrot 5 #]";
    let attrs_input = &input[..input.len() - 1];
    // let expected_attrs = PhonemeAttribute::parse_iter(&mut lex_str(attrs_input));
    let expected = Ok(Phoneme {
        attributes: PhonemeAttribute::parse_iter(&mut lex_str(attrs_input)),
        pos: FilePosition::default(),
    });

    let actual = Phoneme::try_parse(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
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
    let mut tail = lex_str(input).skip(2);
    let expected = Ok(DiacriticDefinition {
        diacritic: Diacritic { char, pos: dia_pos },
        definition: Ok(Attribute {
            kind: PhonemeAttributeKind::Feature(true, "alpha".to_string()),
            pos: attr_pos,
        }),
        excess_tokens: super::end_line(&mut tail).collect_vec(),
    });

    let actual = DiacriticDefinition::try_parse(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn invalid_diacritic_definition() {
    let input = "+alpha bravo charlie";
    let (dia, bravo, charlie) = lex_str(input).collect_tuple().unwrap();
    let expected = Err(vec![
        unexpect(dia, Expectation::Identifier),
        unexpect(bravo, RawToken::Eol),
        unexpect(charlie, RawToken::Eol),
    ]);

    let actual = DiacriticDefinition::try_parse(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
}
