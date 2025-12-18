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
    let expected = Err(unexpect(expected_token, Expectation::Attribute));

    let actual = Attribute::try_parse(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
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
    let expected = Err(unexpect(expected_token, Expectation::Attribute));

    let actual = OutputAttribute::try_parse(&mut lex_str(input), &FilePosition::default());

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
            kind: AttributeKind::Feature(true, "alpha".to_string()),
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
