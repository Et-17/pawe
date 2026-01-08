use itertools::Itertools;
use std::fmt::Debug;
use std::fs::File;
use std::io::BufReader;

use super::errors::ParseErrorType;
use super::*;
use crate::lexer::Lexer;

fn lex_str(input: &'static str) -> impl Iterator<Item = Token> {
    Lexer::lex(std::io::Cursor::new(input), None)
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
fn confirm_token_failure() {
    let input = "#";
    let token = lex_str(input).next().unwrap();
    let expected = Err(unexpect(token, RawToken::PhonemeOpen));

    let actual = confirm_token(
        &mut lex_str(input),
        RawToken::PhonemeOpen,
        &FilePosition::default(),
    );

    assert_eq!(actual, expected);

    let expected_eof = Err(super::errors::eof(
        FilePosition::default(),
        RawToken::PhonemeOpen,
    ));

    let actual_eof = confirm_token(
        &mut lex_str(""),
        RawToken::PhonemeOpen,
        &FilePosition::default(),
    );

    assert_eq!(actual_eof, expected_eof);
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

    let actual = Attribute::<T>::parse_iter(&mut lex_str(input), &FilePosition::default());

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
    let expected = Ok(Phoneme {
        attributes: PhonemeAttribute::parse_iter(
            &mut lex_str(attrs_input),
            &FilePosition::default(),
        ),
        pos: FilePosition::default(),
    });

    let actual = Phoneme::try_parse(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn output_phoneme() {
    let input = "[+alpha +bravo.charlie -delta.echo foxtrot 5 #]";
    let attrs_input = &input[..input.len() - 1];
    let expected = Ok(OutputPhoneme {
        attributes: OutputAttribute::parse_iter(
            &mut lex_str(attrs_input).skip(1),
            &FilePosition::default(),
        ),
        pos: FilePosition::default(),
    });

    let actual = OutputPhoneme::try_parse(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn filter() {
    let input = "+alpha +bravo.charlie -delta.echo foxtrot 5 #)";
    let attrs_input = &input[..input.len() - 1];
    let expected = Ok(Filter {
        attributes: FilterAttribute::parse_iter(
            &mut lex_str(attrs_input),
            &FilePosition::default(),
        ),
        pos: FilePosition::default(),
    });

    let actual = Filter::try_parse(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn selector() {
    let input = "+alpha +bravo.charlie -delta.echo foxtrot 5 #)";
    let code = 12;
    let attrs_input = &input[..input.len() - 1];
    let expected = Ok(Selector {
        attributes: FilterAttribute::parse_iter(
            &mut lex_str(attrs_input),
            &FilePosition::default(),
        ),
        code,
        pos: FilePosition::default(),
    });

    let actual = Selector::try_parse(&mut lex_str(input), &FilePosition::default(), code);

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

#[test]
fn input_atom() {
    let input = "[+alpha] (-bravo.charlie) 1(+delta.echo) foxtrot";

    let dfp = &FilePosition::default();
    let e_lexer = &mut lex_str(input);
    let phoneme_open = e_lexer.next().unwrap();
    let phoneme = Phoneme::try_parse(e_lexer, &phoneme_open.pos);
    let filter_open = e_lexer.next().unwrap();
    let filter = Filter::try_parse(e_lexer, &filter_open.pos);
    let selector_open = e_lexer.next().unwrap();
    let selector = Selector::try_parse(e_lexer, &selector_open.pos, 1);
    let identifier = Identifier::try_parse(e_lexer, dfp);
    let expected = vec![
        Ok(InputAtom::Phoneme(phoneme)),
        Ok(InputAtom::Filter(filter)),
        Ok(InputAtom::Selector(selector)),
        Ok(InputAtom::Identifier(identifier)),
    ];

    let actual = InputAtom::parse_iter(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn invalid_input_atom() {
    let input = "#";

    let e_token = lex_str(input).next().unwrap();
    let expected = Err(unexpect(e_token, Expectation::InputAtom));

    let actual = InputAtom::try_parse(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn environment_atom() {
    let input = "[+alpha] (-bravo) ?charlie *delta !echo";

    let e_lexer = &mut lex_str(input);
    let phoneme_open = e_lexer.next().unwrap();
    let phoneme = Phoneme::try_parse(e_lexer, &phoneme_open.pos);
    let filter_open = e_lexer.next().unwrap();
    let filter = Filter::try_parse(e_lexer, &filter_open.pos);
    e_lexer.next().unwrap();
    let charlie = Identifier::try_parse(e_lexer, &FilePosition::default());
    e_lexer.next().unwrap();
    let delta = Identifier::try_parse(e_lexer, &FilePosition::default());
    e_lexer.next().unwrap();
    let echo = Identifier::try_parse(e_lexer, &FilePosition::default());
    let expected = vec![
        Ok(EnvironmentAtom::Phoneme(phoneme)),
        Ok(EnvironmentAtom::Filter(filter)),
        Ok(EnvironmentAtom::Optional(Box::new(Ok(
            EnvironmentAtom::Identifier(charlie),
        )))),
        Ok(EnvironmentAtom::ZeroOrMore(Box::new(Ok(
            EnvironmentAtom::Identifier(delta),
        )))),
        Ok(EnvironmentAtom::Not(Box::new(Ok(
            EnvironmentAtom::Identifier(echo),
        )))),
    ];

    let actual = EnvironmentAtom::parse_iter(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn environment_atom_nesting() {
    let input = "?*!alpha bravo";

    let pos = str_positions(input).nth(3).unwrap();
    let character = EnvironmentAtom::Identifier(Ok(Identifier {
        text: "alpha".to_string(),
        pos,
    }));
    let not = EnvironmentAtom::Not(Box::new(Ok(character)));
    let zeroormore = EnvironmentAtom::ZeroOrMore(Box::new(Ok(not)));
    let optional = EnvironmentAtom::Optional(Box::new(Ok(zeroormore)));
    let expected = Ok(optional);
    let expected_tail = lex_str(input).last();

    let lexer = &mut lex_str(input);
    let actual = EnvironmentAtom::try_parse(lexer, &FilePosition::default());
    let actual_tail = lexer.next();

    assert_eq!(actual, expected);
    assert_eq!(actual_tail, expected_tail);
}

#[test]
fn invalid_env_atom_nesting() {
    let input = "?*";

    let pos = str_positions(input).last().unwrap();
    let error = super::errors::eof(pos, Expectation::EnvironmentAtom);
    let zeroormore = EnvironmentAtom::ZeroOrMore(Box::new(Err(error)));
    let optional = EnvironmentAtom::Optional(Box::new(Ok(zeroormore)));
    let expected = Ok(optional);

    let actual = EnvironmentAtom::try_parse(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn invalid_environment_atom() {
    let input = "_ # languages";

    let (target, boundary, lang) = lex_str(input).collect_tuple().unwrap();
    let expected = vec![
        Err(ExcessTargets.at(target.pos)),
        Err(MisplacedWordBoundary.at(boundary.pos)),
        Err(unexpect(lang, Expectation::EnvironmentAtom)),
    ];

    let actual = EnvironmentAtom::parse_iter(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn environment_no_boundaries() {
    let input = "/ alpha bravo _ charlie delta";

    let e_lexer = &mut lex_str(input);
    e_lexer.next().unwrap();
    let pre_tokens: [Token; 2] = e_lexer.next_array().unwrap();
    e_lexer.next().unwrap();
    let post_tokens: [Token; 2] = e_lexer.collect_array().unwrap();
    let pre = EnvironmentAtom::parse_iter(&mut pre_tokens.into_iter(), &FilePosition::default());
    let post = EnvironmentAtom::parse_iter(&mut post_tokens.into_iter(), &FilePosition::default());
    let expected = Ok(Environment {
        start: false,
        end: false,
        pre,
        post,
    });

    let actual = Environment::try_parse(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn environment_both_boundaries() {
    let input = "/ # alpha bravo _ charlie delta #";

    let e_lexer = &mut lex_str(input);
    e_lexer.take(2).count();
    let pre_tokens: [Token; 2] = e_lexer.next_array().unwrap();
    e_lexer.next().unwrap();
    let post_tokens: [Token; 2] = e_lexer.next_array().unwrap();
    assert_eq!(e_lexer.count(), 1);
    let pre = EnvironmentAtom::parse_iter(&mut pre_tokens.into_iter(), &FilePosition::default());
    let post = EnvironmentAtom::parse_iter(&mut post_tokens.into_iter(), &FilePosition::default());
    let expected = Ok(Environment {
        start: true,
        end: true,
        pre,
        post,
    });

    let actual = Environment::try_parse(&mut lex_str(input), &FilePosition::default());

    let expected = dbg!(expected);
    assert_eq!(actual, expected);
}

#[test]
fn misplaced_boundary_in_post_env() {
    let input = "/ alpha bravo _ charlie # delta #";

    let e_lexer = &mut lex_str(input);
    e_lexer.next().unwrap();
    let pre = EnvironmentAtom::parse_iter(&mut e_lexer.take(2), &FilePosition::default());
    e_lexer.next().unwrap();
    let post = EnvironmentAtom::parse_iter(&mut e_lexer.take(3), &FilePosition::default());
    assert_eq!(e_lexer.count(), 1);
    let expected = Ok(Environment {
        start: false,
        end: true,
        pre,
        post,
    });

    let actual = Environment::try_parse(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn environment_empty() {
    let input = "";

    let expected = Ok(Environment {
        start: false,
        end: false,
        pre: Vec::new(),
        post: Vec::new(),
    });

    let actual = Environment::try_parse(&mut lex_str(input), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn rule_with_environment() {
    let input_str = "alpha bravo > charlie delta / echo _ golf; hotel";

    let e_lexer = &mut lex_str(input_str);
    let input = InputAtom::parse_iter(&mut e_lexer.take(2), &FilePosition::default());
    e_lexer.next().unwrap();
    let output = OutputPhoneme::parse_iter(&mut e_lexer.take(2), &FilePosition::default());
    let env = Environment::try_parse(&mut e_lexer.take(4), &FilePosition::default());
    let expected = Ok(Rule { input, output, env });
    e_lexer.next().unwrap();
    let expected_tail = e_lexer.collect_vec();

    let a_lexer = &mut lex_str(input_str);
    let actual = Rule::try_parse(a_lexer, &FilePosition::default());
    let actual_tail = a_lexer.collect_vec();

    assert_eq!(actual, expected);
    assert_eq!(actual_tail, expected_tail);
}

#[test]
fn rule_without_environment() {
    let input_str = "alpha bravo > charlie delta; hotel";

    let e_lexer = &mut lex_str(input_str);
    let input = InputAtom::parse_iter(&mut e_lexer.take(2), &FilePosition::default());
    e_lexer.next().unwrap();
    let output = OutputPhoneme::parse_iter(&mut e_lexer.take(2), &FilePosition::default());
    let env = Ok(Environment {
        start: false,
        end: false,
        pre: Vec::new(),
        post: Vec::new(),
    });
    let expected = Ok(Rule { input, output, env });
    e_lexer.next().unwrap();
    let expected_tail = e_lexer.collect_vec();

    let a_lexer = &mut lex_str(input_str);
    let actual = Rule::try_parse(a_lexer, &FilePosition::default());
    let actual_tail = a_lexer.collect_vec();

    assert_eq!(actual, expected);
    assert_eq!(actual_tail, expected_tail);
}

#[test]
fn rule_without_output() {
    let input_str = "alpha bravo / charlie _ delta";

    let e_lexer = &mut lex_str(input_str);
    let (input, last_pos) =
        InputAtom::tracked_parse_iter(&mut e_lexer.take(2), &FilePosition::default());
    let output = vec![Err(super::eof(last_pos, RawToken::Output))];
    let env = Environment::try_parse(e_lexer, &FilePosition::default());
    let expected = Ok(Rule { input, output, env });

    let actual = Rule::try_parse(&mut lex_str(input_str), &FilePosition::default());

    assert_eq!(actual, expected);
}

//ABCDEFGHIJKLMNOPQRSTUVWXYZ
#[test]
fn character_definition_phoneme() {
    let input_str = "alpha [+bravo -charlie.delta] echo foxtrot;";

    let e_lexer = &mut lex_str(input_str);
    let character = Identifier::try_parse(e_lexer, &FilePosition::default()).unwrap();
    let def_pos = e_lexer.next().unwrap().pos;
    let definition = Phoneme::try_parse(e_lexer, &def_pos);
    let excess_tokens = super::end_line(e_lexer).collect_vec();
    let expected = Ok(CharacterDefinition {
        character,
        definition,
        excess_tokens,
    });

    let actual = CharacterDefinition::try_parse(&mut lex_str(input_str), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn character_definition_character() {
    let input_str = "alpha bravo echo foxtrot;";

    let e_lexer = &mut lex_str(input_str);
    let character = Identifier::try_parse(e_lexer, &FilePosition::default()).unwrap();
    let def_tok_pos = e_lexer.next().unwrap().pos;
    let def_attr = PhonemeAttribute {
        kind: PhonemeAttributeKind::Character("bravo".to_string()),
        pos: def_tok_pos.clone(),
    };
    let excess_tokens = super::end_line(e_lexer).collect_vec();
    let definition = Ok(Phoneme {
        attributes: vec![Ok(def_attr)],
        pos: def_tok_pos,
    });
    let expected = Ok(CharacterDefinition {
        character,
        definition,
        excess_tokens,
    });

    let actual = CharacterDefinition::try_parse(&mut lex_str(input_str), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn invalid_character_definition() {
    let input_str = "# alpha bravo;";

    let e_lexer = &mut lex_str(input_str);
    let error = unexpect(e_lexer.next().unwrap(), Expectation::Identifier);
    let mut errors = vec![error];
    errors.extend(super::end_line(e_lexer));
    let expected = Err(errors);

    let actual = CharacterDefinition::try_parse(&mut lex_str(input_str), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn char_def_invalid_def() {
    let input_str = "alpha #;";

    let e_lexer = &mut lex_str(input_str);
    let character = Identifier::try_parse(e_lexer, &FilePosition::default()).unwrap();
    let definition = Err(unexpect(e_lexer.next().unwrap(), RawToken::PhonemeOpen));
    let expected = Ok(CharacterDefinition {
        character,
        definition,
        excess_tokens: Vec::new(),
    });

    let actual = CharacterDefinition::try_parse(&mut lex_str(input_str), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn parameter_definition() {
    let input_str = "alpha bravo charlie delta; echo";

    let e_lexer = &mut lex_str(input_str);
    let parameter = Identifier::try_parse(e_lexer, &FilePosition::default()).unwrap();
    let variants = Identifier::parse_iter(&mut e_lexer.take(3), &FilePosition::default());
    let expected = Ok(ParameterDefinition {
        parameter,
        variants,
    });
    e_lexer.next().unwrap();
    let expected_tail = e_lexer.collect_vec();

    let a_lexer = &mut lex_str(input_str);
    let actual = ParameterDefinition::try_parse(a_lexer, &FilePosition::default());
    let actual_tail = a_lexer.collect_vec();

    assert_eq!(actual, expected);
    assert_eq!(actual_tail, expected_tail);
}

#[test]
fn invalid_parameter_definition() {
    let input_str = "# bravo charlie;";

    let e_lexer = &mut lex_str(input_str);
    let error = unexpect(e_lexer.next().unwrap(), Expectation::Identifier);
    let mut errors = vec![error];
    errors.extend(super::end_line(e_lexer));
    let expected = Err(errors);

    let actual = ParameterDefinition::try_parse(&mut lex_str(input_str), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn identifier_line() {
    let input_str = "alpha bravo charlie;";

    let e_lexer = &mut lex_str(input_str);
    let identifier = Identifier::try_parse(e_lexer, &FilePosition::default()).unwrap();
    let excess_tokens = super::end_line(e_lexer).collect_vec();
    let expected = Ok(IdentifierLine {
        identifier,
        excess_tokens,
    });

    let actual = IdentifierLine::try_parse(&mut lex_str(input_str), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn invalid_identifier_line() {
    let input_str = "# alpha bravo;";

    let e_lexer = &mut lex_str(input_str);
    let error = unexpect(e_lexer.next().unwrap(), Expectation::Identifier);
    let mut errors = vec![error];
    errors.extend(super::end_line(e_lexer));
    let expected = Err(errors);

    let actual = IdentifierLine::try_parse(&mut lex_str(input_str), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn block() {
    let input_str = "{ alpha; bravo; charlie } delta";

    let e_lexer = &mut lex_str(input_str);
    let pos = e_lexer.next().unwrap().pos;
    let definitions = IdentifierLine::parse_iter(&mut e_lexer.take(5), &pos);
    let expected = Ok(Block {
        definitions,
        close_error: Ok(()),
        pos,
    });
    e_lexer.next().unwrap();
    let expected_tail = e_lexer.collect_vec();

    let a_lexer = &mut lex_str(input_str);
    let actual =
        Block::<IdentifierLine, Vec<ParseError>>::try_parse(a_lexer, &FilePosition::default());
    let actual_tail = a_lexer.collect_vec();

    assert_eq!(actual, expected);
    assert_eq!(actual_tail, expected_tail);
}

#[test]
fn invalid_block() {
    let input_str = "# alpha";

    let e_lexer = &mut lex_str(input_str);
    let expected = Err(unexpect(e_lexer.next().unwrap(), RawToken::BlockOpen));
    let expected_tail = e_lexer.collect_vec();

    let a_lexer = &mut lex_str(input_str);
    let actual =
        Block::<IdentifierLine, Vec<ParseError>>::try_parse(a_lexer, &FilePosition::default());
    let actual_tail = a_lexer.collect_vec();

    assert_eq!(actual, expected);
    assert_eq!(actual_tail, expected_tail);
}

#[test]
fn confirm_to_existence() {
    let input_str = "to";

    let pos = str_positions(input_str).next().unwrap();
    let expected = (Ok(()), pos);

    let actual =
        super::confirm_to_existence(&mut lex_str(input_str).peekable(), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn non_ident_invalid_to() {
    let input_str = "#";

    let e_lexer = &mut lex_str(input_str).peekable();
    let token = e_lexer.next().unwrap();
    let pos = token.pos.clone();
    let expected = (Err(super::unexpect(token, RawToken::To)), pos);
    let expected_tail = e_lexer.collect_vec();

    let a_lexer = &mut lex_str(input_str).peekable();
    let actual = super::confirm_to_existence(a_lexer, &FilePosition::default());
    let actual_tail = a_lexer.collect_vec();

    assert_eq!(actual, expected);
    assert_eq!(actual_tail, expected_tail);
}

#[test]
fn ident_invalid_to() {
    let input_str = "alpha";

    let e_lexer = &mut lex_str(input_str).peekable();
    let expected = (
        Err(super::eof(FilePosition::default(), RawToken::To)),
        FilePosition::default(),
    );
    let expected_tail = e_lexer.next().unwrap();

    let a_lexer = &mut lex_str(input_str).peekable();
    let actual = super::confirm_to_existence(a_lexer, &FilePosition::default());
    let actual_tail = a_lexer.next().unwrap();

    assert_eq!(actual, expected);
    assert_eq!(actual_tail, expected_tail);
}

#[test]
fn evolution() {
    let input_str = "alpha to bravo { charlie > delta }";

    let e_lexer = &mut lex_str(input_str);
    let start = Identifier::try_parse(e_lexer, &FilePosition::default());
    e_lexer.next().unwrap(); // skip to
    let end = Identifier::try_parse(e_lexer, &FilePosition::default());
    let block_open_pos = e_lexer.next().unwrap().pos;
    let definitions = Rule::parse_iter(&mut e_lexer.take(3), &FilePosition::default());
    let rules = Ok(Block {
        definitions,
        close_error: Ok(()),
        pos: block_open_pos,
    });
    let expected = Ok(Evolution {
        start,
        missing_to: Ok(()),
        end,
        rules,
    });

    let actual = Evolution::try_parse(&mut lex_str(input_str), &FilePosition::default());

    assert_eq!(actual, expected);
}

#[test]
fn invalid_evolution_pos_stability() {
    let start_input_str = "#";
    let end_input_str = "alpha to #";

    let e_start = lex_str(start_input_str).next().unwrap().pos;
    let e_end = lex_str(end_input_str).nth(2).unwrap().pos;

    let a_start = Evolution::try_parse(&mut lex_str(start_input_str), &FilePosition::default())
        .unwrap()
        .missing_to
        .unwrap_err()
        .pos
        .unwrap();
    let a_end = Evolution::try_parse(&mut lex_str(end_input_str), &FilePosition::default())
        .unwrap()
        .rules
        .unwrap_err()
        .pos
        .unwrap();

    assert_eq!(a_start, e_start);
    assert_eq!(a_end, e_end);
}

#[test]
fn config_parser() {
    let input_file = PathBuf::from("./examples/blank.paw");

    let e_file = File::open(&input_file).unwrap();
    let e_lexer = &mut Lexer::lex(BufReader::new(e_file), Some(input_file.clone()));
    e_lexer.next().unwrap(); // skip languages
    let languages = Block::try_parse(e_lexer, &FilePosition::default()).unwrap();
    e_lexer.next().unwrap(); // skip parameters
    let parameters = Block::try_parse(e_lexer, &FilePosition::default()).unwrap();
    e_lexer.next().unwrap(); // skip features
    let features = Block::try_parse(e_lexer, &FilePosition::default()).unwrap();
    e_lexer.next().unwrap(); // skip characters
    let characters = Block::try_parse(e_lexer, &FilePosition::default()).unwrap();
    e_lexer.next().unwrap(); // skip diacritics
    let diacritics = Block::try_parse(e_lexer, &FilePosition::default()).unwrap();
    e_lexer.next().unwrap(); // skip evolve
    let evolve = Evolution::try_parse(e_lexer, &FilePosition::default()).unwrap();
    let invalid = unexpect(e_lexer.next().unwrap(), Expectation::DefinitionKeyword);
    let expected = vec![
        Ok(DefinitionBlock::Languages(languages)),
        Ok(DefinitionBlock::Parameters(parameters)),
        Ok(DefinitionBlock::Features(features)),
        Ok(DefinitionBlock::Characters(characters)),
        Ok(DefinitionBlock::Diacritics(diacritics)),
        Ok(DefinitionBlock::Evolve(Box::new(evolve))),
        Err(invalid),
    ];

    let parser = ConfigParser::new(input_file).unwrap();
    let actual = parser.collect_vec();

    assert_eq!(actual, expected);
}

#[test]
fn invalid_path_cfg_prsr() {
    let input_file = PathBuf::from("./invalid/invalid.paw");

    let actual = ConfigParser::new(input_file);

    assert!(actual.is_err());
}

#[test]
fn invalid_char_cfg_prsr() {
    let input_file = PathBuf::from("./examples/invalid_utf8.paw");

    let mut actual = ConfigParser::new(input_file).unwrap();
    while actual.next().is_some() {}

    assert_eq!(actual.io_error.len(), 1);
    assert!(actual.next().is_none());
}
