use super::*;

use crate::config::Config;
use crate::error_handling::FilePosition;
use crate::lexer::{Lexer, Token};

fn lex_str(input: &'static str) -> impl Iterator<Item = Token> {
    Lexer::lex(std::io::Cursor::new(input), None)
}

#[test]
fn compiler_passes_def_block_errs() {
    let input_str = "{ # ";

    let block = Block::<IdentifierLine, Vec<ParseError>>::try_parse(
        &mut lex_str(input_str),
        &FilePosition::default(),
    )
    .unwrap();
    let mut actual = Logger::new();
    define_block(block, |_, _, _| (), &mut Config::new(), &mut actual);

    // We expect to see the unexpected token errors for the word boundary, and
    // then an error for the unclosed block.
    assert_eq!(actual.errors.len(), 2);
}

#[test]
fn languages_block() {
    let input_str = "languages { alpha; bravo; charlie }";
    let languages: [String; 3] = ["alpha", "bravo", "charlie"].map(Into::into);

    let mut actual = Config::new();
    let a_lexer = &mut lex_str(input_str);
    let keyword = a_lexer.next().unwrap();
    let block = parse_definition_block(a_lexer, keyword).unwrap();
    compile_definition_block(block, &mut actual, &mut Logger::new());

    for language in &languages {
        assert!(actual.languages.encode(language).is_some());
    }
    assert_eq!(actual.first_language.unwrap(), languages[0]);
    assert_eq!(actual.last_language.unwrap(), languages[2]);
}

#[test]
fn features_block() {
    let input_str = "features { alpha; bravo; charlie }";
    let features: [String; 3] = ["alpha", "bravo", "charlie"].map(Into::into);

    let mut actual = Config::new();
    let a_lexer = &mut lex_str(input_str);
    let keyword = a_lexer.next().unwrap();
    let block = parse_definition_block(a_lexer, keyword).unwrap();
    compile_definition_block(block, &mut actual, &mut Logger::new());

    for feature in &features {
        assert!(actual.features.encode(feature).is_some());
    }
}
