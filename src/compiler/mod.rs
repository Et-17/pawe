mod errors;

use itertools::Itertools;

use crate::compiler::errors::{Defineable, Expectation, eof_error, unexpect};
use crate::config::{Character, CharacterDefinition, Config, Label};
use crate::error_handling::{
    Error, ErrorType, FilePosition, Result, ResultV, check_errors, wrap_io_error,
};
use crate::evolution::{Environment, EnvironmentAtom, InputAtom, Rule};
use crate::lexer::{Lexer, RawToken, Token};
use crate::phonemes::{Attribute, Filter, Phoneme, Selector, SelectorCode, UnboundPhoneme};

use errors::CompileErrorType::*;

pub fn parse_config_file(path: std::path::PathBuf) -> ResultV<Config> {
    let file = std::fs::File::open(&path).map_err(|e| {
        let file_pos = FilePosition::new(Some(&path.as_path().into()), None, None);
        wrap_io_error("parser", Some(&file_pos))(e)
    })?;

    Lexer::lex(std::io::BufReader::new(file), Some(path))
        .process_results(|mut tokens| parse_config(&mut tokens))?
}

pub fn parse_word(word: &str, config: &Config) -> ResultV<Vec<Phoneme>> {
    Lexer::lex(std::io::Cursor::new(word), None)
        .process_results(|mut tokens| parse_unwrapped_word(&mut tokens, config))?
}

fn parse_unwrapped_word(
    word: &mut impl Iterator<Item = Token>,
    config: &Config,
) -> ResultV<Vec<Phoneme>> {
    let mut errors = Vec::new();

    let mut phonemes = Vec::new();

    while let Some(token) = word.next() {
        match token.token {
            RawToken::PhonemeOpen => match parse_phoneme(word, config) {
                Ok(phoneme) => phonemes.push(phoneme),
                Err(mut errs) => errors.append(&mut errs),
            },
            RawToken::UnmarkedIdentifier(ident) => {
                match parse_character(config, &ident, &token.pos) {
                    Ok(phoneme) => phonemes.push(phoneme),
                    Err(mut errs) => errors.append(&mut errs),
                }
            }
            _ => errors.push(unexpect(token, Expectation::WordAtom)),
        }
    }

    let phonemes = phonemes
        .into_iter()
        .map(|p| p.rebase(&config.characters, &config.diacritics))
        .collect();

    check_errors(phonemes, errors)
}

fn parse_config(file: &mut impl Iterator<Item = Token>) -> ResultV<Config> {
    let mut config = Config::new();
    let mut errors = Vec::new();

    while let Some(token) = file.next() {
        let pos = &token.pos;

        let parsing_result = match token.token {
            RawToken::Languages => parse_languages(file, &mut config, pos),
            RawToken::Features => parse_features(file, &mut config, pos),
            RawToken::Parameters => parse_parameters(file, &mut config, pos),
            RawToken::Characters => parse_characters(file, &mut config, pos),
            RawToken::Diacritics => parse_diacritics(file, &mut config, pos),
            RawToken::Evolve => parse_evolve(file, &mut config, pos),
            _ => Err(unexpect(token, Expectation::DefinitionKeyword)),
        };

        if let Err(mut errs) = parsing_result {
            errors.append(&mut errs);
        }
    }

    check_errors(config, errors)
}

fn parse_languages(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: &FilePosition,
) -> ResultV<()> {
    let mut errors = Vec::new();
    let languages = parse_identifier_block(file, &mut errors, pos);

    if config.first_language.is_none() {
        config.first_language = languages.first().cloned();
    }

    if !languages.is_empty() {
        config.last_language = languages.last().cloned();
    }

    config.languages.extend(languages);

    check_errors((), errors)
}

fn parse_features(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: &FilePosition,
) -> ResultV<()> {
    let mut errors = Vec::new();
    let features = parse_identifier_block(file, &mut errors, pos);

    config.features.extend(features);

    check_errors((), errors)
}

fn parse_parameters(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: &FilePosition,
) -> ResultV<()> {
    confirm_token_type(file, RawToken::BlockOpen, pos)?;

    let mut errors: Vec<Error> = Vec::new();

    while let Some(token) = file.next() {
        if token.token == RawToken::BlockClose {
            break;
        } else if token.token == RawToken::Eol {
            continue;
        }

        if let Err(mut errs) = parse_parameter_def(file, config, token) {
            errors.append(&mut errs);
        }
    }

    check_errors((), errors)
}

fn parse_parameter_def(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    name_token: Token,
) -> ResultV<()> {
    let RawToken::UnmarkedIdentifier(name) = name_token.token else {
        return Err(unexpect(name_token, Expectation::Identifier));
    };

    let mut errors = Vec::new();
    let variant_labels = parse_identifier_block(file, &mut errors, &name_token.pos);

    if config.parameters.add(name.clone(), variant_labels).is_err() {
        errors.push(Redefinition(Defineable::Parameter(name)).at(name_token.pos));
    }

    check_errors((), errors)
}

fn parse_characters(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: &FilePosition,
) -> ResultV<()> {
    let mut errors = Vec::new();

    let mut block = read_block(file, pos)?;
    while let Some(token) = block.next() {
        if token.token == RawToken::Eol {
            continue;
        }

        parse_character_def(&mut block, config, &mut errors, token);

        errors.extend(end_line(&mut block));
    }

    check_errors((), errors)
}

// WARNING: This will not clear until EOL. The calling function must ensure the
// line ends properly.
fn parse_character_def(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    errors: &mut Vec<Error>,
    char_token: Token,
) {
    let RawToken::UnmarkedIdentifier(char) = char_token.token else {
        errors.push(unexpect(char_token, Expectation::Identifier));
        return;
    };

    if config.characters.contains_key(&char) {
        errors.push(Redefinition(Defineable::Character(char.clone())).at(char_token.pos.clone()));
    }

    let phoneme = parse_character_def_phoneme(file, config, errors, &char_token.pos);
    config.characters.insert(
        char.clone(),
        Character::new(CharacterDefinition::new(char, phoneme)),
    );
}

fn parse_character_def_phoneme(
    file: &mut impl Iterator<Item = Token>,
    config: &Config,
    errors: &mut Vec<Error>,
    pos: &FilePosition,
) -> Phoneme {
    if let Err(e) = confirm_token_type(file, RawToken::PhonemeOpen, pos) {
        errors.push(e);
        return Phoneme::new(None);
    }

    match parse_phoneme(file, config) {
        Err(e) => {
            errors.extend(e);
            Phoneme::new(None)
        }
        Ok(phoneme) => phoneme,
    }
}

fn parse_diacritics(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: &FilePosition,
) -> ResultV<()> {
    let mut errors = Vec::new();

    let mut block = read_block(file, pos)?;
    while let Some(token) = block.next() {
        if token.token == RawToken::Eol {
            continue;
        }

        let next_def_res = parse_diacritic_def(&mut block, config, token);
        if let Err(mut errs) = next_def_res {
            errors.append(&mut errs);
        }

        errors.extend(end_line(&mut block));
    }

    check_errors((), errors)
}

// WARNING: Like parse_character_def(), this will not clear until EOL. The
// calling function must ensure the line ends properly.
fn parse_diacritic_def(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    dia_token: Token,
) -> ResultV<()> {
    let RawToken::UnmarkedIdentifier(def_str) = dia_token.token else {
        return Err(unexpect(dia_token, Expectation::Identifier));
    };

    let diacritic = def_str.chars().last().expect("empty identifier");

    let Some(next_token) = file.next() else {
        return Err(eof_error(dia_token.pos, Expectation::Attribute));
    };
    let attribute = parse_attribute(config, next_token, true)?;

    config
        .diacritics
        .add(diacritic, attribute)
        .map_err(|()| Redefinition(Defineable::Diacritic(diacritic)).at(dia_token.pos))
}

fn parse_evolve(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: &FilePosition,
) -> ResultV<()> {
    let input_language = read_language(file, config, pos)?;

    confirm_token_type(file, RawToken::To, pos)?;

    let output_language = read_language(file, config, pos)?;

    let mut errors = Vec::new();
    let mut rules = Vec::new();
    let mut block = read_block(file, pos)?.peekable();
    while block.peek().is_some() {
        let mut line_iter = (&mut block).take_while(|t| t.token != RawToken::Eol);

        match parse_evolution_rule(&mut line_iter, config, pos) {
            Ok(rule) => rules.push(rule),
            Err(mut errs) => errors.append(&mut errs),
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    let already_defined = config
        .evolutions
        .get(&input_language)
        .and_then(|le| le.get(&output_language))
        .is_some();
    if already_defined {
        return Err(Redefinition(Defineable::Evolution(
            input_language.to_string(),
            output_language.to_string(),
        ))
        .at(pos.clone()));
    }

    config
        .evolutions
        .entry(input_language)
        .or_default()
        .insert(output_language, rules);

    Ok(())
}

fn read_language(
    file: &mut impl Iterator<Item = Token>,
    config: &Config,
    pos: &FilePosition,
) -> Result<Label> {
    let Some(language_token) = file.next() else {
        return Err(eof_error(pos.clone(), Expectation::Identifier));
    };

    let RawToken::UnmarkedIdentifier(language_name) = language_token.token else {
        return Err(unexpect(language_token, Expectation::Identifier));
    };

    match config.languages.encode(&language_name) {
        Some(label) => Ok(label),
        None => Err(Undefined(Defineable::Language(language_name)).at(language_token.pos)),
    }
}

fn parse_evolution_rule(
    file: &mut impl Iterator<Item = Token>,
    config: &Config,
    pos: &FilePosition,
) -> ResultV<Rule> {
    let mut errors = Vec::new();

    let mut input: Vec<InputAtom> = Vec::new();
    let mut input_iter = file.take_while(|t| t.token != RawToken::Output);
    while let Some(token) = input_iter.next() {
        match token.token {
            RawToken::SelectorOpen(code) => match parse_selector(&mut input_iter, config, code) {
                Ok(selector) => input.push(InputAtom::Selector(selector)),
                Err(mut errs) => errors.append(&mut errs),
            },
            RawToken::FilterOpen => match parse_filter(&mut input_iter, config) {
                Ok(filter) => input.push(InputAtom::Filter(filter)),
                Err(mut errs) => errors.append(&mut errs),
            },
            RawToken::PhonemeOpen => match parse_phoneme(&mut input_iter, config) {
                Ok(phoneme) => input.push(InputAtom::Phoneme(phoneme)),
                Err(mut errs) => errors.append(&mut errs),
            },
            RawToken::UnmarkedIdentifier(ident) => match parse_character(config, &ident, pos) {
                Ok(phoneme) => input.push(InputAtom::Phoneme(phoneme)),
                Err(mut errs) => errors.append(&mut errs),
            },
            _ => errors.push(unexpect(token, Expectation::InputAtom)),
        }
    }

    let mut do_environment = false;
    let mut output: Vec<UnboundPhoneme> = Vec::new();
    let mut output_iter = file.take_while_inclusive(|t| t.token != RawToken::Environment);
    while let Some(token) = output_iter.next() {
        match token.token {
            RawToken::UnmarkedIdentifier(ident) => {
                match parse_character(config, &ident, &token.pos) {
                    Ok(phoneme) => output.push(phoneme),
                    Err(mut err) => errors.append(&mut err),
                }
            }
            RawToken::PhonemeOpen => match parse_attribute_list(&mut output_iter, config, true) {
                Ok(attrs) => output.push(UnboundPhoneme::from_iter(attrs)),
                Err(mut errs) => errors.append(&mut errs),
            },
            RawToken::Environment => {
                do_environment = true;
                break;
            }
            _ => errors.push(unexpect(token, Expectation::OutputAtom)),
        }
    }

    let environment = if do_environment {
        match parse_evolution_environment(file, config, pos) {
            Ok(env) => Some(env),
            Err(mut errs) => {
                errors.append(&mut errs);
                return Err(errors);
            }
        }
    } else {
        None
    };

    check_errors(
        Rule {
            input,
            output,
            environment,
        },
        errors,
    )
}

fn parse_evolution_environment(
    file: &mut impl Iterator<Item = Token>,
    config: &Config,
    pos: &FilePosition,
) -> ResultV<Environment> {
    let mut errors = Vec::new();

    let mut tokens: Vec<Token> = file.take_while(|tok| tok.token != RawToken::Eol).collect();
    let last_pos = tokens.last().map_or(pos, |tok| &tok.pos).clone();

    let match_start = tokens
        .first()
        .is_some_and(|tok| tok.token == RawToken::WordBoundry);
    if match_start {
        tokens.remove(0);
    }

    let match_end = tokens
        .last()
        .is_some_and(|tok| tok.token == RawToken::WordBoundry);
    if match_end {
        tokens.pop();
    }

    let mut splitting_iter = tokens.into_iter().peekable();

    let raw_pre: Vec<_> = splitting_iter
        .peeking_take_while(|tok| tok.token != RawToken::Target)
        .collect();
    let pre = parse_single_environment(raw_pre, config, &mut errors);

    match splitting_iter.next() {
        Some(Token {
            token: RawToken::Target,
            ..
        }) => (),
        Some(token) => errors.push(unexpect(token, RawToken::Target)),
        None => errors.push(eof_error(last_pos, RawToken::Target)),
    }

    let raw_post: Vec<_> = splitting_iter.collect();
    let post = parse_single_environment(raw_post, config, &mut errors);

    let environment = Environment {
        match_start,
        match_end,
        pre,
        post,
    };

    check_errors(environment, errors)
}

// Bit of an odd approach, instead of directly returning a Result, this will put
// all of its generated errors into the vector passed in `errors`, and then
// return whatever atoms it could scrounge together successfully
fn parse_single_environment(
    tokens: Vec<Token>,
    config: &Config,
    errors: &mut Vec<Error>,
) -> Vec<EnvironmentAtom> {
    let mut atoms = Vec::new();
    let mut iter = tokens.into_iter();

    while let Some(next) = parse_environment_atom(&mut iter, &mut atoms, config) {
        match next {
            Ok(atom) => atoms.push(atom),
            Err(mut errs) => errors.append(&mut errs),
        }
    }

    atoms
}

fn parse_environment_atom(
    tokens: &mut impl Iterator<Item = Token>,
    previous: &mut Vec<EnvironmentAtom>,
    config: &Config,
) -> Option<ResultV<EnvironmentAtom>> {
    use EnvironmentAtom::*;

    let token = tokens.next()?;

    Some(match token.token {
        RawToken::PhonemeOpen => parse_phoneme(tokens, config).map(Phoneme),
        RawToken::FilterOpen => parse_filter(tokens, config).map(Filter),
        RawToken::UnmarkedIdentifier(ident) => {
            parse_character(config, &ident, &token.pos).map(Phoneme)
        }
        RawToken::Optional => {
            bundle_special_atom(previous, Optional, RawToken::Optional, token.pos)
        }
        RawToken::ZeroOrMore => {
            bundle_special_atom(previous, ZeroOrMore, RawToken::ZeroOrMore, token.pos)
        }
        RawToken::Not => bundle_special_atom(previous, Not, RawToken::Not, token.pos),

        RawToken::WordBoundry => Err(MisplacedWordBoundary.at(token.pos)),
        RawToken::Target => Err(ExcessTargets.at(token.pos)),
        _ => Err(unexpect(token, Expectation::EnvironmentAtom)),
    })
}

fn bundle_special_atom(
    previous: &mut Vec<EnvironmentAtom>,
    bundler: impl Fn(Box<EnvironmentAtom>) -> EnvironmentAtom,
    raw: RawToken,
    pos: FilePosition,
) -> ResultV<EnvironmentAtom> {
    let Some(argument) = previous.pop() else {
        return Err(InvalidSpecialAtom(raw).at(pos));
    };

    Ok(bundler(argument.into()))
}

// Several syntax elements are composed of a block of lines containing a single
// identifier, such as when defining languages, features, and parameters.
// This will parse an identifier block and then return a vector.
fn parse_identifier_block(
    file: &mut impl Iterator<Item = Token>,
    errors: &mut Vec<Error>,
    pos: &FilePosition,
) -> Vec<String> {
    let mut identifiers: Vec<String> = Vec::new();

    let mut block = match read_block(file, pos) {
        Ok(it) => it,
        Err(e) => {
            errors.push(e);
            return Vec::default();
        }
    };
    while let Some(token) = block.next() {
        match token.token {
            RawToken::Eol => continue,
            RawToken::UnmarkedIdentifier(ident) => identifiers.push(ident),
            _ => errors.push(unexpect(token, Expectation::Identifier)),
        }

        errors.extend(end_line(&mut block));
    }

    identifiers
}

fn parse_phoneme(file: &mut impl Iterator<Item = Token>, config: &Config) -> ResultV<Phoneme> {
    let attributes = parse_attribute_list(file, config, true)?;

    Ok(Phoneme::from_iter(attributes))
}

fn parse_selector(
    file: &mut impl Iterator<Item = Token>,
    config: &Config,
    code: SelectorCode,
) -> ResultV<Selector> {
    Ok(Selector {
        code,
        filter: parse_filter(file, config)?,
    })
}

fn parse_filter(file: &mut impl Iterator<Item = Token>, config: &Config) -> ResultV<Filter> {
    let attributes = parse_attribute_list(file, config, false)?;

    Ok(Filter::from_iter(attributes))
}

// This parses the internals of phonemes, selectors, and filters into a list of
// attributes that other functions can process
fn parse_attribute_list(
    file: &mut impl Iterator<Item = Token>,
    config: &Config,
    is_phoneme: bool,
) -> ResultV<Vec<Attribute>> {
    let close_token = if is_phoneme {
        RawToken::PhonemeClose
    } else {
        RawToken::FilterSelectorClose
    };

    let (attributes, errors): (Vec<_>, Vec<_>) = file
        .take_while(|token| token.token != close_token)
        .map(|token| parse_attribute(config, token, !is_phoneme))
        .partition_result();

    check_errors(attributes, errors.into_iter().flatten().collect_vec())
}

fn parse_attribute(config: &Config, token: Token, allow_neg_param: bool) -> ResultV<Attribute> {
    let res = match token.token {
        RawToken::MarkedFeature(mark, feat) => parse_feature(config, mark, feat, &token.pos)?,
        RawToken::MarkedParameter(mark, param, variant) => {
            parse_parameter(config, mark, param, variant, &token.pos, allow_neg_param)?
        }
        RawToken::UnmarkedIdentifier(character) => {
            Attribute::Phoneme(parse_character(config, &character, &token.pos)?)
        }
        RawToken::SelectorCode(code) => Attribute::Selection(code),
        _ => return Err(unexpect(token, Expectation::Attribute)),
    };
    Ok(res)
}

fn parse_feature(
    config: &Config,
    mark: bool,
    feature: String,
    pos: &FilePosition,
) -> Result<Attribute> {
    match config.features.encode(&feature) {
        Some(label) => Ok(Attribute::Feature(mark, label)),
        None => Err(Undefined(Defineable::Feature(feature)).at(pos.clone())),
    }
}

fn parse_parameter(
    config: &Config,
    mark: bool,
    parameter: String,
    variant: String,
    pos: &FilePosition,
    allow_neg_param: bool,
) -> Result<Attribute> {
    if !allow_neg_param && !mark {
        return Err(NegativeParameterInPhoneme.at(pos.clone()));
    }

    match config.parameters.encode(&parameter, &variant) {
        Some((p_label, Some(v_label))) => Ok(Attribute::Parameter(mark, p_label, v_label)),
        Some((_, None)) => {
            Err(Undefined(Defineable::ParameterVariant(parameter, variant)).at(pos.clone()))
        }
        None => Err(Undefined(Defineable::Parameter(parameter)).at(pos.clone())),
    }
}

fn parse_character<T: FromIterator<Attribute>>(
    config: &Config,
    identifier: &str,
    pos: &FilePosition,
) -> ResultV<T> {
    let mut ident_chars = identifier.chars().peekable();
    let mut attrs = Vec::new();

    let base_string: String = ident_chars
        .peeking_take_while(|&next| config.diacritics.decode(next).is_none())
        .collect();
    let base = parse_base_character(config, base_string, pos)?;
    attrs.push(base);

    let (diacritics, errors): (Vec<_>, Vec<_>) = ident_chars
        .map(|dia| {
            config
                .diacritics
                .decode(dia)
                .cloned()
                .ok_or_else(|| Undefined(Defineable::Diacritic(dia)).at(pos.clone()))
        })
        .partition_result();
    check_errors((), errors)?;
    attrs.extend(diacritics);

    Ok(FromIterator::from_iter(attrs))
}

fn parse_base_character(
    config: &Config,
    character: String,
    pos: &FilePosition,
) -> Result<Attribute> {
    match config.characters.get(&character) {
        Some(phoneme) => Ok(Attribute::Character(phoneme.to_owned())),
        None => Err(Undefined(Defineable::Character(character)).at(pos.clone())),
    }
}

// Confirms that the next token is of a specific type, returning a specified
// error if it is not.
// This function will consume the next token, regardless of what it is.
// If FileLexer gives an IO error, this function will consume and return it
fn confirm_token_type(
    file: &mut impl Iterator<Item = Token>,
    desired: RawToken,
    pos: &FilePosition,
) -> Result<()> {
    let Some(token) = file.next() else {
        return Err(eof_error(pos.clone(), desired));
    };

    if token.token == desired {
        Ok(())
    } else {
        Err(unexpect(token, desired))
    }
}

// Used for ensuring that lines are finished when they are supposed to be.
// Goes until it consumes an EOL, and then marks every found token an error.
fn end_line(file: &mut impl Iterator<Item = Token>) -> impl Iterator<Item = Error> {
    file.take_while(|token| token.token != RawToken::Eol)
        .map(|token| unexpect(token, RawToken::Eol))
}

fn read_block(
    file: &mut impl Iterator<Item = Token>,
    pos: &FilePosition,
) -> Result<impl Iterator<Item = Token>> {
    confirm_token_type(file, RawToken::BlockOpen, pos)?;

    let mut nesting_level = 1;
    Ok(file.take_while(move |token| {
        match token.token {
            RawToken::BlockClose => nesting_level -= 1,
            RawToken::BlockOpen => nesting_level += 1,
            _ => (),
        }
        nesting_level > 0
    }))
}
