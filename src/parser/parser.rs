use itertools::Itertools;

use crate::config::{Config, LabelEncoding};
use crate::error_handling::{Error, Position};
use crate::evolution::Rule;
use crate::phonemes::{Attribute, Filter, Phoneme, Selector, SelectorCode, UnboundPhoneme};

use super::lexer::{FileLexer, RawToken, Token};
use super::{PResult, PResultV, ParseErrorType::*};

pub fn parse_config_file(file: &mut FileLexer) -> PResultV<Config> {
    file.process_results(|mut tokens| parse_config(&mut tokens))?
}

fn parse_config(file: &mut impl Iterator<Item = Token>) -> PResultV<Config> {
    let mut config = Config::new();
    let mut errors = Vec::new();

    while let Some(token) = file.next() {
        let pos = token.pos;

        let parsing_result = match token.token {
            RawToken::Languages => parse_languages(file, &mut config, pos),
            RawToken::Features => parse_features(file, &mut config, pos),
            RawToken::Parameters => parse_parameters(file, &mut config, pos),
            RawToken::Characters => parse_characters(file, &mut config, pos),
            RawToken::Evolve => parse_evolve(file, &mut config, pos),
            _ => Err(ExpectedBlockIdentifier.at(pos).into()),
        };

        if let Err(mut errs) = parsing_result {
            errors.append(&mut errs);
        }
    }

    check_error_vec(config, errors)
}

fn parse_languages(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: Position,
) -> PResultV<()> {
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, pos)?;
    let languages = parse_identifier_block(file)?;

    config.languages.extend(languages);

    Ok(())
}

fn parse_features(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: Position,
) -> PResultV<()> {
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, pos)?;
    let features = parse_identifier_block(file)?;

    config.features.extend(features);

    Ok(())
}

fn parse_parameters(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: Position,
) -> PResultV<()> {
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, pos)?;

    let mut errors: Vec<Error<super::ParseErrorType>> = Vec::new();

    while let Some(token) = file.next() {
        if token.token == RawToken::BlockClose {
            break;
        } else if token.token == RawToken::EOL {
            continue;
        }

        if let Err(mut errs) = parse_parameter_def(file, config, token) {
            errors.append(&mut errs);
        }
    }

    check_error_vec((), errors)
}

fn parse_parameter_def(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    name_token: Token,
) -> PResultV<()> {
    let RawToken::UnmarkedIdentifier(name) = name_token.token else {
        return Err(ExpectedIdentifier.at(name_token.pos).into());
    };

    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, name_token.pos)?;
    let value_labels = parse_identifier_block(file)?.into();

    let new_code = config.parameters.add(name);
    config.parameter_values.insert(new_code, value_labels);

    Ok(())
}

fn parse_characters(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: Position,
) -> PResultV<()> {
    let mut errors = Vec::new();

    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, pos)?;
    let mut def_block = file.take_while(end_block);
    while let Some(token) = def_block.next() {
        if token.token == RawToken::BlockClose {
            break;
        } else if token.token == RawToken::EOL {
            continue;
        }

        let next_def_res = parse_character_def(&mut def_block, config, token);
        if let Err(mut errs) = next_def_res {
            errors.append(&mut errs);
        }

        if let Err(mut errs) = end_line(&mut def_block) {
            errors.append(&mut errs);
        }
    }

    check_error_vec((), errors)
}

// WARNING: This will not clear until EOL, the calling function must ensure the
// line ends properly
fn parse_character_def(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    char_token: Token,
) -> PResultV<()> {
    let RawToken::UnmarkedIdentifier(char) = char_token.token else {
        return Err(ExpectedIdentifier.at(char_token.pos).into());
    };

    confirm_token_type(file, RawToken::PhonemeOpen, ExpectedPhoneme, char_token.pos)?;
    let phoneme = parse_phoneme(file, config)?;

    if config.characters.insert(char, phoneme).is_some() {
        return Err(Redefinition.at(char_token.pos).into());
    }

    Ok(())
}

fn parse_evolve(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: Position,
) -> PResultV<()> {
    let (input_language_code, input_language) = read_language(file, config, pos)?;

    confirm_token_type(file, RawToken::To, ExpectedTo, pos)?;

    let (output_language_code, output_language) = read_language(file, config, pos)?;

    let mut errors = Vec::new();
    let mut rules = Vec::new();
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, pos)?;
    let mut block_iter = file.take_while(end_block).peekable();
    while let Some(_) = block_iter.peek() {
        let mut line_iter = (&mut block_iter).take_while(|t| t.token != RawToken::EOL);

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
        .get(&input_language_code)
        .and_then(|ie| ie.get(&output_language_code))
        .is_some();
    if already_defined {
        return Err(AlreadyDefinedEvolution(input_language, output_language)
            .at(pos)
            .into());
    }

    config
        .evolutions
        .entry(input_language_code)
        .or_default()
        .insert(output_language_code, rules);

    Ok(())
}

fn read_language(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: Position,
) -> PResult<(u32, String)> {
    let Some(language_token) = file.next() else {
        return Err(ExpectedLanguage.at(pos));
    };

    let RawToken::UnmarkedIdentifier(language_name) = language_token.token else {
        return Err(ExpectedLanguage.at(language_token.pos));
    };

    match config.languages.encode(&language_name) {
        Some(&x) => Ok((x, language_name)),
        None => Err(UndefinedLanguage(language_name).at(language_token.pos)),
    }
}

fn parse_evolution_rule(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: Position,
) -> PResultV<Rule> {
    file.count();
    Ok(Rule {
        input: Vec::new(),
        output: Vec::new(),
        pre_environment: Vec::new(),
        post_environment: Vec::new(),
    })
}

// Several syntax elements are composed of a block of lines containing a single
// identifier, such as when defining languages, features, and parameters.
// This will parse an identifier block and then return a vector.
fn parse_identifier_block(file: &mut impl Iterator<Item = Token>) -> PResultV<Vec<String>> {
    let mut identifiers: Vec<String> = Vec::new();
    let mut errors: Vec<Error<super::ParseErrorType>> = Vec::new();

    // If we encounter the end of the block, we are done: end immediately.
    // If we EOL, then reset the empty_line flag.
    // If the line is not empty, then the only valid choice is EOL, which
    //     was not seen: throw an error.
    // We now know that we are on an empty line, so if we encounter an
    //     unmarked identifier, then perfect: add it to the vector.
    // If it was some other unknown token, then throw an error.
    let mut empty_line = true;
    for token in file.take_while(end_block) {
        if token.token == RawToken::EOL {
            empty_line = true;
        } else if !empty_line {
            errors.push(ExpectedEOL.at(token.pos));
        } else if let RawToken::UnmarkedIdentifier(ident) = token.token {
            identifiers.push(ident);
            empty_line = false;
        } else {
            errors.push(ExpectedIdentifier.at(token.pos));
        }
    }

    check_error_vec(identifiers, errors)
}

fn parse_phoneme(file: &mut impl Iterator<Item = Token>, config: &mut Config) -> PResultV<Phoneme> {
    let attributes = parse_attribute_list(file, config, true)?;

    Ok(Phoneme::from_attributes(attributes))
}

fn parse_selector(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    code: SelectorCode,
) -> PResultV<Selector> {
    Ok(Selector {
        code,
        filter: parse_filter(file, config)?,
    })
}

fn parse_filter(file: &mut impl Iterator<Item = Token>, config: &mut Config) -> PResultV<Filter> {
    let attributes = parse_attribute_list(file, config, false)?;

    Ok(Filter::from_attributes(attributes))
}

// This parses the internals of phonemes, selectors, and filters into a list of
// attributes that other functions can process
fn parse_attribute_list(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    is_phoneme: bool,
) -> PResultV<Vec<Attribute>> {
    let close_token = match is_phoneme {
        true => RawToken::PhonemeClose,
        false => RawToken::FilterSelectorClose,
    };

    let (attributes, errors): (Vec<_>, Vec<_>) = file
        .take_while(|token| token.token != close_token)
        .map(|token| parse_attribute(config, token, !is_phoneme))
        .partition_result();

    check_error_vec(attributes, errors)
}

fn parse_attribute(config: &mut Config, token: Token, allow_neg_param: bool) -> PResult<Attribute> {
    match token.token {
        RawToken::MarkedFeature(mark, feat) => parse_feature(config, mark, feat, token.pos),
        RawToken::MarkedParameter(mark, param, variant) => {
            parse_parameter(config, mark, param, variant, token.pos, allow_neg_param)
        }
        RawToken::UnmarkedIdentifier(character) => parse_character(config, character, token.pos),
        RawToken::SelectorCode(code) => Ok(Attribute::Selection(code)),
        unexpected => Err(UnexpectedToken(unexpected).at(token.pos)),
    }
}

fn parse_feature(
    config: &mut Config,
    mark: bool,
    feature: String,
    pos: Position,
) -> PResult<Attribute> {
    match config.features.encode(&feature) {
        Some(&code) => Ok(Attribute::Feature(mark, code)),
        None => Err(UndefinedFeature(feature).at(pos)),
    }
}

fn parse_parameter(
    config: &mut Config,
    mark: bool,
    parameter: String,
    variant: String,
    pos: Position,
    allow_neg_param: bool,
) -> PResult<Attribute> {
    if !allow_neg_param && !mark {
        return Err(NegativeParameterInPhoneme.at(pos));
    }

    let Some(&name_code) = config.parameters.encode(&parameter) else {
        return Err(UndefinedParameter(parameter).at(pos));
    };

    let &variant_code = config
        .parameter_values
        .entry(name_code)
        .or_insert(LabelEncoding::new())
        .encode(&variant)
        .ok_or_else(|| UndefinedParameterVariant(parameter, variant).at(pos))?;

    Ok(Attribute::Parameter(mark, name_code, variant_code))
}

fn parse_character(config: &mut Config, character: String, pos: Position) -> PResult<Attribute> {
    match config.characters.get(&character) {
        Some(phoneme) => Ok(Attribute::Character(phoneme.to_owned())),
        None => Err(UndefinedCharacter(character).at(pos)),
    }
}

// Confirms that the next token is of a specific type, returning a specified
// error if it is not.
// This function will consume the next token, regardless of what it is.
// If FileLexer gives an IO error, this function will consume and return it
fn confirm_token_type(
    file: &mut impl Iterator<Item = Token>,
    desired: RawToken,
    error: super::ParseErrorType,
    pos: Position,
) -> PResult<()> {
    let Some(token) = file.next() else {
        return Err(error.at(pos));
    };

    if token.token == desired {
        Ok(())
    } else {
        Err(error.at(token.pos))
    }
}

// Used for ensuring that lines are finished when they are supposed to be.
// Goes until it consumes an EOL, and then marks every found token an error.
fn end_line(file: &mut impl Iterator<Item = Token>) -> PResultV<()> {
    let errors: Vec<_> = file
        .take_while(|token| token.token != RawToken::EOL)
        .map(|token| ExpectedEOL.at(token.pos))
        .collect();

    check_error_vec((), errors)
}

fn end_block(token: &Token) -> bool {
    token.token != RawToken::BlockClose
}

// If the error vector contains errors, then return those errors. Otherwise,
// return `ok`
fn check_error_vec<T, E>(ok: T, errors: Vec<E>) -> Result<T, Vec<E>> {
    if errors.is_empty() {
        Ok(ok)
    } else {
        Err(errors)
    }
}
