use std::iter::Peekable;

use itertools::Itertools;

use crate::config::{Config, LabelEncoding};
use crate::error_handling::{Error, Position};
use crate::phonemes::{Attribute, Filter, Phoneme, Selector, SelectorCode, UnboundPhoneme};

use super::lexer::{FileLexer, RawToken, Token};
use super::{PResult, PResultV, ParseErrorType::*};

pub fn parse_config(file: &mut Peekable<FileLexer>) -> PResultV<Config> {
    let mut config = Config::new();
    let mut errors = Vec::new();

    while let Some(token_res) = file.next() {
        let token = token_res?;
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

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(config)
    }
}

pub fn parse_languages(
    file: &mut Peekable<FileLexer>,
    config: &mut Config,
    pos: Position,
) -> PResultV<()> {
    let languages = parse_identifier_block(file, pos)?;

    for language in languages {
        config.languages.add(language);
    }

    Ok(())
}

pub fn parse_features(
    file: &mut Peekable<FileLexer>,
    config: &mut Config,
    pos: Position,
) -> PResultV<()> {
    let features = parse_identifier_block(file, pos)?;

    for feature in features {
        config.features.add(feature);
    }

    Ok(())
}

pub fn parse_parameters(
    file: &mut Peekable<FileLexer>,
    config: &mut Config,
    pos: Position,
) -> PResultV<()> {
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, &pos)?;

    let mut errors: Vec<Error<super::ParseErrorType>> = Vec::new();

    while file.peek().is_some() {
        if check_token_type(file, RawToken::BlockClose)? {
            break;
        }

        let next_def_res = parse_parameter_def(file, config, pos);
        if let Err(mut errs) = next_def_res {
            errors.append(&mut errs);
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(())
    }
}

pub fn parse_parameter_def(
    file: &mut Peekable<FileLexer>,
    config: &mut Config,
    pos: Position,
) -> PResultV<()> {
    let name: String;
    match file.next().transpose()? {
        Some(token) => match token.token {
            RawToken::EOL => return Ok(()),
            RawToken::UnmarkedIdentifier(ident) => name = ident,
            _ => return Err(ExpectedIdentifier.at(pos).into()),
        },
        None => return Ok(()),
    }

    let values = parse_identifier_block(file, pos)?;
    let mut value_labels = LabelEncoding::new();
    for value in values {
        value_labels.add(value);
    }

    let new_code = config.parameters.add(name);
    config.parameter_values.insert(new_code, value_labels);

    return Ok(());
}

pub fn parse_characters(
    file: &mut Peekable<FileLexer>,
    config: &mut Config,
    pos: Position,
) -> PResultV<()> {
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, &pos)?;

    let mut errors: Vec<Error<super::ParseErrorType>> = Vec::new();

    while file.peek().is_some() {
        if check_token_type(file, RawToken::BlockClose)? {
            break;
        }

        let next_def_res = parse_character_def(file, config, pos);
        if let Err(mut errs) = next_def_res {
            errors.append(&mut errs);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn parse_character_def(
    file: &mut Peekable<FileLexer>,
    config: &mut Config,
    pos: Position,
) -> PResultV<()> {
    let mut errors = Vec::new();

    let char: String;
    let char_pos: Position;
    match file.next().transpose()? {
        Some(token) => match token.token {
            RawToken::EOL => return Ok(()),
            RawToken::UnmarkedIdentifier(ident) => {
                char = ident;
                char_pos = token.pos;
            }
            _ => return Err(ExpectedIdentifier.at(pos).into()),
        },
        None => return Ok(()),
    }

    if let Some(token) = file.next().transpose()? {
        if token.token != RawToken::PhonemeOpen {
            errors.push(ExpectedPhoneme.at(pos));
            return Err(errors);
        }
    } else {
        errors.push(ExpectedPhoneme.at(pos));
        return Err(errors);
    }
    let phoneme = parse_phoneme(file, config)?;

    if let Some(_) = config.characters.insert(char, phoneme) {
        errors.push(Redefinition.at(char_pos));
    }

    while let Some(token) = file.next().transpose()? {
        if token.token == RawToken::EOL {
            break;
        } else {
            errors.push(ExpectedEOL.at(token.pos));
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn parse_evolve(file: &mut Peekable<FileLexer>, _: &mut Config, _: Position) -> PResultV<()> {
    // todo!("woopsy")

    while !check_token_type(file, RawToken::BlockClose)? {
        file.next();
    }

    Ok(())
}

// Several syntax elements are composed of a block of lines containing a single
// identifier, such as when defining languages, features, and parameters.
// This will parse an identifier block and then return a vector.
// TODO: Currently this won't error if the file ends while the block is open
pub fn parse_identifier_block(
    file: &mut Peekable<FileLexer>,
    pos: Position,
) -> PResultV<Vec<String>> {
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, &pos)?;

    let mut identifiers: Vec<String> = Vec::new();
    let mut errors: Vec<Error<super::ParseErrorType>> = Vec::new();

    let mut empty_line = true;
    while let Some(token_res) = file.next() {
        let token = token_res?;
        let pos = token.pos;

        // If we encounter the end of the block, we are done: end immediately.
        // If we EOL, then reset the empty_line flag.
        // If the line is not empty, then the only valid choice is EOL, which
        //     was not seen: throw an error.
        // We now know that we are on an empty line, so if we encounter an
        //     unmarked identifier, then perfect: add it to the vector.
        // If it was some other unknown token, then throw an error.
        if token.token == RawToken::BlockClose {
            break;
        } else if token.token == RawToken::EOL {
            empty_line = true;
        } else if !empty_line {
            errors.push(ExpectedEOL.at(pos));
        } else if let RawToken::UnmarkedIdentifier(ident) = token.token {
            identifiers.push(ident);
            empty_line = false;
        } else {
            errors.push(ExpectedIdentifier.at(pos));
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(identifiers)
    }
}

pub fn parse_phoneme(file: &mut Peekable<FileLexer>, config: &mut Config) -> PResultV<Phoneme> {
    let attributes = parse_attribute_list(file, config, true)?;

    let mut phoneme = Phoneme::new();
    attributes.into_iter().for_each(|attribute| phoneme.add_attribute(attribute));

    Ok(phoneme)
}

pub fn parse_selector(file: &mut Peekable<FileLexer>, config: &mut Config, code: SelectorCode) -> PResultV<Selector> {
    Ok(Selector {
        code,
        filter: parse_filter(file, config)?,
    })
}

pub fn parse_filter(file: &mut Peekable<FileLexer>, config: &mut Config) -> PResultV<Filter> {
    let attributes = parse_attribute_list(file, config, false)?;

    let mut filter = Filter::new();
    attributes.into_iter().for_each(|attribute| filter.add_attribute(attribute));

    Ok(filter)
}

// This parses the internals of phonemes, selectors, and filters into a list of
// attributes that other functions can process
fn parse_attribute_list(file: &mut Peekable<FileLexer>, config: &mut Config, is_phoneme: bool) -> PResultV<Vec<Attribute>> {
    let close_token = match is_phoneme {
        true => RawToken::PhonemeClose,
        false => RawToken::FilterSelectorClose,
    };

    let (attributes, errors): (Vec<_>, Vec<_>) = file.process_results(|block| {
        block
            .take_while(|token| token.token != close_token)
            .map(|token| parse_attribute(config, token, !is_phoneme))
            .partition_result()
    })?;

    if errors.is_empty() {
        Ok(attributes)
    } else {
        Err(errors)
    }
}

fn parse_attribute(config: &mut Config, token: Token, allow_neg_param: bool) -> PResult<Attribute> {
    match token.token {
        RawToken::MarkedFeature(mark, feat) => parse_feature(config, mark, feat, token.pos),
        RawToken::MarkedParameter(mark, param, variant) => parse_parameter(config, mark, param, variant, token.pos, allow_neg_param),
        RawToken::UnmarkedIdentifier(character) => parse_character(config, character, token.pos),
        RawToken::SelectorCode(code) => Ok(Attribute::Selection(code)),
        unexpected => Err(UnexpectedToken(unexpected).at(token.pos)),
    }
}

fn parse_feature(config: &mut Config, mark: bool, feature: String, pos: Position) -> PResult<Attribute> {
    match config.features.encode(&feature) {
        Some(&code) => Ok(Attribute::Feature(mark, code)),
        None => Err(UndefinedFeature(feature).at(pos)),
    }
}

fn parse_parameter(config: &mut Config, mark: bool, parameter: String, variant: String, pos: Position, allow_neg_param: bool) -> PResult<Attribute> {
    if !allow_neg_param && !mark {
        return Err(NegativeParameterInPhoneme.at(pos));
    }

    if let Some(&name_code) = config.parameters.encode(&parameter) {
        let variant_encoding = config.parameter_values.entry(name_code).or_insert(LabelEncoding::new());

        if let Some(&variant_code) = variant_encoding.encode(&variant) {
            Ok(Attribute::Parameter(mark, name_code, variant_code))
        } else {
            Err(UndefinedParameterVariant(parameter, variant).at(pos))
        }
    } else {
        Err(UndefinedParameter(parameter).at(pos))
    }
}

fn parse_character(config: &mut Config, character: String, pos: Position) -> PResult<Attribute> {
    match config.characters.get(&character) {
        Some(phoneme) => Ok(Attribute::Character(phoneme.to_owned())),
        None => Err(UndefinedCharacter(character).at(pos))
    }
}

// Confirms that the next token is of a specific type, returning a specified
// error if it is not.
// If the correct token is found, this function will consume it and return ()
// If FileLexer gives an IO error, this function will consume and return it
pub fn confirm_token_type(
    file: &mut Peekable<FileLexer>,
    desired: RawToken,
    error: super::ParseErrorType,
    pos: &Position,
) -> PResult<()> {
    if !check_token_type(file, desired)? {
        Err(error.at(*pos))
    } else {
        Ok(())
    }
}

// Checks whether the next token is of a specific type.
// If the correct token is found, this function will consume it and return true.
// If FileLexer gives an IO error, this function will consume and return it.
pub fn check_token_type(file: &mut Peekable<FileLexer>, desired: RawToken) -> PResult<bool> {
    if let Some(token) = file.peek() {
        if token.is_err() {
            // we can just unwrap here because we already know that it is Some
            let fl_error = file.next().unwrap().unwrap_err();
            return Err(fl_error);
        }

        if token.as_ref().unwrap().token == desired {
            file.next();
            return Ok(true);
        }
    }

    Ok(false)
}
