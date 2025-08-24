use itertools::Itertools;

use crate::config::{Config, LabelEncoding};
use crate::error_handling::{Error, Position};
use crate::phonemes::{Attribute, Filter, Phoneme, Selector, SelectorCode, UnboundPhoneme};

use super::lexer::{FileLexer, RawToken, Token};
use super::{PResult, PResultV, ParseErrorType::*};

pub fn parse_config(file: &mut FileLexer) -> PResultV<Config> {
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

    if errors.is_empty() {
        Ok(config)
    } else {
        Err(errors)
    }
}

fn parse_languages(file: &mut FileLexer, config: &mut Config, pos: Position) -> PResultV<()> {
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, pos)?;
    let languages = parse_identifier_block(file)?;

    for language in languages {
        config.languages.add(language);
    }

    Ok(())
}

fn parse_features(file: &mut FileLexer, config: &mut Config, pos: Position) -> PResultV<()> {
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, pos)?;
    let features = parse_identifier_block(file)?;

    for feature in features {
        config.features.add(feature);
    }

    Ok(())
}

fn parse_parameters(file: &mut FileLexer, config: &mut Config, pos: Position) -> PResultV<()> {
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, pos)?;

    let mut errors: Vec<Error<super::ParseErrorType>> = Vec::new();

    while let Some(token) = file.next().transpose()? {
        if token.token == RawToken::BlockClose {
            break;
        }

        let next_def_res = parse_parameter_def(file, config, token);
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

fn parse_parameter_def(
    file: &mut FileLexer,
    config: &mut Config,
    name_token: Token,
) -> PResultV<()> {
    let name: String;
    match name_token.token {
        RawToken::EOL => return Ok(()),
        RawToken::UnmarkedIdentifier(ident) => name = ident,
        _ => return Err(ExpectedIdentifier.at(name_token.pos).into()),
    }

    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, name_token.pos)?;
    let values = parse_identifier_block(file)?;
    let mut value_labels = LabelEncoding::new();
    for value in values {
        value_labels.add(value);
    }

    let new_code = config.parameters.add(name);
    config.parameter_values.insert(new_code, value_labels);

    Ok(())
}

fn parse_characters(file: &mut FileLexer, config: &mut Config, pos: Position) -> PResultV<()> {
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, pos)?;

    let mut errors: Vec<Error<super::ParseErrorType>> = Vec::new();

    while let Some(token) = file.next().transpose()? {
        if token.token == RawToken::BlockClose {
            break;
        }

        let next_def_res = parse_character_def(file, config, token);
        if let Err(mut errs) = next_def_res {
            errors.append(&mut errs);
        }

        if let Err(mut errs) = end_line(file) {
            errors.append(&mut errs);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

// WARNING: This will not clear until EOL, the calling function must ensure the
// line ends properly
fn parse_character_def(
    file: &mut FileLexer,
    config: &mut Config,
    char_token: Token,
) -> PResultV<()> {
    let char: String;
    match char_token.token {
        RawToken::EOL => return Ok(()),
        RawToken::UnmarkedIdentifier(ident) => char = ident,
        _ => return Err(ExpectedIdentifier.at(char_token.pos).into()),
    }

    confirm_token_type(file, RawToken::PhonemeOpen, ExpectedPhoneme, char_token.pos)?;
    let phoneme = parse_phoneme(file, config)?;

    if config.characters.insert(char, phoneme).is_some() {
        return Err(Redefinition.at(char_token.pos).into());
    }

    Ok(())
}

fn parse_evolve(file: &mut FileLexer, _: &mut Config, _: Position) -> PResultV<()> {
    // todo!("woopsy")

    while let Some(token) = file.next().transpose()? {
        if token.token == RawToken::BlockClose {
            break;
        }
    }

    Ok(())
}

// Several syntax elements are composed of a block of lines containing a single
// identifier, such as when defining languages, features, and parameters.
// This will parse an identifier block and then return a vector.
// TODO: Currently this won't error if the file ends while the block is open
fn parse_identifier_block(file: &mut FileLexer) -> PResultV<Vec<String>> {
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

    if errors.is_empty() {
        Ok(identifiers)
    } else {
        Err(errors)
    }
}

fn parse_phoneme(file: &mut FileLexer, config: &mut Config) -> PResultV<Phoneme> {
    let attributes = parse_attribute_list(file, config, true)?;

    let mut phoneme = Phoneme::new();
    for attribute in attributes {
        phoneme.add_attribute(attribute);
    }

    Ok(phoneme)
}

fn parse_selector(
    file: &mut FileLexer,
    config: &mut Config,
    code: SelectorCode,
) -> PResultV<Selector> {
    Ok(Selector {
        code,
        filter: parse_filter(file, config)?,
    })
}

fn parse_filter(file: &mut FileLexer, config: &mut Config) -> PResultV<Filter> {
    let attributes = parse_attribute_list(file, config, false)?;

    let mut filter = Filter::new();
    attributes
        .into_iter()
        .for_each(|attribute| filter.add_attribute(attribute));

    Ok(filter)
}

// This parses the internals of phonemes, selectors, and filters into a list of
// attributes that other functions can process
fn parse_attribute_list(
    file: &mut FileLexer,
    config: &mut Config,
    is_phoneme: bool,
) -> PResultV<Vec<Attribute>> {
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

    if let Some(&name_code) = config.parameters.encode(&parameter) {
        let variant_encoding = config
            .parameter_values
            .entry(name_code)
            .or_insert(LabelEncoding::new());

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
        None => Err(UndefinedCharacter(character).at(pos)),
    }
}

// Confirms that the next token is of a specific type, returning a specified
// error if it is not.
// This function will consume the next token, regardless of what it is.
// If FileLexer gives an IO error, this function will consume and return it
fn confirm_token_type(
    file: &mut FileLexer,
    desired: RawToken,
    error: super::ParseErrorType,
    pos: Position,
) -> PResult<()> {
    if let Some(token) = file.next().transpose()? {
        if token.token == desired {
            Ok(())
        } else {
            Err(error.at(token.pos))
        }
    } else {
        Err(error.at(pos))
    }
}

// Used for ensuring that lines are finished when they are supposed to be.
// Goes until it consumes an EOL, and then marks every found token an error.
fn end_line(file: &mut FileLexer) -> PResultV<()> {
    let mut errors = Vec::new();

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
