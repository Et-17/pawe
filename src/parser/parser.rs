use std::iter::Peekable;

use crate::config::{ConcretePhoneme, Config, LabelEncoding};
use crate::error_handling::{Error, Position};

use super::lexer::{FileLexer, RawToken};
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
        if token.token != RawToken::ConcretePhonemeOpen {
            errors.push(ExpectedPhoneme.at(pos));
            return Err(errors);
        }
    } else {
        errors.push(ExpectedPhoneme.at(pos));
        return Err(errors);
    }
    let phoneme = parse_concrete_phoneme(file, config)?;

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

pub fn parse_evolve(
    file: &mut Peekable<FileLexer>,
    config: &mut Config,
    pos: Position,
) -> PResultV<()> {
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

// This function assumes that the opening [ has already been consumed
pub fn parse_concrete_phoneme(
    file: &mut Peekable<FileLexer>,
    config: &mut Config,
) -> PResultV<ConcretePhoneme> {
    let mut phoneme = ConcretePhoneme::new();
    let mut errors: Vec<Error<super::ParseErrorType>> = Vec::new();

    while let Some(token) = file.next().transpose()? {
        match token.token {
            RawToken::ConcretePhonemeClose => break,
            RawToken::MarkedFeature(mark, feat) => {
                if let Some(code) = config.features.encode(&feat) {
                    phoneme.features.insert(*code, mark);
                } else {
                    errors.push(UndefinedFeature(feat).at(token.pos));
                }
            }
            RawToken::MarkedParameter(true, param, variant) => {
                match encode_parameter(config, &param, &variant, token.pos) {
                    Ok((p_code, v_code)) => {
                        phoneme.parameters.insert(p_code, v_code);
                    }
                    Err(e) => errors.push(e),
                }
            }
            RawToken::MarkedParameter(false, _, _) => {
                errors.push(NegativeParameterInConcrete.at(token.pos))
            }
            unknown => {
                errors.push(UnexpectedToken(unknown).at(token.pos));
            }
        };
    }

    if errors.is_empty() {
        Ok(phoneme)
    } else {
        Err(errors)
    }
}

pub fn encode_parameter(
    config: &mut Config,
    parameter: &String,
    variant: &String,
    pos: Position,
) -> PResult<(u32, u32)> {
    if let Some(&name_code) = config.parameters.encode(parameter) {
        let variant_encoding = config
            .parameter_values
            .entry(name_code)
            .or_insert(LabelEncoding::new());
        if let Some(&variant_code) = variant_encoding.encode(variant) {
            Ok((name_code, variant_code))
        } else {
            Err(UndefinedParameterVariant(parameter.to_owned(), variant.to_owned()).at(pos))
        }
    } else {
        Err(UndefinedParameter(parameter.to_owned()).at(pos))
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
