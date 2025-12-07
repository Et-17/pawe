mod errors;
pub mod lexer;

use itertools::Itertools;

use crate::config::{Character, CharacterDefinition, Config, Label};
use crate::error_handling::{
    Error, ErrorType, FilePosition, Result, ResultV, check_errors, wrap_io_error,
};
use crate::evolution::{Environment, EnvironmentAtom, InputAtom, Rule};
use crate::phonemes::{Attribute, Filter, Phoneme, Selector, SelectorCode, UnboundPhoneme};

use errors::ParseErrorType::*;
use lexer::{Lexer, RawToken, Token};

pub fn parse_config_file(path: std::path::PathBuf) -> ResultV<Config> {
    let file = std::fs::File::open(&path).map_err(|e| {
        let file_pos = FilePosition::new(Some(&path.as_path().into()), None, None);
        wrap_io_error("parser", Some(&file_pos))(e)
    })?;

    Lexer::lex(std::io::BufReader::new(file), Some(path))
        .process_results(|mut tokens| parse_config(&mut tokens))?
}

pub fn parse_word(word: &str, config: &mut Config) -> ResultV<Vec<Phoneme>> {
    Lexer::lex(std::io::Cursor::new(word), None)
        .process_results(|mut tokens| parse_unwrapped_word(&mut tokens, config))?
}

fn parse_unwrapped_word(
    word: &mut impl Iterator<Item = Token>,
    config: &mut Config,
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
                match parse_character(config, ident, &token.pos) {
                    Ok(phoneme) => phonemes.push(phoneme),
                    Err(mut errs) => errors.append(&mut errs),
                }
            }
            unexpected => errors.push(UnexpectedToken(unexpected).sign()),
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
            _ => Err(ExpectedBlockIdentifier.at(token.pos).into()),
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
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, pos)?;
    let languages = parse_identifier_block(file)?;

    if config.first_language.is_none() {
        config.first_language = languages.first().cloned();
    }

    if !languages.is_empty() {
        config.last_language = languages.last().cloned();
    }

    config.languages.extend(languages);

    Ok(())
}

fn parse_features(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: &FilePosition,
) -> ResultV<()> {
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, pos)?;
    let features = parse_identifier_block(file)?;

    config.features.extend(features);

    Ok(())
}

fn parse_parameters(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: &FilePosition,
) -> ResultV<()> {
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, pos)?;

    let mut errors: Vec<Error> = Vec::new();

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

    check_errors((), errors)
}

fn parse_parameter_def(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    name_token: Token,
) -> ResultV<()> {
    let RawToken::UnmarkedIdentifier(name) = name_token.token else {
        return Err(ExpectedIdentifier.at(name_token.pos).into());
    };

    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, &name_token.pos)?;
    let variant_labels = parse_identifier_block(file)?;

    if config.parameters.add(name, variant_labels).is_err() {
        return Err(Redefinition.at(name_token.pos.clone()).into());
    }

    Ok(())
}

fn parse_characters(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: &FilePosition,
) -> ResultV<()> {
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

    check_errors((), errors)
}

// WARNING: This will not clear until EOL. The calling function must ensure the
// line ends properly.
fn parse_character_def(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    char_token: Token,
) -> ResultV<()> {
    let RawToken::UnmarkedIdentifier(char) = char_token.token else {
        return Err(ExpectedIdentifier.at(char_token.pos).into());
    };

    confirm_token_type(
        file,
        RawToken::PhonemeOpen,
        ExpectedPhoneme,
        &char_token.pos,
    )?;
    let phoneme = parse_phoneme(file, config)?;

    if config
        .characters
        .insert(
            char.clone(),
            Character::new(CharacterDefinition::new(char, phoneme)),
        )
        .is_some()
    {
        return Err(Redefinition.at(char_token.pos).into());
    }

    Ok(())
}

fn parse_diacritics(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: &FilePosition,
) -> ResultV<()> {
    let mut errors = Vec::new();

    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, pos)?;
    let mut def_block = file.take_while(end_block);
    while let Some(token) = def_block.next() {
        if token.token == RawToken::EOL {
            continue;
        }

        let next_def_res = parse_diacritic_def(&mut def_block, config, token);
        if let Err(mut errs) = next_def_res {
            errors.append(&mut errs);
            // Don't throw non-line end errors if we fail early
            let _ = end_line(&mut def_block);
            continue;
        }

        if let Err(mut errs) = end_line(&mut def_block) {
            errors.append(&mut errs);
        }
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
        return Err(ExpectedIdentifier.at(dia_token.pos).into());
    };

    let Some((_, diacritic_str)) = def_str.split_at_checked(1) else {
        return Err(InvalidDiacriticDef.at(dia_token.pos).into());
    };

    if diacritic_str.chars().count() > 1 {
        return Err(DiacriticTooLong(diacritic_str.into())
            .at(dia_token.pos)
            .into());
    }
    let Some(diacritic) = diacritic_str.chars().next() else {
        return Err(InvalidDiacriticDef.at(dia_token.pos).into());
    };

    let Some(next_token) = file.next() else {
        return Err(ExpectedAttribute.at(dia_token.pos).into());
    };
    let attribute = parse_attribute(config, next_token, true)?;

    config
        .diacritics
        .add(diacritic, attribute)
        .map_err(|_| Redefinition.at(dia_token.pos).into())
}

fn parse_evolve(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: &FilePosition,
) -> ResultV<()> {
    let input_language = read_language(file, config, pos)?;

    confirm_token_type(file, RawToken::To, ExpectedTo, pos)?;

    let output_language = read_language(file, config, pos)?;

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
        .get(&input_language)
        .and_then(|le| le.get(&output_language))
        .is_some();
    if already_defined {
        return Err(AlreadyDefinedEvolution(
            input_language.to_string(),
            output_language.to_string(),
        )
        .at(pos.clone())
        .into());
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
    config: &mut Config,
    pos: &FilePosition,
) -> Result<Label> {
    let Some(language_token) = file.next() else {
        return Err(ExpectedLanguage.at(pos.clone()));
    };

    let RawToken::UnmarkedIdentifier(language_name) = language_token.token else {
        return Err(ExpectedLanguage.at(language_token.pos.clone()));
    };

    match config.languages.encode(&language_name) {
        Some(label) => Ok(label),
        None => Err(UndefinedLanguage(language_name).at(language_token.pos.clone())),
    }
}

fn parse_evolution_rule(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
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
            RawToken::UnmarkedIdentifier(ident) => match parse_character(config, ident, pos) {
                Ok(phoneme) => input.push(InputAtom::Phoneme(phoneme)),
                Err(mut errs) => errors.append(&mut errs),
            },
            unexpected => errors.push(UnexpectedToken(unexpected).at(token.pos)),
        }
    }

    let mut do_environment = false;
    let mut output: Vec<UnboundPhoneme> = Vec::new();
    let mut output_iter = file.take_while_inclusive(|t| t.token != RawToken::Environment);
    while let Some(token) = output_iter.next() {
        match token.token {
            RawToken::UnmarkedIdentifier(ident) => {
                match parse_character(config, ident, &token.pos) {
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
            unexpected => errors.push(UnexpectedToken(unexpected).at(token.pos)),
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

// This function will consume all of file. Only use on a take_while that will
// restrict it.
fn parse_evolution_environment(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    pos: &FilePosition,
) -> ResultV<Environment> {
    let mut errors = Vec::new();
    let mut last_pos = pos.clone();

    let mut pre_environment = Vec::new();
    let mut first = true;
    let mut match_word_start = false;
    loop {
        // This loop will break if it sees a target token. If it reaches the end
        // of the input before finding a target token, we know that there isn't
        // one in the environment
        let Some(token) = file.next() else {
            errors.push(MissingTarget.at(last_pos.clone()));
            return Err(errors);
        };
        last_pos = token.pos;

        match token.token {
            RawToken::PhonemeOpen => match parse_phoneme(file, config) {
                Ok(phoneme) => pre_environment.push(EnvironmentAtom::Phoneme(phoneme)),
                Err(mut errs) => errors.append(&mut errs),
            },
            RawToken::FilterOpen => match parse_filter(file, config) {
                Ok(filter) => pre_environment.push(EnvironmentAtom::Filter(filter)),
                Err(mut errs) => errors.append(&mut errs),
            },
            RawToken::UnmarkedIdentifier(ident) => {
                match parse_character(config, ident, &last_pos) {
                    Ok(phoneme) => pre_environment.push(EnvironmentAtom::Phoneme(phoneme)),
                    Err(mut errs) => errors.append(&mut errs),
                }
            }
            RawToken::Optional => {
                if !pre_environment.is_empty() {
                    let new_atom =
                        EnvironmentAtom::Optional(Box::new(pre_environment.pop().unwrap()));
                    pre_environment.push(new_atom);
                } else {
                    errors.push(MisplacedOptional.at(last_pos.clone()));
                }
            }
            RawToken::ZeroOrMore => {
                if !pre_environment.is_empty() {
                    let new_atom =
                        EnvironmentAtom::ZeroOrMore(Box::new(pre_environment.pop().unwrap()));
                    pre_environment.push(new_atom);
                } else {
                    errors.push(MisplacedZeroOrMore.at(last_pos.clone()));
                }
            }
            RawToken::Not => {
                if !pre_environment.is_empty() {
                    let new_atom = EnvironmentAtom::Not(Box::new(pre_environment.pop().unwrap()));
                    pre_environment.push(new_atom);
                } else {
                    errors.push(MisplacedNot.at(last_pos.clone()));
                }
            }
            RawToken::WordBoundry => {
                if first {
                    match_word_start = true;
                } else {
                    errors.push(MisplacedWordBoundary.at(last_pos.clone()));
                }
            }
            RawToken::Target => break,
            unexpected => errors.push(UnexpectedToken(unexpected).at(last_pos.clone())),
        }

        first = false;
    }

    let mut post_environment = Vec::new();
    let mut match_word_end_pos: Option<FilePosition> = None;
    while let Some(token) = file.next() {
        // Check if this is extra environment after matching word end
        if let Some(ref token_pos) = match_word_end_pos {
            errors.push(MisplacedWordBoundary.at(token_pos.clone()));
            continue;
        }

        match token.token {
            RawToken::PhonemeOpen => match parse_phoneme(file, config) {
                Ok(phoneme) => post_environment.push(EnvironmentAtom::Phoneme(phoneme)),
                Err(mut errs) => errors.append(&mut errs),
            },
            RawToken::FilterOpen => match parse_filter(file, config) {
                Ok(filter) => post_environment.push(EnvironmentAtom::Filter(filter)),
                Err(mut errs) => errors.append(&mut errs),
            },
            RawToken::UnmarkedIdentifier(ident) => {
                match parse_character(config, ident, &token.pos) {
                    Ok(phoneme) => post_environment.push(EnvironmentAtom::Phoneme(phoneme)),
                    Err(mut errs) => errors.append(&mut errs),
                }
            }
            RawToken::Optional => {
                if !post_environment.is_empty() {
                    let new_atom =
                        EnvironmentAtom::Optional(Box::new(post_environment.pop().unwrap()));
                    post_environment.push(new_atom);
                } else {
                    errors.push(MisplacedOptional.at(token.pos));
                }
            }
            RawToken::ZeroOrMore => {
                if !post_environment.is_empty() {
                    let new_atom =
                        EnvironmentAtom::ZeroOrMore(Box::new(post_environment.pop().unwrap()));
                    post_environment.push(new_atom);
                } else {
                    errors.push(MisplacedZeroOrMore.at(token.pos));
                }
            }
            RawToken::Not => {
                if !post_environment.is_empty() {
                    let new_atom =
                        EnvironmentAtom::ZeroOrMore(Box::new(post_environment.pop().unwrap()));
                    post_environment.push(new_atom);
                } else {
                    errors.push(MisplacedNot.at(token.pos));
                }
            }
            RawToken::WordBoundry => match_word_end_pos = Some(token.pos),
            RawToken::Target => errors.push(MultipleTargets.at(token.pos)),
            unexpected => errors.push(UnexpectedToken(unexpected).at(token.pos)),
        }
    }

    let environment = Environment {
        match_word_start,
        match_word_end: match_word_end_pos.is_some(),
        pre_environment,
        post_environment,
    };

    check_errors(environment, errors)
}

// Several syntax elements are composed of a block of lines containing a single
// identifier, such as when defining languages, features, and parameters.
// This will parse an identifier block and then return a vector.
fn parse_identifier_block(file: &mut impl Iterator<Item = Token>) -> ResultV<Vec<String>> {
    let mut identifiers: Vec<String> = Vec::new();
    let mut errors: Vec<Error> = Vec::new();

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

    check_errors(identifiers, errors)
}

fn parse_phoneme(file: &mut impl Iterator<Item = Token>, config: &mut Config) -> ResultV<Phoneme> {
    let attributes = parse_attribute_list(file, config, true)?;

    Ok(Phoneme::from_iter(attributes))
}

fn parse_selector(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    code: SelectorCode,
) -> ResultV<Selector> {
    Ok(Selector {
        code,
        filter: parse_filter(file, config)?,
    })
}

fn parse_filter(file: &mut impl Iterator<Item = Token>, config: &mut Config) -> ResultV<Filter> {
    let attributes = parse_attribute_list(file, config, false)?;

    Ok(Filter::from_iter(attributes))
}

// This parses the internals of phonemes, selectors, and filters into a list of
// attributes that other functions can process
fn parse_attribute_list(
    file: &mut impl Iterator<Item = Token>,
    config: &mut Config,
    is_phoneme: bool,
) -> ResultV<Vec<Attribute>> {
    let close_token = match is_phoneme {
        true => RawToken::PhonemeClose,
        false => RawToken::FilterSelectorClose,
    };

    let (attributes, errors): (Vec<_>, Vec<_>) = file
        .take_while(|token| token.token != close_token)
        .map(|token| parse_attribute(config, token, !is_phoneme))
        .partition_result();

    check_errors(attributes, errors.into_iter().flatten().collect_vec())
}

fn parse_attribute(config: &mut Config, token: Token, allow_neg_param: bool) -> ResultV<Attribute> {
    let res = match token.token {
        RawToken::MarkedFeature(mark, feat) => parse_feature(config, mark, feat, &token.pos)?,
        RawToken::MarkedParameter(mark, param, variant) => {
            parse_parameter(config, mark, param, variant, &token.pos, allow_neg_param)?
        }
        RawToken::UnmarkedIdentifier(character) => {
            Attribute::Phoneme(parse_character(config, character, &token.pos)?)
        }
        RawToken::SelectorCode(code) => Attribute::Selection(code),
        unexpected => return Err(UnexpectedToken(unexpected).at(token.pos).into()),
    };
    Ok(res)
}

fn parse_feature(
    config: &mut Config,
    mark: bool,
    feature: String,
    pos: &FilePosition,
) -> Result<Attribute> {
    match config.features.encode(&feature) {
        Some(label) => Ok(Attribute::Feature(mark, label)),
        None => Err(UndefinedFeature(feature).at(pos.clone())),
    }
}

fn parse_parameter(
    config: &mut Config,
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
        Some((_, None)) => Err(UndefinedParameterVariant(parameter, variant).at(pos.clone())),
        None => Err(UndefinedParameter(parameter).at(pos.clone())),
    }
}

fn parse_character<T: FromIterator<Attribute>>(
    config: &mut Config,
    identifier: String,
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
                .ok_or_else(|| UndefinedDiacritic(dia).at(pos.clone()))
        })
        .partition_result();
    check_errors((), errors)?;
    attrs.extend(diacritics);

    Ok(FromIterator::from_iter(attrs))
}

fn parse_base_character(
    config: &mut Config,
    character: String,
    pos: &FilePosition,
) -> Result<Attribute> {
    match config.characters.get(&character) {
        Some(phoneme) => Ok(Attribute::Character(phoneme.to_owned())),
        None => Err(UndefinedCharacter(character).at(pos.clone())),
    }
}

// Confirms that the next token is of a specific type, returning a specified
// error if it is not.
// This function will consume the next token, regardless of what it is.
// If FileLexer gives an IO error, this function will consume and return it
fn confirm_token_type(
    file: &mut impl Iterator<Item = Token>,
    desired: RawToken,
    error: errors::ParseErrorType,
    pos: &FilePosition,
) -> Result<()> {
    let Some(token) = file.next() else {
        return Err(error.at(pos.clone()));
    };

    if token.token == desired {
        Ok(())
    } else {
        Err(error.at(token.pos))
    }
}

// Used for ensuring that lines are finished when they are supposed to be.
// Goes until it consumes an EOL, and then marks every found token an error.
fn end_line(file: &mut impl Iterator<Item = Token>) -> ResultV<()> {
    let errors: Vec<_> = file
        .take_while(|token| token.token != RawToken::EOL)
        .map(|token| ExpectedEOL.at(token.pos))
        .collect();

    check_errors((), errors)
}

fn end_block(token: &Token) -> bool {
    token.token != RawToken::BlockClose
}
