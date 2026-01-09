#[cfg(test)]
mod tests;

use std::path::PathBuf;

use itertools::Itertools;

use crate::config::Config;
use crate::error_handling::{Error, check_errors};
use crate::phonemes::Phoneme;

// At some point in time we need to touch every part of the parse tree, so a
// wildcard is appropriate here.
#[allow(clippy::wildcard_imports)]
use crate::parser::*;

type Errors<'a> = &'a mut Vec<Error>;

pub fn parse_word(word: &str, config: &Config) -> Result<Vec<Phoneme>, Vec<Error>> {
    todo!()
}

pub fn parse_config_file(path: PathBuf) -> Result<Config, Vec<Error>> {
    let mut parser = ConfigParser::new(path)?;
    let mut config = Config::new();
    let mut errors = Vec::new();

    for block_res in parser.by_ref() {
        match block_res {
            Ok(block) => compile_definition_block(block, &mut config, &mut errors),
            Err(error) => errors.push(error.into()),
        }
    }

    errors.extend(parser.io_error.into_iter().map_into());

    check_errors(config, errors)
}

pub fn compile_definition_block(block: DefinitionBlock, config: &mut Config, errors: Errors) {
    match block {
        DefinitionBlock::Languages(block) => define_block(block, define_language, config, errors),
        DefinitionBlock::Parameters(block) => todo!(),
        DefinitionBlock::Features(block) => define_block(block, define_feature, config, errors),
        DefinitionBlock::Characters(block) => todo!(),
        DefinitionBlock::Diacritics(block) => todo!(),
        DefinitionBlock::Evolve(evolution) => todo!(),
    }
}

pub fn define_block<T: Parse<Vec<ParseError>>>(
    block: Block<T, Vec<ParseError>>,
    definer: impl Fn(T, &mut Config, Errors),
    config: &mut Config,
    errors: Errors,
) {
    if let Err(error) = block.close_error {
        errors.push(error.into());
    }

    for definition_res in block.definitions {
        match definition_res {
            Ok(definition) => definer(definition, config, errors),
            Err(error_vec) => errors.extend(error_vec.into_iter().map_into()),
        }
    }
}

pub fn define_feature(feature: IdentifierLine, config: &mut Config, errors: Errors) {
    errors.extend(feature.excess_tokens.into_iter().map_into());

    config.features.add(feature.identifier.text);
}

pub fn define_language(language: IdentifierLine, config: &mut Config, errors: Errors) {
    errors.extend(language.excess_tokens.into_iter().map_into());

    config
        .first_language
        .get_or_insert_with(|| language.identifier.text.clone());
    let _ = config
        .last_language
        .insert(language.identifier.text.clone());

    config.languages.add(language.identifier.text);
}
