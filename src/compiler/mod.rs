mod errors;
#[cfg(test)]
mod tests;

use std::path::PathBuf;

use itertools::Itertools;

use crate::compiler::errors::{CompileError, Defineable, Logger};
use crate::config::Config;
use crate::error_handling::{Error, ErrorType, check_errors};
use crate::phonemes::Phoneme;

use errors::CompileErrorType::*;

// At some point in time we need to touch every part of the parse tree, so a
// wildcard is appropriate here.
#[allow(clippy::wildcard_imports)]
use crate::parser::*;

pub fn parse_word(word: &str, config: &Config) -> Result<Vec<Phoneme>, Vec<Error>> {
    todo!()
}

pub fn parse_config_file(path: PathBuf) -> Result<Config, Vec<Error>> {
    let mut parser = ConfigParser::new(path)?;
    let mut config = Config::new();
    let mut logger = Logger::new();

    for block_res in parser.by_ref() {
        match block_res {
            Ok(block) => compile_definition_block(block, &mut config, &mut logger),
            Err(error) => logger.emit(error),
        }
    }

    let mut errors: Vec<Error> = logger.errors.into_iter().map_into().collect_vec();
    errors.extend(parser.io_errors.into_iter().map_into());

    check_errors(config, errors)
}

pub fn compile_definition_block(block: DefinitionBlock, config: &mut Config, logger: &mut Logger) {
    match block {
        DefinitionBlock::Languages(block) => define_block(block, define_language, config, logger),
        DefinitionBlock::Parameters(block) => define_block(block, define_parameter, config, logger),
        DefinitionBlock::Features(block) => define_block(block, define_feature, config, logger),
        DefinitionBlock::Characters(block) => todo!(),
        DefinitionBlock::Diacritics(block) => todo!(),
        DefinitionBlock::Evolve(evolution) => todo!(),
    }
}

fn define_block<T: Parse<Vec<ParseError>>>(
    block: Block<T, Vec<ParseError>>,
    definer: impl Fn(T, &mut Config, &mut Logger),
    config: &mut Config,
    logger: &mut Logger,
) {
    logger.emit_r(block.close_error);

    for definition_res in block.definitions {
        match definition_res {
            Ok(definition) => definer(definition, config, logger),
            Err(error_vec) => logger.emit_i(error_vec),
        }
    }
}

fn define_feature(feature: IdentifierLine, config: &mut Config, logger: &mut Logger) {
    logger.emit_i(feature.excess_tokens);

    config.features.add(feature.identifier.text);
}

fn define_language(language: IdentifierLine, config: &mut Config, logger: &mut Logger) {
    logger.emit_i(language.excess_tokens);

    config
        .first_language
        .get_or_insert_with(|| language.identifier.text.clone());
    let _ = config
        .last_language
        .insert(language.identifier.text.clone());

    config.languages.add(language.identifier.text);
}

fn define_parameter(parameter_def: ParameterDefinition, config: &mut Config, logger: &mut Logger) {
    let parameter = parameter_def.parameter.text;

    let variants = logger
        .extract_ok(parameter_def.variants)
        .map(|ident| ident.text)
        .collect_vec();

    if config.parameters.add(parameter.clone(), variants).is_err() {
        logger.emit(
            Redefinition(Defineable::Parameter(parameter))
                .at::<CompileError>(parameter_def.parameter.pos),
        );
    }
}
