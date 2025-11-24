use std::path::PathBuf;

use clap::Parser;

use arg_parser::{Cli, Command};
use itertools::Itertools;

use crate::cli::arg_parser::{ConfigArgs, EvolutionOutputArgs, EvolveArgs};
use crate::config::{Config, Label};
use crate::error_handling::{ErrorType, FilePosition, Result, ResultV, wrap_io_error};
use crate::evolution::do_rule;
use crate::evolution::routing::find_route;
use crate::parser::{parse_config_file, parse_word};
use crate::phonemes::Phoneme;

mod arg_parser;
mod output;

const EXPECTED_PRIMARY_CONFIG_NAME: &str = "primary.paw";

pub static mut NO_BASE: bool = false;

#[derive(Debug)]
pub enum CliErrorType {
    NoConfigFile(PathBuf),
}

impl ErrorType for CliErrorType {
    fn module(&self) -> String {
        String::from("cli")
    }
}

impl std::fmt::Display for CliErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoConfigFile(path) => {
                write!(f, "Config path `{}` does not exist", path.display())
            }
        }
    }
}

use CliErrorType::*;

pub fn do_cli() -> ResultV<()> {
    let args = Cli::parse();

    unsafe {
        // Doing this allows the many different formatting methods and stuff to
        // know whether or not to reduce to a base without needing to pass this
        // around and muddle everything up
        NO_BASE = args.no_base;
    }

    match args.command {
        Command::Evolve(e_args) => evolve(e_args, args.config),
    }
}

fn find_config_file(args: ConfigArgs) -> Result<PathBuf> {
    let base_path = args
        .config
        .unwrap_or_else(|| EXPECTED_PRIMARY_CONFIG_NAME.into());

    let expected_path = gen_expected_config_path(base_path);

    verify_config_existence(expected_path)
}

fn gen_expected_config_path(path: PathBuf) -> PathBuf {
    if path.exists() && !path.is_file() {
        path.join(EXPECTED_PRIMARY_CONFIG_NAME)
    } else {
        path
    }
}

fn verify_config_existence(path: PathBuf) -> Result<PathBuf> {
    match path.try_exists() {
        Ok(true) => Ok(path),
        Ok(false) => Err(NoConfigFile(path).sign()),
        Err(err) => Err(wrap_io_error(
            "cli",
            Some(&FilePosition::new(Some(&path.into()), None, None)),
        )(err)),
    }
}

// This is for filling in optional parameters with automatic values that might
// not exist. If it doesn't, it uses the default of the type. Which ever it goes
// with, this will clone it before returning it.
fn fill_in_param<T: Default + Clone>(param: &Option<T>, fill_in: &Option<T>) -> T {
    param
        .as_ref()
        .or_else(|| fill_in.as_ref())
        .cloned()
        .unwrap_or_default()
}

fn evolve(args: EvolveArgs, config_args: ConfigArgs) -> ResultV<()> {
    let config_path = find_config_file(config_args)?;
    let mut config = parse_config_file(config_path)?;
    let mut word = parse_word(&args.word, &mut config)?;

    let start_name = fill_in_param(&args.start, &config.first_language);
    let end_name = fill_in_param(&args.end, &config.last_language);

    let route = find_route(
        start_name.clone(),
        end_name.clone(),
        &config.languages,
        &config.evolutions,
    )?;
    let route_pairs = route.iter().tuple_windows();

    output::display_start(&word, &start_name, &args.output);

    for (start, end) in route_pairs {
        word = do_evolution_step(word, start, end, &args.output, &config);
    }

    output::display_final_result(&word, &args.output);

    Ok(())
}

fn do_evolution_step(
    mut word: Vec<Phoneme>,
    start: &Label,
    end: &Label,
    args: &EvolutionOutputArgs,
    config: &Config,
) -> Vec<Phoneme> {
    output::display_evolution_label(start, end, args);

    let rules = config.evolutions.get(start).unwrap().get(end).unwrap();

    for rule in rules {
        if let Some(new_word) = do_rule(&word, &rule, &config.characters) {
            word = new_word;
            output::display_application(&word, true, args);
        } else {
            output::display_application(&word, false, args);
        }
    }

    output::display_lang_result(&word, args);

    word
}
