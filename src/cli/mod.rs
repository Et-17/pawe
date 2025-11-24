use std::path::PathBuf;

use clap::Parser;

use arg_parser::{Cli, Command};
use itertools::Itertools;

use crate::cli::arg_parser::{ConfigArgs, EvolveArgs};
use crate::config::{Config, Label};
use crate::error_handling::{ErrorType, FilePosition, Result, ResultV, wrap_io_error};
use crate::evolution::do_rule;
use crate::evolution::routing::find_route;
use crate::parser::{parse_config_file, parse_word};
use crate::phonemes::Phoneme;

mod arg_parser;

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

fn evolve(args: EvolveArgs, config_args: ConfigArgs) -> ResultV<()> {
    let config_path = find_config_file(config_args)?;
    let mut config = parse_config_file(config_path)?;
    let mut word = parse_word(&args.word, &mut config)?;

    let indent = match args.no_labels {
        true => "",
        false => "    ",
    };

    let start_name = args
        .start
        .as_ref()
        .or(config.first_language.as_ref())
        .cloned()
        .unwrap_or_default();
    let end_name = args
        .end
        .as_ref()
        .or(config.last_language.as_ref())
        .cloned()
        .unwrap_or_default();

    let route = find_route(
        start_name.clone(),
        end_name.clone(),
        &config.languages,
        &config.evolutions,
    )?;
    let route_pairs = route.iter().tuple_windows();

    if !args.no_stages {
        if !args.no_labels {
            println!("{}", &start_name);
        }
        println!("{indent}{}", word.iter().join(""));
    }

    for (start, end) in route_pairs {
        word = do_evolution_step(word, start, end, indent, &args, &config);
    }

    if args.no_stages {
        println!("{}", word.iter().join(""));
    }

    Ok(())
}

fn do_evolution_step(
    mut word: Vec<Phoneme>,
    start: &Label,
    end: &Label,
    ident: &str,
    args: &EvolveArgs,
    config: &Config,
) -> Vec<Phoneme> {
    if args.show_rules && !args.no_labels {
        println!("Evolving from {start} to {end}");
    } else if !args.no_stages && !args.no_labels {
        println!("{end}");
    }

    let rules = config.evolutions.get(start).unwrap().get(end).unwrap();

    for rule in rules {
        if let Some(new_word) = do_rule(&word, &rule, &config.characters) {
            word = new_word;
            if args.show_rules {
                println!("{ident}{}", word.iter().join(""));
            }
        } else {
            if args.all_rules {
                println!("{ident}{}", word.iter().join(""));
            }
        }
    }

    if !args.show_rules && !args.no_stages {
        println!("{ident}{}", word.iter().join(""));
    }

    word
}
