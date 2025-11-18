use std::path::PathBuf;

use clap::Parser;

use arg_parser::{Cli, Command};
use itertools::Itertools;

use crate::cli::arg_parser::EvolveArgs;
use crate::config::{Config, Label};
use crate::error_handling::ResultV;
use crate::evolution::do_rule;
use crate::evolution::routing::find_route;
use crate::parser::{parse_config_file, parse_word};
use crate::phonemes::Phoneme;

mod arg_parser;

pub static mut NO_BASE: bool = false;

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

fn evolve(args: EvolveArgs, config_path: PathBuf) -> ResultV<()> {
    let mut config = parse_config_file(config_path)?;
    let mut word = parse_word(&args.word, &mut config)?;

    let route = find_route(
        args.start.clone(),
        args.end.clone(),
        &config.languages,
        &config.evolutions,
    )?;
    let route_pairs = route.iter().tuple_windows();

    if !args.no_stages {
        println!("{}", args.start);
        println!("    {}", word.iter().join(""));
    }

    for (start, end) in route_pairs {
        word = do_evolution_step(word, start, end, &args, &config);
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
    args: &EvolveArgs,
    config: &Config,
) -> Vec<Phoneme> {
    if args.show_rules {
        println!("Evolving from {start} to {end}");
    } else if !args.no_stages {
        println!("{end}");
    }

    let rules = config.evolutions.get(start).unwrap().get(end).unwrap();

    for rule in rules {
        word = do_rule(word, &rule, &config.characters);
        if args.show_rules {
            println!("    {}", word.iter().join(""));
        }
    }

    if !args.show_rules && !args.no_stages {
        println!("    {}", word.iter().join(""));
    }

    word
}
