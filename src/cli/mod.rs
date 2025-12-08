use std::collections::HashMap;
use std::path::PathBuf;

use clap::Parser;

use arg_parser::{Cli, Command};
use itertools::Itertools;

use crate::cli::arg_parser::{ConfigArgs, EvolutionOutputArgs, EvolveArgs, TreeArgs};
use crate::config::{Config, Label};
use crate::error_handling::{ErrorType, FilePosition, Result, ResultV, wrap_io_error};
use crate::evolution::routing::find_route;
use crate::evolution::{Rule, do_rule};
use crate::parser::{parse_config_file, parse_word};
use crate::phonemes::Phoneme;

mod arg_parser;
mod output;

const EXPECTED_PRIMARY_CONFIG_NAME: &str = "primary.paw";

pub static mut NO_BASE: bool = false;
pub static mut RAW_INPUT: bool = false;

#[derive(Debug)]
pub enum CliErrorType {
    NoConfigFile(PathBuf),
    UndefinedLanguage(String),
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
            Self::UndefinedLanguage(lang) => {
                write!(f, "Could not find language `{lang}`")
            }
        }
    }
}

use CliErrorType::*;

pub fn do_cli() -> ResultV<()> {
    let args = Cli::parse();

    unsafe {
        // This lets us easily send these parameters around the program without
        // having to jam them around the program and use things like custom
        // display traits to take parameters
        NO_BASE = args.no_base;
        RAW_INPUT = args.raw_input;
    }

    match args.command {
        Command::Evolve(e_args) => evolve(e_args, args.config),
        Command::Tree(t_args) => tree(t_args, args.config),
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
        .or(fill_in.as_ref())
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
        if let Some(new_word) = do_rule(&word, rule, &config.characters, &config.diacritics) {
            word = new_word;
            output::display_application(&word, true, rule, args);
        } else {
            output::display_application(&word, false, rule, args);
        }
    }

    output::display_lang_result(&word, args);

    word
}

fn tree(args: TreeArgs, config_args: ConfigArgs) -> ResultV<()> {
    let config_path = find_config_file(config_args)?;
    let mut config = parse_config_file(config_path)?;
    let word = parse_word(&args.word, &mut config)?;

    let start_name = fill_in_param(&args.start, &config.first_language);
    let Some(start) = config.languages.encode(&start_name) else {
        return Err(UndefinedLanguage(start_name).sign().into());
    };

    let result_tree = gen_tree(word, start, 0, &args, &config);

    output::display_tree(&result_tree);

    Ok(())
}

fn no_display_evolution_step(word: Vec<Phoneme>, rules: &[Rule], config: &Config) -> Vec<Phoneme> {
    rules.iter().fold(word, |w, rule| {
        do_rule(&w, rule, &config.characters, &config.diacritics).unwrap_or(w)
    })
}

pub struct TreeNode {
    language: Label,
    word: Vec<Phoneme>,
    children: Vec<TreeNode>,
}

fn gen_tree(
    word: Vec<Phoneme>,
    language: Label,
    depth: u32,
    args: &TreeArgs,
    config: &Config,
) -> TreeNode {
    if args.depth.is_some_and(|max| depth >= max) {
        return TreeNode {
            language,
            word,
            children: Vec::new(),
        };
    }

    let child_generator = gen_tree_child(&word, depth, args, config);
    let children = config
        .evolutions
        .get(&language)
        .map(HashMap::iter)
        .unwrap_or_default()
        .map(child_generator)
        .collect();

    TreeNode {
        language,
        word,
        children,
    }
}

fn gen_tree_child(
    word: &[Phoneme],
    depth: u32,
    args: &TreeArgs,
    config: &Config,
) -> impl FnMut((&Label, &Vec<Rule>)) -> TreeNode {
    move |(child, child_rules)| {
        let new_word = no_display_evolution_step(word.to_owned(), child_rules, config);
        gen_tree(new_word, child.clone(), depth + 1, args, config)
    }
}
