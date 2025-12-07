use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(version, about)]
#[command(propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,

    #[command(flatten)]
    pub config: ConfigArgs,

    /// Always write out all attributes of phonemes instead of reducing them to
    /// a base character
    #[arg(long, global = true)]
    pub no_base: bool,
}

#[derive(Subcommand, Debug)]
pub enum Command {
    /// Predict the evolution of a word from one language to another
    #[command(visible_alias = "ev")]
    Evolve(EvolveArgs),

    /// Predict the evolution of a word to all descendents of a language
    #[command(visible_alias = "tr")]
    Tree(TreeArgs),
}

#[derive(Args, Debug)]
pub struct ConfigArgs {
    /// Specify entry configuration file or the directory to search in
    #[arg(short, long, value_name = "path", global = true)]
    pub config: Option<PathBuf>,
}

#[derive(Args, Debug)]
pub struct EvolveArgs {
    /// The word to evolve, with phonemes seperated by whitespace
    pub word: String,

    /// Language to start from. If not specified, the first defined language
    /// will be used.
    #[arg(short, long, value_name = "language")]
    pub start: Option<String>,

    /// Language to evolve to. If not specified, the last defined language will
    /// be used
    #[arg(short, long, value_name = "language")]
    pub end: Option<String>,

    #[command(flatten)]
    pub output: EvolutionOutputArgs,
}

#[derive(Args, Debug)]
pub struct EvolutionOutputArgs {
    /// Only output the final result
    #[arg(short, long)]
    pub no_stages: bool,

    /// Show the rule being applied, preceeded with `//`
    #[arg(long, conflicts_with = "no_stages")]
    pub show_rules: bool,

    /// Output the result after applying rules that change the word instead of
    /// just after each language stage
    #[arg(long, conflicts_with = "no_stages")]
    pub show_changes: bool,

    /// Output the result after applying every rule, including ones that don't
    /// change the word
    #[arg(long, requires = "show_changes")]
    pub show_all_rules: bool,

    /// Don't label the different stages
    #[arg(long, conflicts_with = "no_stages")]
    pub no_labels: bool,

    /// Output result stages separated by commas instead of newlines
    #[arg(long)]
    pub csv: bool,
}

#[derive(Args, Debug)]
pub struct TreeArgs {
    /// The word to evolve, with phonemes seperated by whitespace
    pub word: String,

    /// Language to start from. If not specified, the first defined language
    /// will be used.
    #[arg(short, long, value_name = "language")]
    pub start: Option<String>,

    /// Only evolve to a specific depth
    #[arg(short, long, value_name = "levels")]
    pub depth: Option<u32>,
}
