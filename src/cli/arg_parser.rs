use std::path::PathBuf;

use clap::{Args, Parser, Subcommand, ValueEnum};

#[derive(Parser, Debug)]
#[command(version, about)]
#[command(propagate_version = true)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,

    /// Entry configuration file to use
    #[arg(short, long, value_name = "path")]
    pub config: PathBuf,

    /// Always write out all attributes of phonemes instead of reducing them to
    /// a base character
    #[arg(long, global = true)]
    pub no_base: bool,
}

#[derive(Subcommand, Debug)]
pub enum Command {
    /// Predict the evolution of a lexeme from one language to another
    // #[command(visible_alias = "e")]
    #[command(alias = "ev")]
    Evolve {
        /// The lexeme to evolve, with phonemes seperated by whitespace
        lexeme: String,

        /// Language to start from
        start: String,

        /// Language to evolve to
        end: String,

        /// Use the specified routing method to connect the start and end
        /// languages
        #[arg(long, value_name = "method", value_enum, default_value_t = RoutingMethods::LeastSteps)]
        route: RoutingMethods,

        /// Output evolution stages on one line, separated by commas
        #[arg(long)]
        csv: bool,

        #[command(flatten)]
        stage_verbosity: StageVerbosity,
    },
}

#[derive(Args, Debug, Clone)]
#[group(required = false, multiple = false)]
pub struct StageVerbosity {
    /// Just output the final result
    #[arg(short, long)]
    no_stages: bool,

    /// Output the result after applying rules that change the word
    #[arg(long)]
    show_rules: bool,

    /// Output the result after applying every rule, including ones that don't
    /// change the word
    #[arg(long)]
    all_rules: bool,
}

#[derive(ValueEnum, Debug, Clone)]
pub enum RoutingMethods {
    /// maximizes language steps
    MostSteps,
    /// minimizes language steps
    LeastSteps,
    /// maximizes included rules
    MostRules,
    /// minimizes included rules
    LeastRules,
    /// only uses explicitly configured evolutions
    Direct,
}
