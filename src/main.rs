#![warn(clippy::pedantic)]
#![allow(clippy::enum_glob_use)]

use std::process::ExitCode;

mod cli;
mod compiler;
mod config;
mod error_handling;
mod evolution;
mod lexer;
mod parser;
mod phonemes;

fn main() -> ExitCode {
    if let Err(errs) = cli::do_cli() {
        for err in errs {
            println!("{err}");
        }
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
