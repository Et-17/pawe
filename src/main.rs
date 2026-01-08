#![warn(clippy::pedantic)]
#![allow(clippy::enum_glob_use)]

use std::process::ExitCode;

use crate::parser::ConfigParser;

mod cli;
mod compiler;
mod config;
mod error_handling;
mod evolution;
mod lexer;
mod parser;
mod phonemes;

fn main() -> ExitCode {
    // if let Err(errs) = cli::do_cli() {
    //     for err in errs {
    //         println!("{err}");
    //     }
    //     ExitCode::FAILURE
    // } else {
    //     ExitCode::SUCCESS
    // }

    let parser = ConfigParser::new("primary.new.paw".into()).unwrap();

    for block in parser {
        println!("{:#?}", block.unwrap());
    }

    ExitCode::SUCCESS
}
