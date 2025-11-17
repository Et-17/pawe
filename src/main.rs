use itertools::Itertools;

use crate::error_handling::Error;
use crate::evolution::do_rule;
use crate::parser::lexer::FileLexer;

mod cli;
mod config;
mod error_handling;
mod evolution;
mod parser;
mod phonemes;

fn main() -> () {
    // if let Err(errs) = test() {
    //     for err in errs {
    //         println!("{}", err);
    //     }
    // }
    cli::do_cli();
}

fn test() -> Result<(), Vec<Error>> {
    let mut lexer = FileLexer::lex_file("syntax_example.paw".into())?;
    let config = parser::parse_config_file(&mut lexer)?;

    // let mut word: Vec<_> = vec!["t", "r", "é", "y", "e", "s"]
    let mut word: Vec<_> = vec!["l", "ó", "w", "k", "s", "n", "e", "h₂"]
        .into_iter()
        .map(|c| config.characters.get(c).unwrap().clone())
        .map(|c| c.into())
        .collect();

    println!("Configured word:");
    println!("    {}", word.iter().join(""));
    println!();

    let route = evolution::routing::find_route(
        "ProtoIndoEuropean".to_owned(),
        "LateProtoGermanic".to_owned(),
        &config.languages,
        &config.evolutions,
    )?;
    let route_pairs: Vec<(_, _)> = route.iter().tuple_windows().collect();

    println!("Found route:");
    for lang in &route {
        println!("    {lang}");
    }
    println!();

    for (start, end) in route_pairs {
        println!("Evolving from {start} to {end}");
        let rules = config.evolutions.get(start).unwrap().get(end).unwrap();

        for rule in rules {
            word = do_rule(word, &rule, &config.characters);
            println!("    {}", word.iter().join(""));
        }
    }

    Ok(())
}
