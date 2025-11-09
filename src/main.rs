use crate::error_handling::{Error, ErrorType};
use crate::evolution::do_rule;
use crate::parser::lexer::FileLexer;

mod config;
mod error_handling;
mod evolution;
mod parser;
mod phonemes;

fn main() -> () {
    if let Err(errs) = test() {
        for err in errs {
            println!("{}", err);
        }
    }
}

fn test() -> Result<(), Vec<Error<impl ErrorType>>> {
    let mut lexer = FileLexer::lex_file("syntax_example.paw".into())?;
    let config = parser::parser::parse_config_file(&mut lexer)?;

    println!("Config: {config:#?}");

    let mut word: Vec<_> = vec!["l", "ó", "w", "k", "s", "n", "e", "h₂"]
        .into_iter()
        .map(|c| config.characters.get(c).unwrap().clone())
        .map(|c| c.into())
        .collect();

    println!("Configured word:");
    for c in &word {
        println!("    {:?}", c);
    }
    println!();

    for (start_lang, evos) in config.evolutions {
        for (end_lang, rules) in evos {
            println!("Evolving from {start_lang} to {end_lang}");

            for rule in rules {
                word = do_rule(word, &rule, &config.characters);
                word.iter().for_each(|c| print!("{:?}", c));
                println!();
            }
        }
    }

    Ok(())
}
