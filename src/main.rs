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

    // let mut word: Vec<_> = vec!["l", "ó", "w", "k", "s", "n", "e", "h₂"]
    let mut word: Vec<_> = vec!["t", "r", "é", "y", "e", "s"]
        .into_iter()
        .map(|c| config.characters.get(c).unwrap().clone())
        .map(|c| c.into())
        .collect();

    println!("Configured word:");
    for c in &word {
        println!("    {:?}", c);
    }
    println!();

    let testing_steps = vec![
        ("ProtoIndoEuropean", "PreProtoGermanic"),
        ("PreProtoGermanic", "EarlyProtoGermanic"),
        ("EarlyProtoGermanic", "LateProtoGermanic"),
    ];

    for (lang_a_name, lang_b_name) in testing_steps {
        println!("Evolving from {lang_a_name} to {lang_b_name}");
        let lang_a = &config.languages.encode(&lang_a_name.into()).unwrap();
        let lang_b = &config.languages.encode(&lang_b_name.into()).unwrap();
        let rules = config.evolutions.get(lang_a).unwrap().get(lang_b).unwrap();

        for rule in rules {
            word = do_rule(word, &rule, &config.characters);
            word.iter().for_each(|c| print!("{:?}", c));
            println!();
        }
    }

    Ok(())
}
