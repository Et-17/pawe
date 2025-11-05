use crate::{evolution::do_rule, parser::lexer::FileLexer};

mod config;
mod error_handling;
mod evolution;
mod parser;
mod phonemes;

fn main() -> parser::PResultV<()> {
    let mut lexer = FileLexer::lex_file("syntax_example.paw".into())?;
    let config = parser::parser::parse_config_file(&mut lexer)?;

    let word = ["l", "o", "w", "k", "s", "n", "e", "h₂"]
        .map(|c| config.characters.get(c).unwrap().clone());

    println!("Configured word:");
    for c in &word {
        println!("    {:?}", c);
    }
    println!();

    for (start_lang, evos) in config.evolutions {
        for (end_lang, rules) in evos {
            println!(
                "Applying rules for evolving from {} to {}",
                start_lang, end_lang
            );
            for rule in rules {
                let _ = do_rule(&word[..], &rule);
            }
        }
    }

    return Ok(());
}
