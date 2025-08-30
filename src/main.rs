use crate::parser::lexer::FileLexer;

mod config;
mod error_handling;
mod evolution;
mod parser;
mod phonemes;

fn main() -> parser::PResult<()> {
    let mut lexer = FileLexer::lex_file("syntax_example.paw".into())?;

    // lexer.for_each(|t| println!("{:?}", t.unwrap()));

    let config_attempt = parser::parser::parse_config_file(&mut lexer);

    match config_attempt {
        Ok(config) => println!("{:#?}", config),
        Err(errs) => errs.iter().for_each(|e| println!("{}", e)),
    }

    return Ok(());
}
