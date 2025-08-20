use crate::parser::lexer::FileLexer;

mod config;
mod error_handling;
mod parser;
mod phonemes;

fn main() -> parser::PResult<()> {
    let lexer = FileLexer::lex_file("syntax_example.paw".into())?;

    // lexer.for_each(|t| println!("{:?}", t.unwrap()));

    let config_attempt = parser::parser::parse_config(&mut lexer.peekable());

    match config_attempt {
        Ok(config) => println!("{:#?}", config),
        Err(errs) => errs.iter().for_each(|e| println!("{}", e)),
    }

    return Ok(());
}
