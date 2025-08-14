use crate::{config::Config, error_handling::Position, parser::lexer::FileLexer};

mod config;
mod error_handling;
mod parser;

fn main() -> parser::PResult<()> {
    let mut lexer = FileLexer::lex_file("syntax_example.paw".into())?;

    lexer.next();

    // lexer.for_each(|t| println!("{:?}", t.unwrap()));

    let mut config = Config::new();
    parser::parser::parse_languages(
        &mut lexer.peekable(),
        &mut config,
        Position { line: 0, char: 0 },
    )?;

    println!("{:#?}", config);

    return Ok(());
}
