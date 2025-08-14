use crate::parser::lexer::FileLexer;

mod config;
mod error_handling;
mod parser;

fn main() -> parser::PResult<()> {
    let lexer = FileLexer::lex_file("syntax_example.paw".into())?;

    // lexer.for_each(|t| println!("{:?}", t.unwrap()));

    let config = parser::parser::parse_config(&mut lexer.peekable())?;

    println!("{:#?}", config);

    return Ok(());
}
