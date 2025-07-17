use crate::parser::lexer::FileLexer;

mod error_handling;
mod parser;

fn main() -> std::io::Result<()> {
    let lexer = FileLexer::lex_file("syntax_example.paw".into())?;

    lexer.for_each(|t| println!("{:?}", t.unwrap()));

    return Ok(());
}
