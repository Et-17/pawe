use crate::parser::lexer::lex_stream;

mod error_handling;
mod parser;

fn main() {
    let text = "e > a / h₂ _".chars().peekable();
    lex_stream(text).for_each(|t| println!("{:?}", t));
}
