use clap::Parser;

mod arg_parser;

pub fn do_cli() {
    let args = arg_parser::Cli::parse();

    println!("{args:#?}");
}
