mod cli;
mod config;
mod error_handling;
mod evolution;
mod parser;
mod phonemes;

fn main() -> () {
    if let Err(errs) = cli::do_cli() {
        let len = errs.len();
        for err in errs {
            println!("{}", err);
        }
        println!("Encountered {len} errors");
    }
}
