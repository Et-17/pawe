use std::path::PathBuf;

use crate::config::Config;
use crate::error_handling::Error;
use crate::phonemes::Phoneme;

pub fn parse_word(word: &str, config: &Config) -> Result<Vec<Phoneme>, Vec<Error>> {
    unimplemented!()
}

pub fn parse_config_file(path: PathBuf) -> Result<Config, Vec<Error>> {
    unimplemented!()
}
