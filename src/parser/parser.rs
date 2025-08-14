use std::iter::Peekable;

use crate::config::Config;
use crate::error_handling::{Error, Position};

use super::lexer::{FileLexer, RawToken};
use super::{PResult, ParseErrorType::*};

pub fn parse_config(file: &mut Peekable<FileLexer>) -> PResult<Config> {
    let mut config = Config::new();

    while let Some(token_res) = file.next() {
        let token = token_res?;
        let token_type = token.token;
        let pos = token.pos;

        match token_type {
            RawToken::Languages => parse_languages(file, &mut config, pos)?,
            _ => {
                return Err(Error {
                    pos,
                    error: ExpectedBlockIdentifier,
                });
            }
        }
    }

    return Ok(config);
}

pub fn parse_languages(
    file: &mut Peekable<FileLexer>,
    config: &mut Config,
    pos: Position,
) -> PResult<()> {
    confirm_token_type(file, RawToken::BlockOpen, ExpectedBlock, &pos)?;

    let mut latest_pos = pos;

    while let Some(token_res) = file.next() {
        let token = token_res?;
        latest_pos = token.pos;

        match token.token {
            RawToken::BlockClose => return Ok(()),
            RawToken::UnmarkedIdentifier(ref lang) => add_language(config, lang.clone()),
            _ => {
                return Err(Error {
                    pos: latest_pos,
                    error: ExpectedLangName,
                });
            }
        }

        if check_token_type(file, RawToken::BlockClose)? {
            return Ok(());
        }

        confirm_token_type(file, RawToken::EOL, ExpectedEOL, &latest_pos)?;
    }

    Err(Error {
        pos: latest_pos,
        error: UnexpectedEOF,
    })
}

// This function exists to change the return type of add to ()
fn add_language(config: &mut Config, language: String) -> () {
    let _ = config.languages.add(language);
}

// Confirms that the next token is of a specific type, returning a specified
// error if it is not.
// If the correct token is found, this function will consume it and return ()
// If FileLexer gives an IO error, this function will consume and return it
pub fn confirm_token_type(
    file: &mut Peekable<FileLexer>,
    desired: RawToken,
    error: super::ParseErrorType,
    pos: &Position,
) -> PResult<()> {
    if !check_token_type(file, desired)? {
        Err(Error { pos: *pos, error })
    } else {
        Ok(())
    }
}

// Checks whether the next token is of a specific type.
// If the correct token is found, this function will consume it and return true.
// If FileLexer gives an IO error, this function will consume and return it.
pub fn check_token_type(file: &mut Peekable<FileLexer>, desired: RawToken) -> PResult<bool> {
    if let Some(token) = file.peek() {
        if token.is_err() {
            // we can just unwrap here because we already know that it is Some
            let fl_error = file.next().unwrap().unwrap_err();
            return Err(fl_error);
        }

        if token.as_ref().unwrap().token == desired {
            file.next();
            return Ok(true);
        }
    }

    Ok(false)
}
