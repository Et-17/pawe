use std::{iter::Peekable, str::Chars};

use itertools::{Itertools, PeekingNext};

#[derive(Debug, PartialEq)]
pub enum Token {
    // Keywords
    Languages, // languages
    Parameters, // parameters
    Features, // features
    Characters, // characters
    Evolve, // evolve
    To, // to

    // Special characters
    Output, // >
    Environment, // /
    InputLocation, // _
    WordBoundry, // #
    EOL, // ;

    // Section markers
    BlockOpen, // {
    BlockClose, // }
    MatchingPhonemeOpen, // (
    TaggedMatchingPhonemeOpen(String), // █(
    MatchingPhonemeClose, // )
    ConcretePhonemeOpen, // [
    ConcretePhonemeClose, // ]

    // General tokens
    PositiveIdentifier(String), // +███
    NegativeIdentifier(String), // -███
    UnmarkedIdentifier(String), //  ███

    UnknownCharacter(char),
}

pub struct LexemeStream<'a> {
    char_stream: Peekable<Chars<'a>>
}

impl Iterator for LexemeStream<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        lex_token(&mut self.char_stream)
    }
}

pub fn lex_raw_identifier(line: &mut Peekable<Chars>) -> Token {
    let identifier = line.peeking_take_while(|c| c.is_alphanumeric()).collect();

    // Check if this is the opening of a tagged matching phoneme
    if line.peeking_next(|c| *c == '(').is_some() {
        return Token::TaggedMatchingPhonemeOpen(identifier)
    }

    return match identifier.as_str() {
        "languages" => Token::Languages,
        "parameters" => Token::Parameters,
        "features" => Token::Features,
        "characters" => Token::Characters,
        "evolve" => Token::Evolve,
        "to" => Token::To,
        _ => Token::UnmarkedIdentifier(identifier)
    }
}

pub fn lex_pos_identifier(line: &mut Peekable<Chars>) -> Token {
    let identifier = line.peeking_take_while(|c| c.is_alphanumeric()).collect();
    return Token::PositiveIdentifier(identifier);
}

pub fn lex_neg_identifier(line: &mut Peekable<Chars>) -> Token {
    let identifier = line.peeking_take_while(|c| c.is_alphanumeric()).collect();
    return Token::NegativeIdentifier(identifier);
}

pub fn lex_token(line: &mut Peekable<Chars>) -> Option<Token> {
    while let Some(c) = line.peek() {
        if c.is_whitespace() {
            line.next();
            continue;
        } else if c.is_alphanumeric() {
            return Some(lex_raw_identifier(line));
        } else if *c == '+' {
            line.next();
            return Some(lex_pos_identifier(line));
        } else if *c == '-' {
            line.next();
            return Some(lex_neg_identifier(line));
        }

        let to_return = Some(match *c {
            '>' => Token::Output,
            '/' => Token::Environment,
            '_' => Token::InputLocation,
            '#' => Token::WordBoundry,
            ';' => Token::EOL,

            '{' => Token::BlockOpen,
            '}' => Token::BlockClose,
            '(' => Token::MatchingPhonemeOpen,
            ')' => Token::MatchingPhonemeClose,
            '[' => Token::ConcretePhonemeOpen,
            ']' => Token::ConcretePhonemeClose,

            unknown => Token::UnknownCharacter(unknown)
        });

        line.next();
        return to_return;
    }

    return None;
}

pub fn lex_stream (line: Peekable<Chars>) -> LexemeStream {
    LexemeStream { char_stream: line }
}
