use std::{
    collections::VecDeque,
    fmt::Debug,
    fs::File,
    io::{BufRead, BufReader},
    iter::Peekable,
    path::PathBuf,
};

use itertools::{Itertools, PeekingNext};

use crate::error_handling::{Error, Position};

pub struct FileLexer {
    file: BufReader<File>,
    current_line_tokens: VecDeque<Token>,
    line_num: usize,
}

impl FileLexer {
    fn lex_line(&mut self) -> Option<super::PResult<()>> {
        let mut next_line = String::new();
        let read_amount = self.file.read_line(&mut next_line);

        if let Ok(0) = read_amount {
            return None;
        } else if let Err(e) = read_amount {
            return Some(Err(Error {
                pos: Position {
                    line: self.line_num + 1,
                    char: 0,
                },
                error: super::ParseErrorType::FileError(e),
            }));
        }

        self.line_num += 1;

        let mut graphemes = next_line
            .chars()
            .enumerate()
            .map(MarkedChar::from_enumerated_grapheme(self.line_num))
            .peekable();

        while graphemes.peek().is_some() {
            let next_token_opt = lex_token(&mut graphemes);

            if let Some(t) = next_token_opt {
                if t.token == RawToken::Comment {
                    break; // Stop processing this line
                }

                self.current_line_tokens.push_back(t);
            } else {
                break;
            }
        }

        return Some(Ok(()));
    }

    pub fn lex_file(path: PathBuf) -> super::PResult<FileLexer> {
        let file = File::open(path).map_err(|e| Error {
            pos: Position { line: 0, char: 0 },
            error: super::ParseErrorType::FileError(e),
        })?;

        Ok(FileLexer {
            file: BufReader::new(file),
            current_line_tokens: VecDeque::new(),
            line_num: 0,
        })
    }
}

impl Iterator for FileLexer {
    type Item = super::PResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_line_tokens.len() > 0 {
            let next_token = self.current_line_tokens.pop_front()?;
            return Some(Ok(next_token));
        } else {
            let line_lexing_res = self.lex_line()?;

            return match line_lexing_res {
                Err(e) => Some(Err(e)),
                Ok(()) => self.next(),
            };
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum RawToken {
    // Keywords
    Languages,  // languages
    Parameters, // parameters
    Features,   // features
    Characters, // characters
    Evolve,     // evolve
    To,         // to

    // Special characters
    Output,        // >
    Environment,   // /
    InputLocation, // _
    WordBoundry,   // #
    EOL,           // ;
    Comment,       // //

    // Section markers
    BlockOpen,                         // {
    BlockClose,                        // }
    MatchingPhonemeOpen,               // (
    TaggedMatchingPhonemeOpen(String), // █(
    MatchingPhonemeClose,              // )
    ConcretePhonemeOpen,               // [
    ConcretePhonemeClose,              // ]

    // General tokens
    PositiveIdentifier(String), // +███
    NegativeIdentifier(String), // -███
    UnmarkedIdentifier(String), //  ███

    UnknownCharacter(char),
}

fn mark_token(token: RawToken, pos: Position) -> Token {
    Token { token, pos }
}

pub struct Token {
    pub token: RawToken,
    pub pos: Position,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:}::{:?}", self.pos, self.token)
    }
}

// This is a unicode grapheme that also contains information about where in
// the file it occurs
struct MarkedChar {
    grapheme: char,
    pos: Position,
}

impl MarkedChar {
    // Generates a closure for a specific line number that can be used in a
    // map after .graphemes().enumerate()
    fn from_enumerated_grapheme(line: usize) -> impl Fn((usize, char)) -> MarkedChar {
        move |(i, grapheme): (usize, char)| MarkedChar {
            grapheme,
            pos: Position { line, char: i + 1 },
        }
    }
}

fn intra_identifier_character(c: char) -> bool {
    c.is_alphanumeric() || c == '.'
}

fn lex_raw_identifier<T: Iterator<Item = MarkedChar>>(line: &mut Peekable<T>) -> RawToken {
    // let identifier = line.peeking_take_while(|c| c.grapheme.is_alphanumeric()).map(|c| c.grapheme).collect();
    let identifier = line
        .peeking_take_while(|c| intra_identifier_character(c.grapheme))
        .map(|c| c.grapheme)
        .collect();

    // Check if this is the opening of a tagged matching phoneme
    if line.peeking_next(|c| c.grapheme == '(').is_some() {
        return RawToken::TaggedMatchingPhonemeOpen(identifier);
    }

    return match identifier.as_str() {
        "languages" => RawToken::Languages,
        "parameters" => RawToken::Parameters,
        "features" => RawToken::Features,
        "characters" => RawToken::Characters,
        "evolve" => RawToken::Evolve,
        "to" => RawToken::To,
        _ => RawToken::UnmarkedIdentifier(identifier),
    };
}

fn lex_pos_identifier<T: Iterator<Item = MarkedChar>>(line: &mut Peekable<T>) -> RawToken {
    let identifier = line
        .peeking_take_while(|c| intra_identifier_character(c.grapheme))
        .map(|c| c.grapheme)
        .collect();
    return RawToken::PositiveIdentifier(identifier);
}

fn lex_neg_identifier<T: Iterator<Item = MarkedChar>>(line: &mut Peekable<T>) -> RawToken {
    let identifier = line
        .peeking_take_while(|c| intra_identifier_character(c.grapheme))
        .map(|c| c.grapheme)
        .collect();
    return RawToken::NegativeIdentifier(identifier);
}

fn lex_token<T: Iterator<Item = MarkedChar>>(line: &mut Peekable<T>) -> Option<Token> {
    while let Some(marked_char) = line.peek() {
        let c = marked_char.grapheme;
        let pos = marked_char.pos.clone();

        if c.is_whitespace() {
            line.next();
            continue;
        } else if c.is_alphanumeric() {
            return Some(mark_token(lex_raw_identifier(line), pos));
        } else if c == '+' {
            line.next();
            return Some(mark_token(lex_pos_identifier(line), pos));
        } else if c == '-' {
            line.next();
            return Some(mark_token(lex_neg_identifier(line), pos));
        }

        let to_return = match c {
            '>' => RawToken::Output,
            '/' => RawToken::Environment,
            '_' => RawToken::InputLocation,
            '#' => RawToken::WordBoundry,
            ';' => RawToken::EOL,

            '{' => RawToken::BlockOpen,
            '}' => RawToken::BlockClose,
            '(' => RawToken::MatchingPhonemeOpen,
            ')' => RawToken::MatchingPhonemeClose,
            '[' => RawToken::ConcretePhonemeOpen,
            ']' => RawToken::ConcretePhonemeClose,

            unknown => RawToken::UnknownCharacter(unknown),
        };

        line.next();

        if to_return == RawToken::Environment {
            if line.peek().is_some_and(|c| c.grapheme == '/') {
                line.next();
                return Some(mark_token(RawToken::Comment, pos));
            }
        }
        return Some(mark_token(to_return, pos));
    }

    return None;
}
