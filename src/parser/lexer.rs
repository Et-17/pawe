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
    // Whether the current line is ended or not, so the lexer can insert an EOL
    // before a block close if it's not
    insert_eol: bool,
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
            insert_eol: false,
        })
    }
}

impl Iterator for FileLexer {
    type Item = super::PResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.current_line_tokens.is_empty() {
            if let Err(e) = self.lex_line()? {
                return Some(Err(e));
            }
        }

        let next_token = self.current_line_tokens.pop_front()?;

        if next_token.token == RawToken::BlockClose && self.insert_eol {
            self.insert_eol = false;
            let fake_eol = Token {
                token: RawToken::EOL,
                pos: next_token.pos,
            };
            self.current_line_tokens.push_front(next_token);
            return Some(Ok(fake_eol));
        }

        self.insert_eol = next_token.token != RawToken::EOL;

        Some(Ok(next_token))
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
    BlockOpen,            // {
    BlockClose,           // }
    FilterOpen,           // (
    SelectorOpen(String), // █(
    FilterSelectorClose,  // )
    PhonemeOpen,          // [
    PhonemeClose,         // ]

    // General tokens
    MarkedFeature(bool, String),           // ±███
    MarkedParameter(bool, String, String), // ±███.███
    UnmarkedIdentifier(String),            //  ███

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
    c.is_alphanumeric() || c == '.' || c == '_'
}

fn read_raw_identifier<T: Iterator<Item = MarkedChar>>(line: &mut Peekable<T>) -> String {
    line.peeking_take_while(|c| intra_identifier_character(c.grapheme))
        .map(|c| c.grapheme)
        .collect()
}

fn lex_raw_identifier<T: Iterator<Item = MarkedChar>>(line: &mut Peekable<T>) -> RawToken {
    let identifier = read_raw_identifier(line);

    // Check if this is the opening of a selector
    if line.peeking_next(|c| c.grapheme == '(').is_some() {
        return RawToken::SelectorOpen(identifier);
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

fn lex_marked_identifier<T: Iterator<Item = MarkedChar>>(
    line: &mut Peekable<T>,
    mark: bool,
) -> RawToken {
    let identifier = read_raw_identifier(line);

    if let Some((name, variant)) = identifier.split_once('.') {
        RawToken::MarkedParameter(mark, name.to_string(), variant.to_string())
    } else {
        RawToken::MarkedFeature(mark, identifier)
    }
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
            return Some(mark_token(lex_marked_identifier(line, true), pos));
        } else if c == '-' {
            line.next();
            return Some(mark_token(lex_marked_identifier(line, false), pos));
        }

        let to_return = match c {
            '>' => RawToken::Output,
            '/' => RawToken::Environment,
            '_' => RawToken::InputLocation,
            '#' => RawToken::WordBoundry,
            ';' => RawToken::EOL,

            '{' => RawToken::BlockOpen,
            '}' => RawToken::BlockClose,
            '(' => RawToken::FilterOpen,
            ')' => RawToken::FilterSelectorClose,
            '[' => RawToken::PhonemeOpen,
            ']' => RawToken::PhonemeClose,

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
