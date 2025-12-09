use std::collections::VecDeque;
use std::fmt::Debug;
use std::io::BufRead;
use std::iter::Peekable;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use itertools::{Itertools, PeekingNext};
use unicode_normalization::UnicodeNormalization;

use crate::cli::RAW_INPUT;
use crate::error_handling::{FilePosition, Result, wrap_io_error};
use crate::phonemes::SelectorCode;

pub struct Lexer<T: BufRead> {
    file: T,
    current_line_tokens: VecDeque<Token>,
    line_num: usize,
    path: Option<Rc<Path>>, // For generating errors
}

impl<T: BufRead> Lexer<T> {
    fn lex_line(&mut self) -> Option<Result<()>> {
        let mut next_line = String::new();
        let read_amount = self.file.read_line(&mut next_line);

        if let Ok(0) = read_amount {
            return None;
        } else if let Err(e) = read_amount {
            let file_pos = FilePosition::new(self.path.as_ref(), Some(self.line_num + 1), None);
            let wrapped_error = wrap_io_error("lexer", Some(&file_pos))(e);
            return Some(Err(wrapped_error));
        }

        self.line_num += 1;

        let chars = next_line.chars().enumerate();
        if unsafe { !RAW_INPUT } {
            let decomposed =
                chars.flat_map(|(column, raw)| raw.nfd().map(move |dec| (column, dec)));

            self.add_line_to_buffer(decomposed);
        } else {
            self.add_line_to_buffer(chars);
        }

        Some(Ok(()))
    }

    fn add_line_to_buffer(&mut self, line: impl Iterator<Item = (usize, char)>) {
        let mark = MarkedChar::from_enumerated_grapheme(self.line_num, self.path.as_ref());
        let mut marked = line.map(mark).peekable();

        while marked.peek().is_some() {
            let next_token_opt = lex_token(&mut marked);

            if let Some(t) = next_token_opt {
                if t.token == RawToken::Comment {
                    break; // Stop processing this line
                }

                self.current_line_tokens.push_back(t);
            } else {
                break;
            }
        }
    }

    pub fn lex(input: T, path: Option<PathBuf>) -> Lexer<T> {
        Lexer {
            file: input,
            current_line_tokens: VecDeque::new(),
            line_num: 0,
            path: path.map(Into::into),
        }
    }
}

impl<T: BufRead> Iterator for Lexer<T> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.current_line_tokens.is_empty() {
            if let Err(e) = self.lex_line()? {
                return Some(Err(e));
            }
        }

        Ok(self.current_line_tokens.pop_front()).transpose()
    }
}

#[derive(Debug, PartialEq)]
pub enum RawToken {
    // Keywords
    Languages,  // languages
    Parameters, // parameters
    Features,   // features
    Characters, // characters
    Diacritics, // diacritics
    Evolve,     // evolve
    To,         // to

    // Special characters
    Output,      // >
    Environment, // /
    Target,      // _
    WordBoundry, // #
    Optional,    // ?
    ZeroOrMore,  // *
    Not,         // !
    Eol,         // ;
    Comment,     // //

    // Section markers
    BlockOpen,                  // {
    BlockClose,                 // }
    FilterOpen,                 // (
    SelectorOpen(SelectorCode), // █(
    FilterSelectorClose,        // )
    PhonemeOpen,                // [
    PhonemeClose,               // ]

    // General tokens
    MarkedFeature(bool, String),           // ±███
    MarkedParameter(bool, String, String), // ±███.███
    SelectorCode(SelectorCode),            //  N
    UnmarkedIdentifier(String),            //  ███

    UnknownCharacter(char),
}

fn mark_token(token: RawToken, pos: FilePosition) -> Token {
    Token { token, pos }
}

pub struct Token {
    pub token: RawToken,
    pub pos: FilePosition,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:}::{:?}", self.pos, self.token)
    }
}

// This is a unicode grapheme that also contains information about where in
// the file it occurs
#[derive(Debug)]
struct MarkedChar {
    grapheme: char,
    pos: FilePosition,
}

impl MarkedChar {
    // Generates a closure for a specific line number that can be used in a
    // map after .graphemes().enumerate()
    fn from_enumerated_grapheme(
        line: usize,
        path: Option<&Rc<Path>>,
    ) -> impl Fn((usize, char)) -> MarkedChar {
        move |(i, grapheme): (usize, char)| MarkedChar {
            grapheme,
            pos: FilePosition::new(path, Some(line), Some(i + 1)),
        }
    }

    fn is_whitespace(&self) -> bool {
        self.grapheme.is_whitespace()
    }
}

const FORBIDDEN_IDENTIFIER_CHARACTERS: &str = "{}()[]/>*?!;";

fn intra_identifier_character(c: char) -> bool {
    !c.is_whitespace() && FORBIDDEN_IDENTIFIER_CHARACTERS.chars().all(|fc| fc != c)
}

fn read_raw_identifier<T: Iterator<Item = MarkedChar>>(line: &mut Peekable<T>) -> String {
    line.peeking_take_while(|c| intra_identifier_character(c.grapheme))
        .map(|c| c.grapheme)
        .collect()
}

fn lex_unmarked_identifier<T: Iterator<Item = MarkedChar>>(line: &mut Peekable<T>) -> RawToken {
    let identifier = read_raw_identifier(line);

    match identifier.as_str() {
        "languages" => RawToken::Languages,
        "parameters" => RawToken::Parameters,
        "features" => RawToken::Features,
        "characters" => RawToken::Characters,
        "diacritics" => RawToken::Diacritics,
        "evolve" => RawToken::Evolve,
        "to" => RawToken::To,
        _ => {
            if identifier.chars().all(char::is_numeric)
                && let Ok(code) = identifier.parse::<SelectorCode>()
            {
                if line.peeking_next(|c| c.grapheme == '(').is_some() {
                    return RawToken::SelectorOpen(code);
                }
                return RawToken::SelectorCode(code);
            }

            RawToken::UnmarkedIdentifier(identifier)
        }
    }
}

fn lex_marked_identifier<T: Iterator<Item = MarkedChar>>(
    line: &mut Peekable<T>,
    mark: bool,
) -> RawToken {
    line.next(); // skip mark
    let identifier = read_raw_identifier(line);

    if let Some((name, variant)) = identifier.split_once('.') {
        RawToken::MarkedParameter(mark, name.to_string(), variant.to_string())
    } else {
        RawToken::MarkedFeature(mark, identifier)
    }
}

fn lex_token<T: Iterator<Item = MarkedChar>>(line: &mut Peekable<T>) -> Option<Token> {
    line.peeking_take_while(MarkedChar::is_whitespace).count();
    let pos = line.peek()?.pos.clone();

    let initial = lex_character(line)?;

    let potential_comment = match initial {
        RawToken::Environment => test_comment(line),
        others => others,
    };

    Some(mark_token(potential_comment, pos))
}

fn lex_character<T: Iterator<Item = MarkedChar>>(line: &mut Peekable<T>) -> Option<RawToken> {
    let next = line.peek()?.grapheme;

    if next == '+' {
        return Some(lex_marked_identifier(line, true));
    } else if next == '-' {
        return Some(lex_marked_identifier(line, false));
    } else if next.is_alphanumeric() {
        return Some(lex_unmarked_identifier(line));
    }

    let to_return = Some(match next {
        '>' => RawToken::Output,
        '/' => RawToken::Environment,
        '_' => RawToken::Target,
        '#' => RawToken::WordBoundry,
        '?' => RawToken::Optional,
        '*' => RawToken::ZeroOrMore,
        '!' => RawToken::Not,
        ';' => RawToken::Eol,

        '{' => RawToken::BlockOpen,
        '}' => RawToken::BlockClose,
        '(' => RawToken::FilterOpen,
        ')' => RawToken::FilterSelectorClose,
        '[' => RawToken::PhonemeOpen,
        ']' => RawToken::PhonemeClose,

        unknown => RawToken::UnknownCharacter(unknown),
    });

    line.next();
    to_return
}

fn test_comment<T: Iterator<Item = MarkedChar>>(line: &mut Peekable<T>) -> RawToken {
    if line.peek().is_some_and(|c| c.grapheme == '/') {
        RawToken::Comment
    } else {
        RawToken::Environment
    }
}
