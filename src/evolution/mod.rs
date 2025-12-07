use itertools::PeekingNext;

use crate::config::{Character, DiacriticMap};
use crate::phonemes::{Filter, Phoneme, Selector, SelectorCode, UnboundPhoneme};
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::iter::Peekable;

pub mod routing;

#[derive(Debug)]
pub enum InputAtom {
    Selector(Selector),
    Filter(Filter),
    Phoneme(Phoneme),
}

impl InputAtom {
    pub fn matches(&self, other: &Phoneme) -> bool {
        match self {
            Self::Selector(selector) => selector.filter.matches(other),
            Self::Filter(filter) => filter.matches(other),
            Self::Phoneme(phoneme) => phoneme.matches(other),
        }
    }
}

impl Display for InputAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InputAtom::Selector(selector) => Display::fmt(selector, f),
            InputAtom::Filter(filter) => Display::fmt(filter, f),
            InputAtom::Phoneme(phoneme) => Display::fmt(phoneme, f),
        }
    }
}

#[derive(Debug)]
pub enum EnvironmentAtom {
    Filter(Filter),
    Phoneme(Phoneme),
    Optional(Box<EnvironmentAtom>),
    ZeroOrMore(Box<EnvironmentAtom>),
    Not(Box<EnvironmentAtom>),
}

impl Display for EnvironmentAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EnvironmentAtom::Filter(filter) => write!(f, "{filter}"),
            EnvironmentAtom::Phoneme(phoneme) => write!(f, "{phoneme}"),
            EnvironmentAtom::Optional(atom) => write!(f, "{atom}?"),
            EnvironmentAtom::ZeroOrMore(atom) => write!(f, "{atom}*"),
            EnvironmentAtom::Not(atom) => write!(f, "{atom}!"),
        }
    }
}

#[derive(Debug)]
pub struct Environment {
    pub match_word_start: bool,
    pub match_word_end: bool,
    pub pre_environment: Vec<EnvironmentAtom>,
    pub post_environment: Vec<EnvironmentAtom>,
}

impl Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.match_word_start {
            write!(f, "# ")?;
        }

        for atom in &self.pre_environment {
            write!(f, "{atom} ")?;
        }
        write!(f, "_")?;
        for atom in &self.post_environment {
            write!(f, " {atom}")?;
        }

        if self.match_word_end {
            write!(f, " #")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Rule {
    pub input: Vec<InputAtom>,
    pub output: Vec<UnboundPhoneme>,
    pub environment: Option<Environment>,
}

impl Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for atom in &self.input {
            write!(f, "{} ", atom)?;
        }
        write!(f, ">")?;
        for atom in &self.output {
            write!(f, " {}", atom)?;
        }

        if let Some(env) = &self.environment {
            write!(f, " / {}", env)?;
        }

        Ok(())
    }
}

// This scans through the word trying to apply the rule, and if it applies it
// successfully, it will continue to scan through the new word.
pub fn do_rule<'a>(
    word: &[Phoneme],
    rule: &Rule,
    character_table: &HashMap<String, Character>,
    diacritic_table: &DiacriticMap,
) -> Option<Vec<Phoneme>> {
    _do_rule(&word, rule, character_table, diacritic_table, 0)
}

fn _do_rule<'a>(
    word: &[Phoneme],
    rule: &Rule,
    character_table: &HashMap<String, Character>,
    diacritic_table: &DiacriticMap,
    start: usize,
) -> Option<Vec<Phoneme>> {
    // If we reach the end of the word, stop
    if start >= word.len() {
        return None;
    }

    // If we are no longer at the beginning, but the rule says to only be
    // applied at the beginning, stop
    if let Some(env) = &rule.environment {
        if env.match_word_start && start > 0 {
            return None;
        }
    }

    match do_rule_from_pos(&word, rule, character_table, diacritic_table, start) {
        Some(new_word) => {
            match _do_rule(&new_word, rule, character_table, diacritic_table, start + 1) {
                Some(new_word2) => Some(new_word2),
                None => Some(new_word),
            }
        }
        None => _do_rule(word, rule, character_table, diacritic_table, start + 1),
    }
}

// Tries to match the rule beginning at start. This will match word end
// boundaries, but it will not match word start boundaries.
fn do_rule_from_pos<'a>(
    word: &[Phoneme],
    rule: &Rule,
    character_table: &HashMap<String, Character>,
    diacritic_table: &DiacriticMap,
    start: usize,
) -> Option<Vec<Phoneme>> {
    // Preënvironment
    let mut end_pre_environment = start;
    if let Some(env) = &rule.environment {
        end_pre_environment = match_environment(&word[start..], &env.pre_environment, start)?;
    }

    // Input
    let (end_input, selection_table) = match_input(
        &word[end_pre_environment..],
        &rule.input,
        end_pre_environment,
    )?;

    //Postenvironment
    if let Some(env) = &rule.environment {
        let end_post_environment =
            match_environment(&word[end_input..], &env.post_environment, end_input)?;

        if env.match_word_end && end_post_environment != word.len() {
            return None;
        }
    }

    Some(modify_word(
        &rule.output,
        &word[..end_pre_environment],
        &word[end_input..],
        &selection_table,
        character_table,
        diacritic_table,
    ))
}

fn modify_word(
    rule_output: &[UnboundPhoneme],
    pre: &[Phoneme],
    post: &[Phoneme],
    selection_table: &HashMap<SelectorCode, &Phoneme>,
    character_table: &HashMap<String, Character>,
    diacritic_table: &DiacriticMap,
) -> Vec<Phoneme> {
    // Clones galore! May the Omnissiah grant me the ability to be gentler with
    // the memories of Its Machine Spirits.
    pre.iter()
        .cloned()
        .chain(bind_output(
            rule_output,
            selection_table,
            character_table,
            diacritic_table,
        ))
        .chain(post.iter().cloned())
        .collect()
}

fn bind_output(
    rule_output: &[UnboundPhoneme],
    selection_table: &HashMap<SelectorCode, &Phoneme>,
    character_table: &HashMap<String, Character>,
    diacritic_table: &DiacriticMap,
) -> impl Iterator<Item = Phoneme> {
    rule_output
        .iter()
        .map(|up| up.bind(selection_table))
        .map(|p| p.rebase(character_table, diacritic_table))
}

// Word boundary matching is not performed by this function, that must be
// performed by the caller.
// Start is provided just so that we can return the right environment stop
// position, we expect word to already be subsliced.
// Returns the index of the phoneme after the matched environment, ie the last
// index of the environment + 1.
pub fn match_environment(word: &[Phoneme], env: &[EnvironmentAtom], start: usize) -> Option<usize> {
    let mut end = start;

    let mut word_iter = word.iter().peekable();
    let mut env_iter = env.iter();

    while let Some(atom) = env_iter.next() {
        end += match_environment_atom(atom, &mut word_iter, false)?;
    }

    Some(end)
}

// Matches an environment atom to the environment, returning either None if it
// fails to match, or Some with the number of environment phonemes consumed
fn match_environment_atom<'a>(
    matcher: &EnvironmentAtom,
    word: &mut Peekable<impl Iterator<Item = &'a Phoneme>>,
    invert: bool,
) -> Option<usize> {
    match matcher {
        EnvironmentAtom::Filter(filter) => word
            .peeking_next(|w_phoneme| filter.matches(w_phoneme) ^ invert)
            .map(|_| 1),
        EnvironmentAtom::Phoneme(phoneme) => word
            .peeking_next(|w_phoneme| phoneme.matches(w_phoneme) ^ invert)
            .map(|_| 1),
        EnvironmentAtom::Optional(atom) => match_environment_atom(&atom, word, invert).or(Some(1)),
        EnvironmentAtom::ZeroOrMore(atom) => match_environment_atom(&atom, word, invert)
            .map(|c| c + match_environment_atom(matcher, word, invert).unwrap_or(0)),
        EnvironmentAtom::Not(atom) => match_environment_atom(&atom, word, !invert),
    }
}

pub fn match_input<'a>(
    word: &'a [Phoneme],
    input: &[InputAtom],
    start: usize,
) -> Option<(usize, HashMap<SelectorCode, &'a Phoneme>)> {
    let mut end = start;
    let mut selection_map = HashMap::new();

    for (base, matcher) in word.iter().zip(input.iter()) {
        if !matcher.matches(base) {
            return None;
        }
        end += 1;

        if let InputAtom::Selector(selector) = matcher {
            selection_map.insert(selector.code, base);
        }
    }

    if end != start + input.len() {
        None
    } else {
        Some((end, selection_map))
    }
}
