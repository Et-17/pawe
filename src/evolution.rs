use crate::phonemes::{Filter, Phoneme, Selector, SelectorCode, UnboundPhoneme};
use std::collections::HashMap;

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

impl std::fmt::Debug for InputAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InputAtom::Selector(selector) => selector.fmt(f),
            InputAtom::Filter(filter) => filter.fmt(f),
            InputAtom::Phoneme(phoneme) => phoneme.fmt(f),
        }
    }
}

pub enum EnvironmentAtom {
    Filter(Filter),
    Phoneme(Phoneme),
}

impl EnvironmentAtom {
    pub fn matches(&self, other: &Phoneme) -> bool {
        match self {
            Self::Filter(filter) => filter.matches(other),
            Self::Phoneme(phoneme) => phoneme.matches(other),
        }
    }
}

impl std::fmt::Debug for EnvironmentAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EnvironmentAtom::Filter(filter) => filter.fmt(f),
            EnvironmentAtom::Phoneme(phoneme) => phoneme.fmt(f),
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

#[derive(Debug)]
pub struct Rule {
    pub input: Vec<InputAtom>,
    pub output: Vec<UnboundPhoneme>,
    pub environment: Option<Environment>,
}

// This scans through the word trying to apply the rule, and if it applies it
// successfully, it will go back to the beginning of the word and try again,
// until the word can no longer be evolved with the rule.
pub fn do_rule<'a>(word: &'a [Phoneme], rule: &Rule) -> &'a [Phoneme] {
    _do_rule(word, rule, 0)
}

fn _do_rule<'a>(word: &'a [Phoneme], rule: &Rule, start: usize) -> &'a [Phoneme] {
    // If we reach the end of the word, stop
    if start >= word.len() {
        return word;
    }

    // If we are no longer at the beginning, but the rule says to only be
    // applied at the beginning, stop
    if let Some(env) = &rule.environment {
        if env.match_word_start && start > 0 {
            return word;
        }
    }

    match do_rule_from_pos(word, rule, start) {
        Some(new_word) => _do_rule(new_word, rule, 0),
        None => _do_rule(word, rule, start + 1),
    }
}

// Tries to match the rule beginning at start. This will match word end
// boundaries, but it will not match word start boundaries
fn do_rule_from_pos<'a>(word: &[Phoneme], rule: &Rule, start: usize) -> Option<&'a [Phoneme]> {
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
    let mut end_post_environment = end_input;
    if let Some(env) = &rule.environment {
        end_post_environment =
            match_environment(&word[end_input..], &env.post_environment, end_input)?;

        if env.match_word_end && end_post_environment < word.len() {
            return None;
        }
    }

    println!(
        "Found valid rule, with S: {start}, ePre: {end_pre_environment}, eI: {end_input}, and ePost: {end_post_environment}"
    );
    println!("{rule:?}");

    None
}

// Word boundary matching is not performed by this function, that must be
// performed by the caller.
// Start is provided just so that we can return the right environment stop
// position, we expect word to already be subsliced.
// Returns the index of the phoneme after the matched environment, ie the last
// index of the environment + 1.
pub fn match_environment(word: &[Phoneme], env: &[EnvironmentAtom], start: usize) -> Option<usize> {
    let mut end = start;

    for (base, matcher) in word.iter().zip(env.iter()) {
        if !matcher.matches(base) {
            return None;
        }
        end += 1;
    }

    if env.len() > 0 && end == start {
        None
    } else {
        Some(end)
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

    if input.len() > 0 && end == start {
        None
    } else {
        Some((end, selection_map))
    }
}
