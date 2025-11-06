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

pub struct Environment {
    pub match_word_start: bool,
    pub match_word_end: bool,
    pub pre_environment: Vec<EnvironmentAtom>,
    pub post_environment: Vec<EnvironmentAtom>,
}

impl std::fmt::Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.match_word_start {
            write!(f, "# ")?;
        }

        for atom in &self.pre_environment {
            write!(f, "{:?} ", atom)?;
        }
        write!(f, "_")?;
        for atom in &self.post_environment {
            write!(f, " {:?}", atom)?;
        }

        if self.match_word_end {
            write!(f, " #")?;
        }
        Ok(())
    }
}

pub struct Rule {
    pub input: Vec<InputAtom>,
    pub output: Vec<UnboundPhoneme>,
    pub environment: Option<Environment>,
}

impl std::fmt::Debug for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for atom in &self.input {
            write!(f, "{:?} ", atom)?;
        }
        write!(f, ">")?;
        for atom in &self.output {
            write!(f, " {:?}", atom)?;
        }

        if let Some(env) = &self.environment {
            write!(f, " / {:?}", env)?;
        }

        Ok(())
    }
}

// This scans through the word trying to apply the rule, and if it applies it
// successfully, it will go back to the beginning of the word and try again,
// until the word can no longer be evolved with the rule.
pub fn do_rule<'a>(word: Vec<Phoneme>, rule: &Rule) -> Vec<Phoneme> {
    _do_rule(word, rule, 0)
}

fn _do_rule<'a>(word: Vec<Phoneme>, rule: &Rule, start: usize) -> Vec<Phoneme> {
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

    match do_rule_from_pos(&word, rule, start) {
        Some(new_word) => _do_rule(new_word, rule, 0),
        None => _do_rule(word, rule, start + 1),
    }
}

// Tries to match the rule beginning at start. This will match word end
// boundaries, but it will not match word start boundaries
fn do_rule_from_pos<'a>(word: &[Phoneme], rule: &Rule, start: usize) -> Option<Vec<Phoneme>> {
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
    ))
}

fn modify_word(
    rule_output: &[UnboundPhoneme],
    pre: &[Phoneme],
    post: &[Phoneme],
    selection_table: &HashMap<SelectorCode, &Phoneme>,
) -> Vec<Phoneme> {
    // Clones galore! May the Omnissiah grant me the ability to be gentler with
    // the memories of Its Machine Spirits.
    pre.iter()
        .cloned()
        .chain(bind_output(rule_output, selection_table))
        .chain(post.iter().cloned())
        .collect()
}

fn bind_output(
    rule_output: &[UnboundPhoneme],
    selection_table: &HashMap<SelectorCode, &Phoneme>,
) -> impl Iterator<Item = Phoneme> {
    rule_output.iter().map(|up| up.bind(selection_table))
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

    if end != start + env.len() {
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

    if end != start + input.len() {
        None
    } else {
        Some((end, selection_map))
    }
}
