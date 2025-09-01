use crate::phonemes::{Filter, Phoneme, Selector, UnboundPhoneme};

pub enum InputAtom {
    Selector(Selector),
    Filter(Filter),
    Phoneme(Phoneme),
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
