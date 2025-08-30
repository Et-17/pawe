use crate::phonemes::{Filter, Phoneme, Selector, UnboundPhoneme};

#[derive(Debug)]
pub enum InputAtom {
    Selector(Selector),
    Filter(Filter),
    Phoneme(Phoneme),
}

#[derive(Debug)]
pub enum EnvironmentAtom {
    Filter(Filter),
    Phoneme(Phoneme),
    WordBoundry,
}

#[derive(Debug)]
pub struct Rule {
    pub input: Vec<InputAtom>,
    pub output: Vec<UnboundPhoneme>,
    pub environment: Option<(Vec<EnvironmentAtom>, Vec<EnvironmentAtom>)>,
}
