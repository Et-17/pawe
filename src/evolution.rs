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
    WordBoundry,
}

impl std::fmt::Debug for EnvironmentAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EnvironmentAtom::Filter(filter) => filter.fmt(f),
            EnvironmentAtom::Phoneme(phoneme) => phoneme.fmt(f),
            EnvironmentAtom::WordBoundry => write!(f, "#WordBoundary#"),
        }
    }
}

#[derive(Debug)]
pub struct Rule {
    pub input: Vec<InputAtom>,
    pub output: Vec<UnboundPhoneme>,
    pub environment: Option<(Vec<EnvironmentAtom>, Vec<EnvironmentAtom>)>,
}
