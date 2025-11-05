use std::collections::HashMap;
use std::fmt::Debug;

use itertools::Itertools;

use crate::config::Label;

pub type SelectorCode = u8;

pub enum Attribute {
    Feature(bool, Label),
    Parameter(bool, Label, Label),
    Character(Phoneme),
    Selection(SelectorCode),
}

impl Debug for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Attribute::Feature(mark, label) => write!(f, "{}{}", mark_char(*mark), label),
            Attribute::Parameter(mark, param, var) => {
                write!(f, "{}{}.{}", mark_char(*mark), param, var)
            }
            Attribute::Character(phoneme) => write!(f, "{:?}", phoneme),
            Attribute::Selection(code) => write!(f, "{}", code),
        }
    }
}

#[derive(Clone)]
pub struct Phoneme {
    pub features: HashMap<Label, bool>,
    pub parameters: HashMap<Label, Label>,
}

impl Phoneme {
    pub fn new() -> Self {
        Self {
            features: HashMap::new(),
            parameters: HashMap::new(),
        }
    }

    // Adds all the attributes from the given phoneme to this phoneme, so that
    // you can write defined characters in phoneme blocks to modify them
    pub fn add_phoneme(&mut self, phoneme: Phoneme) -> () {
        self.features.extend(phoneme.features);
        self.parameters.extend(phoneme.parameters);
    }

    // This function will discard selector references and negative parameters
    pub fn add_attribute(&mut self, attribute: Attribute) -> () {
        match attribute {
            Attribute::Feature(mark, feat) => {
                self.features.insert(feat, mark);
            }
            Attribute::Parameter(_, param, variant) => {
                self.parameters.insert(param, variant);
            }
            Attribute::Character(definition) => {
                self.add_phoneme(definition);
            }
            Attribute::Selection(_) => (),
        }
    }

    pub fn matches(&self, other: &Phoneme) -> bool {
        Self::features_matches(&self.features, &other.features)
            && Self::parameters_matches(&self.parameters, &other.parameters)
    }

    fn features_matches(pattern: &HashMap<Label, bool>, other: &HashMap<Label, bool>) -> bool {
        pattern
            .iter()
            .all(|(feat, mark)| other.get(feat).is_some_and(|o| mark == o))
    }

    fn parameters_matches(pattern: &HashMap<Label, Label>, other: &HashMap<Label, Label>) -> bool {
        pattern
            .iter()
            .all(|(param, var)| other.get(param).is_some_and(|o_var| var == o_var))
    }
}

impl FromIterator<Attribute> for Phoneme {
    fn from_iter<T: IntoIterator<Item = Attribute>>(iter: T) -> Self {
        let mut phoneme = Self::new();

        for attribute in iter {
            phoneme.add_attribute(attribute);
        }

        phoneme
    }
}

impl Debug for Phoneme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let features_strs = self
            .features
            .iter()
            .sorted_by_key(|x| x.0.code)
            .map(|(label, &mark)| format!("{}{}", mark_char(mark), label));
        let parameter_strs = self
            .parameters
            .iter()
            .sorted_by_key(|x| x.0.code)
            .map(|(param, var)| format!("+{}.{}", param, var));

        write!(f, "[{}]", features_strs.chain(parameter_strs).join(" "))
    }
}

pub struct UnboundPhoneme {
    pub attributes: Vec<Attribute>,
}

impl FromIterator<Attribute> for UnboundPhoneme {
    fn from_iter<T: IntoIterator<Item = Attribute>>(iter: T) -> Self {
        Self {
            attributes: iter.into_iter().collect(),
        }
    }
}

impl Debug for UnboundPhoneme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let attributes_strs = self
            .attributes
            .iter()
            .map(|attr| format!("{:?}", attr))
            .join(" ");

        write!(f, "*[{}]*", attributes_strs)
    }
}

pub struct Selector {
    pub code: SelectorCode,
    pub filter: Filter,
}

impl Debug for Selector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{:?}", self.code, self.filter)
    }
}

pub struct Filter {
    pub features: HashMap<Label, bool>,
    pub parameters: HashMap<Label, (bool, Label)>,
}

impl Filter {
    pub fn new() -> Self {
        Self {
            features: HashMap::new(),
            parameters: HashMap::new(),
        }
    }

    // This function will discard selector refernces and characters
    pub fn add_attribute(&mut self, attribute: Attribute) -> () {
        match attribute {
            Attribute::Feature(mark, feat) => {
                self.features.insert(feat, mark);
            }
            Attribute::Parameter(mark, param, variant) => {
                self.parameters.insert(param, (mark, variant));
            }
            _ => (),
        }
    }

    pub fn matches(&self, other: &Phoneme) -> bool {
        Self::features_matches(&self.features, &other.features)
            && Self::parameters_matches(&self.parameters, &other.parameters)
    }

    fn features_matches(pattern: &HashMap<Label, bool>, other: &HashMap<Label, bool>) -> bool {
        pattern
            .iter()
            .all(|(feat, mark)| other.get(feat).is_some_and(|o| mark == o))
    }

    fn parameters_matches(
        pattern: &HashMap<Label, (bool, Label)>,
        other: &HashMap<Label, Label>,
    ) -> bool {
        pattern.iter().all(|(param, (mark, var))| {
            other
                .get(param)
                .is_some_and(|o_var| (var == o_var) == *mark)
        })
    }
}

impl FromIterator<Attribute> for Filter {
    fn from_iter<T: IntoIterator<Item = Attribute>>(iter: T) -> Self {
        let mut filter = Self::new();

        for attribute in iter {
            filter.add_attribute(attribute);
        }

        return filter;
    }
}

impl Debug for Filter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let features_strs = self
            .features
            .iter()
            .sorted_by_key(|x| x.0.code)
            .map(|(label, &mark)| format!("{}{}", mark_char(mark), label));
        let parameter_strs = self
            .parameters
            .iter()
            .sorted_by_key(|x| x.0.code)
            .map(|(param, (mark, var))| format!("{}{}.{}", mark_char(*mark), param, var));

        write!(f, "({})", features_strs.chain(parameter_strs).join(" "))
    }
}

fn mark_char(mark: bool) -> char {
    if mark { '+' } else { '-' }
}
