use std::collections::HashMap;

use crate::config::Label;

pub type SelectorCode = u8;

#[derive(Debug)]
pub enum Attribute {
    Feature(bool, Label),
    Parameter(bool, Label, Label),
    Character(Phoneme),
    Selection(SelectorCode),
}

#[derive(Debug, Clone)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Selector {
    pub code: SelectorCode,
    pub filter: Filter,
}

#[derive(Debug)]
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
