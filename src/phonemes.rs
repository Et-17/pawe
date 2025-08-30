use std::collections::HashMap;

pub type SelectorCode = u8;

#[derive(Debug)]
pub enum Attribute {
    Feature(bool, u32),
    Parameter(bool, u32, u32),
    Character(Phoneme),
    Selection(SelectorCode),
}

#[derive(Debug, Clone)]
pub struct Phoneme {
    pub features: HashMap<u32, bool>,
    pub parameters: HashMap<u32, u32>,
}

impl Phoneme {
    pub fn new() -> Self {
        Self {
            features: HashMap::new(),
            parameters: HashMap::new(),
        }
    }

    pub fn from_attributes(attributes: Vec<Attribute>) -> Self {
        let mut phoneme = Self::new();

        for attribute in attributes {
            phoneme.add_attribute(attribute);
        }

        return phoneme;
    }

    // Adds all the attributes from the given phoneme to this phoneme, so that
    // you can write defined characters in phoneme blocks to modify them
    pub fn add_phoneme(&mut self, phoneme: Phoneme) -> () {
        self.features.extend(phoneme.features.iter());
        self.parameters.extend(phoneme.parameters.iter());
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

#[derive(Debug)]
pub struct UnboundPhoneme {
    pub attributes: Vec<Attribute>,
}

impl UnboundPhoneme {
    pub fn new() -> Self {
        Self {
            attributes: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Selector {
    pub code: SelectorCode,
    pub filter: Filter,
}

impl Selector {
    pub fn new(code: SelectorCode) -> Self {
        Self {
            code,
            filter: Filter::new(),
        }
    }
}

#[derive(Debug)]
pub struct Filter {
    pub features: HashMap<u32, bool>,
    pub parameters: HashMap<u32, (bool, u32)>,
}

impl Filter {
    pub fn new() -> Self {
        Self {
            features: HashMap::new(),
            parameters: HashMap::new(),
        }
    }

    pub fn from_attributes(attributes: Vec<Attribute>) -> Self {
        let mut filter = Self::new();

        for attribute in attributes {
            filter.add_attribute(attribute);
        }

        return filter;
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
