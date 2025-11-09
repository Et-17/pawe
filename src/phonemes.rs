use std::collections::HashMap;
use std::fmt::Debug;

use itertools::Itertools;

use crate::config::{Character, Label};

pub type SelectorCode = u8;

#[derive(Clone)]
pub enum Attribute {
    Feature(bool, Label),
    Parameter(bool, Label, Label),
    Character(Character),
    Selection(SelectorCode),
    BoundPhoneme(Phoneme),
}

impl Attribute {
    // If an attribute is already something concrete like a parameter, it will
    // just return it as is, but if it's a selection, then it will attempt to
    // get the target from the selection table. If the selection is invalid, it
    // will just not bind
    fn bind<'a>(self, selection_table: &HashMap<SelectorCode, &'a Phoneme>) -> Self {
        if let Self::Selection(code) = self {
            selection_table
                .get(&code)
                .map(|&selected_phoneme| Attribute::BoundPhoneme(selected_phoneme.clone()))
                .unwrap_or(self)
        } else {
            self
        }
    }
}

impl Debug for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Attribute::Feature(mark, label) => write!(f, "{}{}", mark_char(*mark), label),
            Attribute::Parameter(mark, param, var) => {
                write!(f, "{}{}.{}", mark_char(*mark), param, var)
            }
            Attribute::Character(definition) => write!(f, "{}", definition.symbol),
            Attribute::Selection(code) => write!(f, "{}", code),
            Attribute::BoundPhoneme(phoneme) => write!(f, "{:?}", phoneme),
        }
    }
}

#[derive(Clone)]
pub struct Phoneme {
    pub features: HashMap<Label, bool>,
    pub parameters: HashMap<Label, Label>,
    pub base: Option<Character>,
}

impl Phoneme {
    pub fn new(base: Option<Character>) -> Self {
        Self {
            features: HashMap::new(),
            parameters: HashMap::new(),
            base,
        }
    }

    pub fn add_character(&mut self, character: Character) -> () {
        // We have to clone before inserting the new character because if we
        // add the phoneme before inserting the character, it will take whatever
        // the new character's base is as this phonemes base, and won't take
        // the character as the new base.
        let phoneme = character.phoneme.clone();
        self.base.get_or_insert(character);
        self.add_phoneme(phoneme);
    }

    // Adds all the attributes from the given phoneme to this phoneme, so that
    // you can write defined characters in phoneme blocks to modify them
    pub fn add_phoneme(&mut self, phoneme: Phoneme) -> () {
        self.features.extend(phoneme.features);
        self.parameters.extend(phoneme.parameters);
        if let Some(new_base) = phoneme.base {
            self.base.get_or_insert(new_base);
        }
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
                self.add_character(definition);
            }
            Attribute::Selection(_) => (),
            Attribute::BoundPhoneme(phoneme) => {
                self.add_phoneme(phoneme);
            }
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

    // Converts this phoneme to a set of Attributes assuming that there is a
    // base phoneme. If there is no base, it simple returns an empty Vec
    fn attr_display(&self) -> Vec<Attribute> {
        let mut attrs = Vec::new();
        if let Some(ref base_value) = self.base {
            attrs.push(Attribute::Character(base_value.clone()));
        }

        for (param, self_value) in self.parameters.iter() {
            if let Some(ref base) = self.base {
                if let Some(base_value) = base.phoneme.parameters.get(&param) {
                    if *self_value == *base_value {
                        continue;
                    }
                }
            }

            attrs.push(Attribute::Parameter(
                true,
                param.clone(),
                self_value.clone(),
            ));
        }

        for (feat, self_value) in self.features.iter() {
            if let Some(ref base) = self.base {
                if let Some(base_value) = base.phoneme.features.get(&feat) {
                    if *self_value == *base_value {
                        continue;
                    }
                }
            }

            attrs.push(Attribute::Feature(self_value.clone(), feat.clone()));
        }

        attrs
    }

    // Goes through the available characters and tries to find a base that would
    // require less modification than the current base.
    pub fn rebase(self, characters: &HashMap<String, Character>) -> Self {
        if let Some(new_base) = self.best_base(characters) {
            Self {
                base: Some(new_base.clone()),
                ..self
            }
        } else {
            self
        }
    }

    // Finds the best base for the phoneme
    fn best_base<'a>(&self, characters: &'a HashMap<String, Character>) -> Option<&'a Character> {
        characters
            .values()
            .map(|c| (c, self.count_necessary_modifications(&c.phoneme)))
            .take_while_inclusive(|(_, mods)| *mods != 0)
            .min_by_key(|(_, mods)| *mods)
            .map(|(c, _)| c)
    }

    // Counts the necessary modifications in order to represent this phoneme in
    // terms of the given base.
    fn count_necessary_modifications(&self, base: &Phoneme) -> usize {
        // self.parameters.iter().filter(|(param, self_val)| base.parameters.get())
        let phoneme_mods = self
            .parameters
            .iter()
            .filter(|(param, self_val)| {
                base.parameters
                    .get(param)
                    .is_none_or(|base_val| **self_val != *base_val)
            })
            .count();

        let features_mods = self
            .features
            .iter()
            .filter(|(feat, self_val)| {
                base.features
                    .get(feat)
                    .is_none_or(|base_val| **self_val != *base_val)
            })
            .count();

        phoneme_mods + features_mods
    }
}

impl FromIterator<Attribute> for Phoneme {
    fn from_iter<T: IntoIterator<Item = Attribute>>(iter: T) -> Self {
        let mut phoneme = Self::new(None);

        for attribute in iter {
            phoneme.add_attribute(attribute);
        }

        phoneme
    }
}

impl Debug for Phoneme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let attrs = self.attr_display();
        if attrs.len() == 0 {
            return write!(f, "EMPTY=PHONEME");
        }
        if attrs.len() == 1 {
            if let Attribute::Character(character) = &attrs[0] {
                return write!(f, "{:?}", character);
            }
        }

        write!(f, "[")?;
        let mut attrs_iter = attrs.iter();
        attrs_iter
            .next()
            .map(|attr| write!(f, "{:?}", attr))
            .transpose()?;
        for attr in attrs_iter {
            write!(f, " {:?}", attr)?;
        }
        write!(f, "]")
    }
}

#[derive(Clone)]
pub struct UnboundPhoneme {
    pub attributes: Vec<Attribute>,
}

impl UnboundPhoneme {
    // Binds this into a normal Phoneme. Invalid selectors are simply ignored,
    // because Phoneme's constructor just ignores selector Attributes, and the
    // Attribute's bind function just doesn't bind if the selection is invalid.
    // Note that this creates a new Phoneme by cloning all the Attributes.
    pub fn bind<'a>(&self, selection_table: &HashMap<SelectorCode, &'a Phoneme>) -> Phoneme {
        self.attributes
            .iter()
            .cloned()
            .map(|attr| attr.bind(selection_table))
            .collect()
    }
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

        write!(f, "[{}]", attributes_strs)
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
