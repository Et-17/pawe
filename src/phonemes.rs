use std::collections::HashMap;
use std::fmt::{Debug, Display};

use itertools::Itertools;

use crate::cli::NO_BASE;
use crate::config::{Character, CharacterDefinition, DiacriticMap, Label};

pub type SelectorCode = u8;

#[derive(Clone, Debug)]
pub enum Attribute {
    Feature(bool, Label),
    Parameter(bool, Label, Label),
    Character(Character),
    Selection(SelectorCode),
    Phoneme(Phoneme),
    Display(String), // just a pass through for displaying atoms
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
                .map(|&selected_phoneme| Attribute::Phoneme(selected_phoneme.clone()))
                .unwrap_or(self)
        } else {
            self
        }
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Attribute::Feature(mark, feat) => write!(f, "{}{}", mark_char(*mark), feat),
            Attribute::Parameter(mark, param, var) => {
                write!(f, "{}{}.{}", mark_char(*mark), param, var)
            }
            Attribute::Character(definition) => write!(f, "{}", definition.symbol),
            Attribute::Selection(code) => write!(f, "{}", code),
            Attribute::Phoneme(phoneme) => write!(f, "{}", phoneme),
            Attribute::Display(display) => write!(f, "{}", display),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Phoneme {
    pub features: HashMap<Label, bool>,
    pub parameters: HashMap<Label, Label>,
    pub base: Option<Box<CharacterDefinition>>,
}

impl Phoneme {
    pub fn new(base: Option<CharacterDefinition>) -> Self {
        Self {
            features: HashMap::new(),
            parameters: HashMap::new(),
            base: base.map(Box::new),
        }
    }

    pub fn add_character(&mut self, character: Character) -> () {
        if self.base.is_none() {
            self.set_base(&character);
        }
        self.add_phoneme(character.phoneme.clone());
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
                self.add_character(definition);
            }
            Attribute::Phoneme(phoneme) => {
                self.add_phoneme(phoneme);
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

    fn parameters_matches(pattern: &HashMap<Label, Label>, other: &HashMap<Label, Label>) -> bool {
        pattern
            .iter()
            .all(|(param, var)| other.get(param).is_some_and(|o_var| var == o_var))
    }

    // Converts this phoneme to a set of Attributes assuming that there is a
    // base phoneme. If there is no base, it simple returns an empty Vec
    fn attr_display(&self) -> Vec<Attribute> {
        let no_base = unsafe { NO_BASE };

        let mut attrs = Vec::new();
        if !no_base && let Some(ref base_value) = self.base {
            attrs.push(Attribute::Display(base_value.symbol.clone()));
        }

        for (param, self_value) in self.parameters.iter() {
            if !no_base && let Some(ref base) = self.base {
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
            if !no_base && let Some(ref base) = self.base {
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
    // require less modification than the current base. This will not act if
    // --no-base is set, because it won't have an effect anyway.
    pub fn rebase(
        mut self,
        characters: &HashMap<String, Character>,
        diacritics: &DiacriticMap,
    ) -> Self {
        if unsafe { NO_BASE } {
            return self;
        }

        if let Some(new_base) = self.best_base(characters, diacritics) {
            self.set_base(&new_base);
            let mods = self.necessary_modifications(&new_base.phoneme);
            self.integrate_modifications(mods, diacritics);
        }
        self
    }

    fn set_base(&mut self, base: &CharacterDefinition) {
        let mut new_base = base.clone();
        new_base.phoneme.base = None;
        self.base = Some(Box::new(new_base));
    }

    fn integrate_modifications(
        &mut self,
        mods: impl IntoIterator<Item = Attribute>,
        diacritics: &DiacriticMap,
    ) {
        let Some(ref mut base) = self.base else {
            return;
        };

        let new_dia_pairs = diacritics.find_possible_diacritics(mods.into_iter());

        for (diacritic, attribute) in new_dia_pairs {
            base.symbol.push(diacritic);
            base.phoneme.add_attribute(attribute);
        }
    }

    // Finds the best base for the phoneme
    fn best_base<'a>(
        &self,
        characters: &'a HashMap<String, Character>,
        diacritics: &DiacriticMap,
    ) -> Option<&'a Character> {
        characters
            .values()
            .map(|c| (c, self.mismatch_with(&c.phoneme, diacritics)))
            .min_by_key(|(_, mods)| *mods)
            .map(|(c, _)| c)
    }

    // Counts the necessary modifications in order to represent this phoneme in
    // terms of the given base, including possible diacritics
    fn mismatch_with(&self, base: &Phoneme, diacritics: &DiacriticMap) -> usize {
        self.necessary_modifications(base)
            .iter()
            .filter(|attr| !diacritics.has_attribute(attr))
            .count()
    }

    // Gets the necessary modifications in order to represent this phoneme in
    // terms of the given base.
    fn necessary_modifications(&self, base: &Phoneme) -> Vec<Attribute> {
        let feature_mods = divergences(&self.features, &base.features)
            .map(|(feat, mark)| Attribute::Feature(mark.clone(), feat.clone()));

        let parameter_mods = divergences(&self.parameters, &base.parameters)
            .map(|(param, variant)| Attribute::Parameter(true, param.clone(), variant.clone()));

        feature_mods.chain(parameter_mods).collect_vec()
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

impl Display for Phoneme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let attrs = self.attr_display();
        if attrs.len() == 0 {
            return write!(f, "EMPTY=PHONEME");
        }
        if attrs.len() == 1 {
            if let Attribute::Display(character) = &attrs[0] {
                return write!(f, "{}", character);
            }
        }

        write!(f, "[{}]", attrs.iter().join(" "))
    }
}

#[derive(Clone, Debug)]
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

impl Display for UnboundPhoneme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let attributes_strs = self
            .attributes
            .iter()
            .map(|attr| format!("{}", attr))
            .join(" ");

        write!(f, "[{}]", attributes_strs)
    }
}

#[derive(Debug)]
pub struct Selector {
    pub code: SelectorCode,
    pub filter: Filter,
}

impl Display for Selector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.code, self.filter)
    }
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

impl Display for Filter {
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

// Get the key/value pairs in HashMap `target` that either do not exist or
// aren't the same in HashMap `base`
fn divergences<'a, K: Eq + std::hash::Hash, V: PartialEq>(
    target: &'a HashMap<K, V>,
    base: &'a HashMap<K, V>,
) -> impl Iterator<Item = (&'a K, &'a V)> {
    target.iter().filter(|(key, target_val)| {
        base.get(key)
            .is_none_or(|base_val| *base_val != **target_val)
    })
}
