use itertools::Itertools;

use crate::evolution::Rule;
use crate::phonemes::Phoneme;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

type LabelCode = u16;

pub struct Label {
    pub code: LabelCode,
    pub label: Rc<String>,
}

impl Label {
    pub fn new(code: LabelCode, label: String) -> Label {
        Label {
            code,
            label: Rc::new(label),
        }
    }
}

impl Clone for Label {
    fn clone(&self) -> Self {
        Label {
            code: self.code.clone(),
            label: Rc::clone(&self.label),
        }
    }
}

impl std::fmt::Debug for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.label)
    }
}

// This is so that we can use Label as the key in HashMaps. It only hashes the
// code for speed.
impl std::hash::Hash for Label {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.code.hash(state);
    }
}

impl PartialEq for Label {
    fn eq(&self, other: &Self) -> bool {
        LabelCode::eq(&self.code, &other.code)
    }
}

impl Eq for Label {}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.label)
    }
}

#[derive(Default)]
pub struct LabelEncoding {
    table: HashMap<String, Label>,
    next_code: LabelCode,
}

impl LabelEncoding {
    fn with_capacity(capacity: usize) -> Self {
        Self {
            table: HashMap::with_capacity(capacity),
            ..Default::default()
        }
    }

    pub fn add(&mut self, label: String) -> Label {
        self.table
            .entry(label)
            .or_insert_with_key(|key| {
                self.next_code += 1;
                Label::new(self.next_code - 1, key.clone())
            })
            .clone()
    }

    pub fn encode(&self, label: &String) -> Option<Label> {
        self.table.get(label).cloned()
    }
}

impl From<Vec<String>> for LabelEncoding {
    fn from(value: Vec<String>) -> Self {
        let mut encoding = Self::with_capacity(value.len());

        encoding.extend(value);

        return encoding;
    }
}

impl Extend<String> for LabelEncoding {
    fn extend<T: IntoIterator<Item = String>>(&mut self, iter: T) {
        let into_iter = iter.into_iter();
        let size = into_iter.size_hint().0;

        self.table.reserve(size);

        for element in into_iter {
            self.add(element);
        }
    }
}

impl Debug for LabelEncoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set()
            .entries(self.table.values().sorted_by_key(|x| x.code))
            .finish()
    }
}

#[derive(Default)]
pub struct ParameterEncoding {
    p_table: HashMap<String, Label>,
    v_table: HashMap<LabelCode, LabelEncoding>,
    next_code: LabelCode,
}

impl ParameterEncoding {
    // Returns Ok() with the new parameter label if the add was successful,
    // but returns Err() if the parameter already existed
    pub fn add(&mut self, name: String, variants: Vec<String>) -> Result<(), ()> {
        if self.p_table.get(&name).is_some() {
            return Err(());
        }

        self.p_table
            .entry(name)
            .or_insert_with_key(|key| Label::new(self.next_code, key.clone()));

        self.v_table
            .insert(self.next_code, LabelEncoding::from(variants));

        self.next_code += 1;
        Ok(())
    }

    // Returns Some(parameter, Some(variant)). If the parameter could not be
    // found, this will return None. If the variant could not be found, this
    // will return Some(parameter, None).
    pub fn encode(
        &mut self,
        parameter: &String,
        variant: &String,
    ) -> Option<(Label, Option<Label>)> {
        let p_label = self.p_table.get(parameter)?.clone();

        let v_label_opt = self
            .v_table
            .get(&p_label.code)
            .and_then(|v_le| v_le.encode(variant));

        Some((p_label, v_label_opt))
    }
}

impl Debug for ParameterEncoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let empty = LabelEncoding::default();

        f.debug_map()
            .entries(
                self.p_table
                    .values()
                    .sorted_by_key(|x| x.code)
                    .map(|l| (l, self.v_table.get(&l.code).unwrap_or(&empty))),
            )
            .finish()
    }
}

#[derive(Debug, Default)]
pub struct Config {
    pub languages: LabelEncoding,
    pub features: LabelEncoding,
    pub parameters: ParameterEncoding,
    pub characters: HashMap<String, Phoneme>,
    pub evolutions: HashMap<Label, HashMap<Label, Vec<Rule>>>,
}

impl Config {
    pub fn new() -> Self {
        Self::default()
    }
}
