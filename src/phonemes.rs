use std::collections::HashMap;

#[derive(Debug)]
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

    // Adds all the attributes from the given phoneme to this phoneme, so that
    // you can write defined characters in phoneme blocks to modify them
    pub fn add_phoneme(&mut self, phoneme: &Phoneme) -> () {
        self.features.extend(phoneme.features.iter());
        self.parameters.extend(phoneme.parameters.iter());
    }
}
