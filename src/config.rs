use crate::phonemes::Phoneme;
use std::collections::HashMap;

#[derive(Debug)]
pub struct LabelEncoding {
    encode: HashMap<String, u32>,
    decode: HashMap<u32, String>,
    next_code: u32,
}

impl LabelEncoding {
    pub fn new() -> Self {
        Self {
            encode: HashMap::new(),
            decode: HashMap::new(),
            next_code: 0,
        }
    }

    pub fn add(&mut self, label: String) -> u32 {
        if let Some(preëxisting_code) = self.encode.get(&label) {
            // This label already exists
            return *preëxisting_code;
        }

        self.encode.insert(label.clone(), self.next_code);
        self.decode.insert(self.next_code, label);

        self.next_code += 1;
        return self.next_code - 1;
    }

    pub fn encode(&self, label: &String) -> Option<&u32> {
        self.encode.get(label)
    }

    pub fn decode(&self, code: &u32) -> Option<&String> {
        self.decode.get(code)
    }
}

#[derive(Debug)]
pub struct Config {
    pub languages: LabelEncoding,
    pub features: LabelEncoding,
    pub parameters: LabelEncoding,
    pub parameter_values: HashMap<u32, LabelEncoding>,
    pub characters: HashMap<String, Phoneme>,
}

impl Config {
    pub fn new() -> Self {
        Self {
            languages: LabelEncoding::new(),
            features: LabelEncoding::new(),
            parameters: LabelEncoding::new(),
            parameter_values: HashMap::new(),
            characters: HashMap::new(),
        }
    }
}
