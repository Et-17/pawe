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

        let code = *self.encode.entry(label.clone()).or_insert(self.next_code);
        self.decode.entry(code).or_insert(label);

        self.next_code += 1;
        return code;
    }

    pub fn encode(&self, label: String) -> Option<&u32> {
        self.encode.get(&label)
    }

    pub fn decode(&self, code: u32) -> Option<&String> {
        self.decode.get(&code)
    }
}

#[derive(Debug)]
pub struct Config {
    pub languages: LabelEncoding,
    pub features: LabelEncoding,
    pub parameters: LabelEncoding,
    pub parameter_values: HashMap<u32, LabelEncoding>,
}

impl Config {
    pub fn new() -> Self {
        Self {
            languages: LabelEncoding::new(),
            features: LabelEncoding::new(),
            parameters: LabelEncoding::new(),
            parameter_values: HashMap::new(),
        }
    }
}
