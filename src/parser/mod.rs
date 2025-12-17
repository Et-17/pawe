mod errors;

use crate::error_handling::FilePosition;
use crate::phonemes::SelectorCode;

struct Identifier {
    text: String,
    pos: FilePosition,
}

struct Diacritic {
    char: char,
    pos: FilePosition,
}

enum AttributeKind {
    Feature(bool, String),
    Parameter(bool, String, String),
    Character(String),
    SelectorCode(SelectorCode),
}

struct Attribute {
    kind: AttributeKind,
    pos: FilePosition,
}

struct Phoneme {
    attributes: Vec<Attribute>,
    pos: FilePosition,
}

struct Filter {
    attributes: Vec<Attribute>,
    pos: FilePosition,
}

struct Selector {
    attributes: Vec<Attribute>,
    code: SelectorCode,
    pos: FilePosition,
}

enum InputAtom {
    Phoneme(Phoneme),
    Filter(Filter),
    Selector(Selector),
}

enum EnvironmentAtom {
    Phoneme(Phoneme),
    Filter(Filter),
    Optional(Box<EnvironmentAtom>),
    ZeroOrMore(Box<EnvironmentAtom>),
    Not(Box<EnvironmentAtom>),
}

struct Environment {
    start: bool,
    end: bool,
    pre: Vec<EnvironmentAtom>,
    post: Vec<EnvironmentAtom>,
}

struct Rule {
    inputs: Vec<InputAtom>,
    outputs: Vec<Phoneme>,
    env: Environment,
}

enum DefinitionBlock {
    Languages(Vec<Identifier>),
    Parameters(Vec<(Identifier, Vec<Identifier>)>),
    Features(Vec<Identifier>),
    Characters(Vec<(Identifier, Phoneme)>),
    Diacritics(Vec<(Diacritic, Attribute)>),
    Evolve(Identifier, Identifier, Vec<Rule>),
}

struct Configuration {
    definitions: Vec<DefinitionBlock>,
}
