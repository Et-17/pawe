use std::fmt::Display;

use crate::error_handling::{Error, ErrorType, Location};

mod error_handling;

#[derive(Debug)]
pub enum TestErrorType {
    TestError,
}

impl ErrorType for TestErrorType {}

impl PartialEq for TestErrorType {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Display for TestErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TestErrorType::TestError => write!(f, "This is a test error"),
        }
    }
}

fn main() {
    let test_error = Error {
        location: Location { line: 5 },
        error: TestErrorType::TestError,
    };
    println!("Hello, world!");
    eprintln!("{}", test_error);
}
