use std::ffi::OsStr;

use reedline::{ValidationResult, Validator};

use crate::Parser;

#[derive(Default)]
pub struct DiatomValidator;

impl Validator for DiatomValidator {
    fn validate(&self, line: &str) -> ValidationResult {
        let mut parser = Parser::new();
        parser.parse_str(OsStr::new(""), line);
        if parser.input_can_continue() {
            ValidationResult::Incomplete
        } else {
            ValidationResult::Complete
        }
    }
}
