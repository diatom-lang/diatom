use std::{
    io::Stdout,
    sync::{Arc, Mutex},
};

use reedline::{ValidationResult, Validator};

use crate::Interpreter;

pub struct DiatomValidator {
    pub interpreter: Arc<Mutex<Interpreter<Stdout>>>,
}

impl Validator for DiatomValidator {
    fn validate(&self, line: &str) -> ValidationResult {
        if self
            .interpreter
            .lock()
            .unwrap()
            .verify_input_completeness(line)
        {
            ValidationResult::Complete
        } else {
            ValidationResult::Incomplete
        }
    }
}
