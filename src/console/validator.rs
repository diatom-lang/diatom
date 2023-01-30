use reedline::{ValidationResult, Validator};

use crate::Interpreter;

#[derive(Default)]
pub struct DiatomValidator;

impl Validator for DiatomValidator {
    fn validate(&self, line: &str) -> ValidationResult {
        if Interpreter::new().verify_input_completeness(line) {
            ValidationResult::Complete
        } else {
            ValidationResult::Incomplete
        }
    }
}

#[test]
fn test_validator() {
    use std::mem::discriminant;
    let val = DiatomValidator::default();
    assert_eq!(
        discriminant(&val.validate("begin a,b [1,3]")),
        discriminant(&ValidationResult::Incomplete)
    );
    assert_eq!(
        discriminant(&val.validate("if a,b then [1,3]")),
        discriminant(&ValidationResult::Incomplete)
    );
    assert_eq!(
        discriminant(&val.validate("if then [1,3]")),
        discriminant(&ValidationResult::Complete)
    );
}
