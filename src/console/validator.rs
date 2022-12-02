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
