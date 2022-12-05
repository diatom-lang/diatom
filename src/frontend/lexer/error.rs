use codespan_reporting::diagnostic::Label;

use crate::diagnostic::{Diagnostic, Loc};

/// Error Code used by `Lexer`
#[cfg_attr(test, derive(Debug))]
pub enum ErrorCode {
    /// E0001 Invalid digit in number literal
    InvalidNum(String),
    /// E0002 Integer literal overflow 64-bits integer
    IntegerOverflow,
    /// E0003 Parse float error
    ParseFloatError(String),
    /// E0004 Invalid Escape Sequence
    InvalidEscapeSequence,
    /// E0005 Open quotation
    OpenQuote,
    /// E0006 Invalid operator
    InvalidOp(char),
}

pub fn to_diagnostic(error: ErrorCode, loc: Loc, file_id: usize) -> Diagnostic {
    match error {
        ErrorCode::InvalidNum(s) => Diagnostic::error()
            .with_code("E0001")
            .with_message(format!("Invalid number literal `{s}`"))
            .with_labels(vec![Label::primary(file_id, loc)]),
        ErrorCode::IntegerOverflow => Diagnostic::error()
            .with_code("E0002")
            .with_message("Integer literal overflow 64-bits integer".to_string())
            .with_labels(vec![Label::primary(file_id, loc)]),
        ErrorCode::ParseFloatError(s) => Diagnostic::error()
            .with_code("E0003")
            .with_message(s)
            .with_labels(vec![Label::primary(file_id, loc)]),
        ErrorCode::InvalidEscapeSequence => Diagnostic::error()
            .with_code("E0004")
            .with_message("Invalid escape sequence in string literal")
            .with_labels(vec![Label::primary(file_id, loc)]),
        ErrorCode::OpenQuote => Diagnostic::error()
            .with_code("E0005")
            .with_message("String literal is not terminated")
            .with_labels(vec![Label::primary(file_id, loc)]),
        ErrorCode::InvalidOp(c) => Diagnostic::error()
            .with_code("E0006")
            .with_message(format!("Invalid operator `{c}`"))
            .with_labels(vec![Label::primary(file_id, loc)]),
    }
}
