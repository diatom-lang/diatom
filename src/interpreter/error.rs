use codespan_reporting::diagnostic::Label;

use crate::diagnostic::{Diagnostic, Loc};

/// Error code for code generator
///
/// E2000 - E2999
pub enum ErrorCode {
    /// E2000 Can not assign to this expression
    CannotAssign(Loc),
}

impl From<ErrorCode> for Diagnostic {
    fn from(value: ErrorCode) -> Self {
        match value {
            ErrorCode::CannotAssign(loc) => Diagnostic::error()
                .with_code("E2000")
                .with_message("Can not assign to this expression")
                .with_labels(vec![Label::primary((), loc)]),
        }
    }
}
