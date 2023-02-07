use codespan_reporting::diagnostic::Label;

use crate::diagnostic::{Diagnostic, Loc};

pub enum VmError {
    /// Yield control back to host
    Yield(Option<usize>),
    /// E3001 Binary Operator can not be applied
    OpBinNotApplicable(Loc, &'static str, String, String),
    /// E3002 Prefix Operator can not be applied
    OpPrefixNotApplicable(Loc, &'static str, String),
    /// E3003 Invalid Condition
    InvalidCondition(Loc, String),
    /// E3004 Type is not callable
    NotCallable(Loc, String),
    /// E3005 Function call with wrong parameters length
    ParameterLengthNotMatch {
        loc: Loc,
        expected: usize,
        got: usize,
    },
    /// E3006 Panic
    Panic {
        loc: Loc,
        reason: String,
        notes: Vec<String>,
    },
    /// E3007 Invalid Ref from external function
    InvalidRef {
        loc: Loc,
        t: &'static str,
        id: usize,
    },
    /// E3008 Io Error
    IoError {
        loc: Option<Loc>,
        error: std::io::Error,
    },
}

impl From<VmError> for Diagnostic {
    fn from(value: VmError) -> Self {
        match value {
            VmError::Yield(_) => unreachable!(),
            VmError::OpBinNotApplicable(loc, op, t1, t2) => Diagnostic::error()
                .with_code("E3001")
                .with_message(format!(
                    "`{op}` can not be applied between `{t1}` and `{t2}`"
                ))
                .with_labels(vec![Label::primary((), loc)]),
            VmError::OpPrefixNotApplicable(loc, op, t) => Diagnostic::error()
                .with_code("E3002")
                .with_message(format!("`{op}` can not be applied to `{t}`"))
                .with_labels(vec![Label::primary((), loc)]),
            VmError::InvalidCondition(loc, t) => Diagnostic::error()
                .with_code("E3003")
                .with_message(format!("Expect a bool value as condition, got a `{t}`"))
                .with_labels(vec![Label::primary((), loc)]),
            VmError::NotCallable(loc, t) => Diagnostic::error()
                .with_code("E3004")
                .with_message(format!("Type `{t}` is not callable"))
                .with_labels(vec![Label::primary((), loc)]),
            VmError::ParameterLengthNotMatch { loc, expected, got } => Diagnostic::error()
                .with_code("E3005")
                .with_message(format!(
                    "Function takes {expected} parameters but {got} is provided"
                ))
                .with_labels(vec![Label::primary((), loc)]),
            VmError::Panic { loc, reason, notes } => Diagnostic::error()
                .with_code("E3006")
                .with_message("Main function panic durning execution")
                .with_labels(vec![Label::primary((), loc).with_message(reason)])
                .with_notes(notes),
            VmError::InvalidRef { loc, t, id } => Diagnostic::error()
                .with_code("E3007")
                .with_message(format!(
                    "External function returns an invalid reference to {t}@{id}"
                ))
                .with_labels(vec![Label::primary((), loc)]),
            VmError::IoError { loc, error } => {
                let mut error = Diagnostic::error()
                    .with_code("E3008")
                    .with_message(format!("Io Error: {error}"));
                if let Some(loc) = loc {
                    error = error.with_labels(vec![Label::primary((), loc)]);
                }
                error
            }
        }
    }
}
