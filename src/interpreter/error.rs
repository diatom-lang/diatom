use codespan_reporting::diagnostic::Label;

use crate::diagnostic::{Diagnostic, Loc};

/// Error code for code generator
///
/// E2000 - E2999
pub enum ErrorCode {
    /// E2000 Can not assign to this expression
    CannotAssign(Loc),
    /// E2001 Name not defined
    NameNotDefined(Loc, String),
    /// E2002 Assignment Not Allowed here
    InvalidAssignment(Loc),
    /// E2003 Parameter Shadowing
    ParameterShadowing {
        // if previously defined by external function
        // it does not have a location
        previous: Option<Loc>,
        parameter: Loc,
        name: String,
    },
}

impl From<ErrorCode> for Diagnostic {
    fn from(value: ErrorCode) -> Self {
        match value {
            ErrorCode::CannotAssign(loc) => Diagnostic::error()
                .with_code("E2000")
                .with_message("Can not assign to this expression")
                .with_labels(vec![Label::primary((), loc)]),
            ErrorCode::NameNotDefined(loc, name) => Diagnostic::error()
                .with_code("E2001")
                .with_message(format!("Name `{name}` is not defined"))
                .with_labels(vec![Label::primary((), loc)]),
            ErrorCode::InvalidAssignment(loc) => Diagnostic::error()
                .with_code("E2002")
                .with_message("Assignment can not be used as expression")
                .with_labels(vec![Label::primary((), loc)]),
            ErrorCode::ParameterShadowing {
                previous,
                parameter,
                name,
            } => {
                let mut error = Diagnostic::error().with_code("E2003").with_message(format!(
                    "Function parameter `{name}` has a name that is already defined"
                ));
                let mut labels = vec![Label::primary((), parameter)];
                if let Some(previous) = previous {
                    labels.push(
                        Label::secondary((), previous).with_message("Name previously defined here"),
                    )
                } else {
                    error = error.with_notes(vec![format!(
                        "`{name}` is an external function defined by host"
                    )])
                };
                error.with_labels(labels)
            }
        }
    }
}
