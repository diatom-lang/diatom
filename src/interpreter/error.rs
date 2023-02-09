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
        is_extern: bool,
        parameter: Loc,
        name: String,
    },
    /// E2004 Break outside loop
    BreakOutsideLoop(Loc),
    /// E2005 Continue outside loop
    ContinueOutsideLoop(Loc),
    /// E2006 Return outside function
    ReturnOutsideFunction(Loc),
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
                .with_message(format!("Name `{name}` is not defined in current scope"))
                .with_labels(vec![Label::primary((), loc)]),
            ErrorCode::InvalidAssignment(loc) => Diagnostic::error()
                .with_code("E2002")
                .with_message("Assignment can not be used as expression")
                .with_labels(vec![Label::primary((), loc)]),
            ErrorCode::ParameterShadowing {
                previous,
                is_extern,
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
                } else if is_extern {
                    error = error.with_notes(vec![format!(
                        "`{name}` is an external function defined by host"
                    )])
                };
                error.with_labels(labels)
            }
            ErrorCode::BreakOutsideLoop(loc) => Diagnostic::error()
                .with_code("E2004")
                .with_message("Can not break outside a loop")
                .with_labels(vec![Label::primary((), loc)]),
            ErrorCode::ContinueOutsideLoop(loc) => Diagnostic::error()
                .with_code("E2005")
                .with_message("Can not continue outside a loop")
                .with_labels(vec![Label::primary((), loc)]),
            ErrorCode::ReturnOutsideFunction(loc) => Diagnostic::error()
                .with_code("E2006")
                .with_message("Can not return outside a function")
                .with_labels(vec![Label::primary((), loc)]),
        }
    }
}
