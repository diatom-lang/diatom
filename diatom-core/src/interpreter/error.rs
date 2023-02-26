use codespan_reporting::diagnostic::Label;

use crate::file_manager::{Diagnostic, Loc};

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
    /// E2003 Break outside loop
    BreakOutsideLoop(Loc),
    /// E2004 Continue outside loop
    ContinueOutsideLoop(Loc),
    /// E2005 Return outside function
    ReturnOutsideFunction(Loc),
    /// E2006 Invalid Member Access
    InvalidMember(Loc),
    /// E2007 Set Meta Not Allowed
    MetaNotAllowed(Loc),
}

impl From<ErrorCode> for Diagnostic {
    fn from(value: ErrorCode) -> Self {
        match value {
            ErrorCode::CannotAssign(loc) => Diagnostic::error()
                .with_code("E2000")
                .with_message("Can not assign to this expression")
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            ErrorCode::NameNotDefined(loc, name) => Diagnostic::error()
                .with_code("E2001")
                .with_message(format!("Name `{name}` is not defined in current scope"))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            ErrorCode::InvalidAssignment(loc) => Diagnostic::error()
                .with_code("E2002")
                .with_message("Assignment can not be used as expression")
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            ErrorCode::BreakOutsideLoop(loc) => Diagnostic::error()
                .with_code("E2003")
                .with_message("Can not break outside a loop")
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            ErrorCode::ContinueOutsideLoop(loc) => Diagnostic::error()
                .with_code("E2004")
                .with_message("Can not continue outside a loop")
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            ErrorCode::ReturnOutsideFunction(loc) => Diagnostic::error()
                .with_code("E2005")
                .with_message("Can not return outside a function")
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            ErrorCode::InvalidMember(loc) => Diagnostic::error()
                .with_code("E2006")
                .with_message("Member assessment must be an identifier")
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            ErrorCode::MetaNotAllowed(loc) => Diagnostic::error()
                .with_code("E2007")
                .with_message("Set meta table is not allowed here")
                .with_labels(vec![Label::primary(loc.fid, loc)])
                .with_notes(vec![
                    "Meta table can only be set on newly created table".to_string(),
                    "For example: `table = {...} <- Meta`".to_string(),
                ]),
        }
    }
}
