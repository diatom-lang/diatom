use codespan_reporting::diagnostic::Label;

use crate::{
    diagnostic::{Diagnostic, DisplayableOsString, Loc},
    frontend::Token,
};

/// Error code for `Parser`
///
/// Error code `E1000` to `E1999` is reserved.
pub enum ErrorCode {
    /// E1000 Can not open file
    NoSuchFile(DisplayableOsString, std::io::Error),
    /// E1001 Unexpected token
    ///
    /// Parameters:
    /// - 1 Token met (None if eof is met)
    /// - 2 Token expected here (None if not expected a specific one)
    /// - 3 Previous token to match the expected token (None if there is not any)
    UnexpectedToken(Option<Token>, Option<Token>, Option<(Token, Loc)>),
    /// E1002 Unexpected end of file
    UnexpectedEof,
    /// E1003 Missing expression
    MissingExpr(Loc),
    /// E1004 Missing statement
    MissingStmt(Loc),
    /// E1005 Missing map value
    MissingMapValue,
    /// E1006 Missing data constructor
    MissingConstructor,
    /// E1007 Require Wrong Argument
    RequireWrongArgument,
    /// E1008 Invalid module string
    InvalidModuleString,
    /// E1009 Module Not Found
    ModuleNotFound(String),
    /// E1010 Invalid Module, Can Not Parse
    InvalidModule,
}

pub fn to_diagnostic(error: ErrorCode, loc: Loc) -> (Diagnostic, bool) {
    let eof = matches!(
        error,
        ErrorCode::UnexpectedEof | ErrorCode::UnexpectedToken(None, _, _)
    );
    let diag = match error {
        ErrorCode::NoSuchFile(f, err) => Diagnostic::error()
            .with_code("E1000")
            .with_message(format!("Can not read file `{}`: `{}`", f, err)),
        ErrorCode::UnexpectedToken(met, expected, to_match) => {
            let mut diagnostic = Diagnostic::error().with_code("E1001");
            if let Some(t) = met {
                diagnostic = diagnostic
                    .with_message(format!("Unexpected token `{}`", t))
                    .with_labels(vec![Label::primary((), loc)]);
            } else {
                diagnostic = diagnostic
                    .with_message("End of file while parsing")
                    .with_labels(vec![Label::primary((), loc)]);
            }
            if let Some(t) = expected {
                diagnostic = diagnostic.with_notes(vec![format!("Consider add a `{}` here", t)]);
            }
            if let Some((t, loc)) = to_match {
                diagnostic = diagnostic.with_labels(vec![
                    Label::secondary((), loc).with_message(format!("Due to `{}` here", t))
                ]);
            }
            diagnostic
        }
        ErrorCode::UnexpectedEof => Diagnostic::error()
            .with_code("E1002")
            .with_message("Unexpected end of file here")
            .with_labels(vec![Label::primary((), loc)]),
        ErrorCode::MissingExpr(loc_pre) => Diagnostic::error()
            .with_code("E1003")
            .with_message("Missing expression here")
            .with_labels(vec![
                Label::primary((), loc),
                Label::secondary((), loc_pre).with_message("Previous token here"),
            ]),
        ErrorCode::MissingStmt(loc_pre) => Diagnostic::error()
            .with_code("E1004")
            .with_message("Missing statement here")
            .with_labels(vec![
                Label::primary((), loc),
                Label::secondary((), loc_pre).with_message("Previous token here"),
            ]),
        ErrorCode::MissingMapValue => Diagnostic::error()
            .with_code("E1005")
            .with_message("Missing map value here")
            .with_labels(vec![Label::primary((), loc)])
            .with_notes(vec!["Consider add `: <some value>` here".to_string()]),
        ErrorCode::MissingConstructor => Diagnostic::error()
            .with_code("E1006")
            .with_message("Missing data type constructor here")
            .with_labels(vec![Label::primary((), loc)]),
        ErrorCode::RequireWrongArgument => Diagnostic::error()
            .with_code("E1007")
            .with_message("Module must be a string literal")
            .with_labels(vec![Label::primary((), loc)]),
        ErrorCode::InvalidModuleString => Diagnostic::error()
            .with_code("E1008")
            .with_message("Invalid character in module string")
            .with_labels(vec![Label::primary((), loc)])
            .with_notes(vec![
                "Module string can only contains '.', '-' and [0-9a-zA-Z_].".to_string(),
                "Module String must not be empty.".to_string(),
            ]),
        ErrorCode::ModuleNotFound(s) => Diagnostic::error()
            .with_code("E1009")
            .with_message("Module can not be found")
            .with_labels(vec![Label::primary((), loc)])
            .with_notes(vec![format!("Looking for `{}.dm` or `{}/mod.dm`", s, s)]),
        ErrorCode::InvalidModule => Diagnostic::error()
            .with_code("E1010")
            .with_message("Error encountered while parsing module")
            .with_labels(vec![Label::primary((), loc)]),
    };
    (diag, eof)
}
