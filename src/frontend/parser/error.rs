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
    /// E1003 Missing expression in parentheses
    MissingExpr(Loc),
}

pub fn to_diagnostic(error: ErrorCode, loc: Loc, file_id: usize) -> Diagnostic {
    match error {
        ErrorCode::NoSuchFile(f, err) => Diagnostic::error()
            .with_code("E1000")
            .with_message(format!("Can not read file `{}`: `{}`", f, err))
            .with_labels(vec![Label::primary(file_id, loc)]),
        ErrorCode::UnexpectedToken(met, expected, to_match) => {
            let mut diagnostic = Diagnostic::error().with_code("E1001");
            if let Some(t) = met {
                diagnostic = diagnostic
                    .with_message(format!("Unexpected token `{}`", t))
                    .with_labels(vec![Label::primary(file_id, loc)]);
            } else {
                diagnostic = diagnostic
                    .with_message("End of file while parsing")
                    .with_labels(vec![Label::primary(file_id, loc)]);
            }
            if let Some(t) = expected {
                diagnostic = diagnostic.with_notes(vec![format!("Consider add a `{}` here", t)]);
            }
            if let Some((t, loc)) = to_match {
                diagnostic = diagnostic
                    .with_labels(vec![Label::secondary(file_id, loc)
                        .with_message(format!("to match `{}` here", t))]);
            }
            diagnostic
        }
        ErrorCode::UnexpectedEof => Diagnostic::error()
            .with_code("E1002")
            .with_message("Unexpected end of file here")
            .with_labels(vec![Label::primary(file_id, loc)]),
        ErrorCode::MissingExpr(loc_pre) => Diagnostic::error()
            .with_code("E1003")
            .with_message("Missing expression here")
            .with_labels(vec![
                Label::primary(file_id, loc),
                Label::secondary(file_id, loc_pre).with_message("Previous token here"),
            ]),
    }
}
