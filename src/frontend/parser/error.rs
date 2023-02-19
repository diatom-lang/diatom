use crate::{file_manager::Loc, frontend::Token};

/// Error code for `Parser`
///
/// Error code `E1000` to `E1999` is reserved.
pub enum ErrorCode {
    /// E1000 Unexpected token
    ///
    /// Parameters:
    /// - 1 Token met (None if eof is met)
    /// - 2 Token expected here (None if not expected a specific one)
    /// - 3 Previous token to match the expected token (None if there is not any)
    UnexpectedToken(Option<Token>, Option<Token>, Option<(Token, Loc)>),
    /// E1001 Unexpected end of file
    UnexpectedEof,
    /// E1002 Missing expression
    MissingExpr(Loc),
    /// E1003 Invalid Table Key
    InvalidTableKey,
    /// E1004 Require Wrong Argument
    _RequireWrongArgument,
    /// E1005 Invalid module string
    _InvalidModuleString,
    /// E1006 Module Not Found
    _ModuleNotFound(String),
    /// E1007 Invalid Module, Can Not Parse
    _InvalidModule,
    /// E1008 Invalid Table Format
    InvalidTableFormat,
    /// E1009 Duplicate Key
    DuplicateKey(Loc, String),
}
