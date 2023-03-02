use codespan_reporting::diagnostic::Label;

use crate::file_manager::{Diagnostic, Loc};

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
    /// E3009 Can not set attr
    CanNotSetAttr { loc: Loc, t: String },
    /// E3010 No Such Key
    NoSuchKey { loc: Loc, attr: String },
    /// E3011 Not a table
    NotATable { loc: Loc, t: String },
    /// E3012 Not a tuple
    NotATuple { loc: Loc, t: String },
    /// E3013 Tuple access out of bound
    TupleOutOfBound {
        loc: Loc,
        bound: usize,
        access: usize,
    },
    /// E3014 Invalid meta table
    InvalidMetaTable { loc: Loc, t: String },
    /// E3015 Index out of bound
    IndexOutOfBound { loc: Loc, bound: usize, index: i64 },
    /// E3016 Can Not Index
    CanNotIndex { loc: Loc, t1: String, t2: String },
    /// E3017 Missing extern variable
    MissingExtern { loc: Loc, name: String },
    /// E3018 Module not return table
    ModuleInvalidReturn { loc: Loc, t: String },
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
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::OpPrefixNotApplicable(loc, op, t) => Diagnostic::error()
                .with_code("E3002")
                .with_message(format!("`{op}` can not be applied to `{t}`"))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::InvalidCondition(loc, t) => Diagnostic::error()
                .with_code("E3003")
                .with_message(format!("Expect a bool value as condition, got a `{t}`"))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::NotCallable(loc, t) => Diagnostic::error()
                .with_code("E3004")
                .with_message(format!("Type `{t}` is not callable"))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::ParameterLengthNotMatch { loc, expected, got } => Diagnostic::error()
                .with_code("E3005")
                .with_message(format!(
                    "Function takes {expected} parameters but {got} is provided"
                ))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::Panic { loc, reason, notes } => Diagnostic::error()
                .with_code("E3006")
                .with_message("Main function panic durning execution")
                .with_labels(vec![Label::primary(loc.fid, loc).with_message(reason)])
                .with_notes(notes),
            VmError::InvalidRef { loc, t, id } => Diagnostic::error()
                .with_code("E3007")
                .with_message(format!(
                    "External function returns an invalid reference to {t}@{id}"
                ))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::IoError { loc, error } => {
                let mut error = Diagnostic::error()
                    .with_code("E3008")
                    .with_message(format!("Io Error: {error}"));
                if let Some(loc) = loc {
                    error = error.with_labels(vec![Label::primary(loc.fid, loc)]);
                }
                error
            }
            VmError::CanNotSetAttr { loc, t } => Diagnostic::error()
                .with_code("E3009")
                .with_message(format!("Can not set type `{t}`'s attribute'"))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::NoSuchKey { loc, attr } => Diagnostic::error()
                .with_code("E3010")
                .with_message(format!(
                    "Table or its meta table does not contain key `{attr}`"
                ))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::NotATable { loc, t } => Diagnostic::error()
                .with_code("E3011")
                .with_message(format!(
                    "Read attribute from type `{t}` which is not a table"
                ))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::NotATuple { loc, t } => Diagnostic::error()
                .with_code("E3012")
                .with_message(format!("Read content from type `{t}` which is not a tuple"))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::TupleOutOfBound { loc, bound, access } => Diagnostic::error()
                .with_code("E3013")
                .with_message(format!(
                    "Tuple has {bound} item(s) while attempting to get an item at {access}"
                ))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::InvalidMetaTable { loc, t } => Diagnostic::error()
                .with_code("E3014")
                .with_message(format!("Attempt to use type `{t}` as meta table"))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::IndexOutOfBound { loc, bound, index } => Diagnostic::error()
                .with_code("E3015")
                .with_message(format!(
                    "List has {bound} item(s) while attempting to get an item at {index}"
                ))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::CanNotIndex { loc, t1, t2 } => Diagnostic::error()
                .with_code("E3016")
                .with_message(format!("Type `{t1}` can not be indexed by type `{t2}`"))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::MissingExtern { loc, name } => Diagnostic::error()
                .with_code("E3017")
                .with_message(format!("External variable `{name}` is not loaded by host"))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
            VmError::ModuleInvalidReturn { loc, t } => Diagnostic::error()
                .with_code("E3018")
                .with_message(format!("Module returns type `{t}` which is not a table"))
                .with_labels(vec![Label::primary(loc.fid, loc)]),
        }
    }
}
