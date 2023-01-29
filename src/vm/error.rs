use codespan_reporting::diagnostic::Label;

use crate::diagnostic::{Diagnostic, Loc};

pub enum VmError {
    /// E3001 Type Not Addable
    NotAddable(Loc, String, String),
    InvalidFunc(Loc, usize),
    NotCallable(Loc, String),
    ParameterLengthNotMatch(Loc, usize, usize),
    Panic(Loc, String),
}

impl From<VmError> for Diagnostic {
    fn from(value: VmError) -> Self {
        match value {
            VmError::NotAddable(loc, t1, t2) => Diagnostic::error()
                .with_code("E3001")
                .with_message(format!("`+` can not be applied between `{t1}` and `{t2}`"))
                .with_labels(vec![Label::primary((), loc)]),
            VmError::InvalidFunc(_, _) => todo!(),
            VmError::NotCallable(_, _) => todo!(),
            VmError::ParameterLengthNotMatch(_, _, _) => todo!(),
            VmError::Panic(_, _) => todo!(),
        }
    }
}
