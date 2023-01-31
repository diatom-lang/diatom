use codespan_reporting::diagnostic::Label;

use crate::diagnostic::{Diagnostic, Loc};

pub enum VmError {
    /// Yield control back to host
    Yield,
    /// E3001 Binary Operator can not be applied
    OpBinNotApplicable(Loc, &'static str, String, String),
    /// E3002 Prefix Operator can not be applied
    OpPrefixNotApplicable(Loc, &'static str, String),
    /// E3003 Invalid Condition
    InvalidCondition(Loc, String),
    InvalidFunc(Loc, usize),
    NotCallable(Loc, String),
    ParameterLengthNotMatch(Loc, usize, usize),
    Panic(Loc, String),
}

impl From<VmError> for Diagnostic {
    fn from(value: VmError) -> Self {
        match value {
            VmError::Yield => unreachable!(),
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
            VmError::Panic(_, _) => todo!(),
            VmError::InvalidFunc(_, _) => todo!(),
            VmError::NotCallable(_, _) => todo!(),
            VmError::ParameterLengthNotMatch(_, _, _) => todo!(),
        }
    }
}
