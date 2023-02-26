use crate::{
    file_manager::Loc,
    frontend::parser::ast::{Const, Expr, OpInfix, Stmt},
    gc::Gc,
    vm::{op::OpLoadConstant, VmInst},
    IoWrite,
};

use super::{
    register_table::{ConstantValue, RegisterTable},
    Reg,
};

mod capture_scanner;
mod const_scanner;

pub use capture_scanner::CaptureScanner;
pub use const_scanner::ConstScanner;
