use std::collections::HashMap;

use crate::frontend::LineLocation;

/// All possible types used by parser.
///
/// `Set`, `List` and `Dict` are three special classes that should be implemented by code generator
/// backend. Specially, `Any` means any type except `Nil` is possible.
pub enum Type {
    Any,
    Float,
    Int,
    Str,
    Class(String),
    Function(Vec<FuncArgType>),
    Nil,
}

/// Function argument & return types
///
/// Normal contains a specific type already defined. Auto contains a name of the type as a template.
pub enum FuncArgType {
    Normal(Type),
    Auto(String),
}

/// A vector of traits.
pub type Traits = Vec<String>;

type IrError = Result<(), String>;

/// Any backend implements this trait can be used by the parser.
pub trait IrBuilder {
    /// Create a new IrBuilder
    fn new() -> Self;

    /// Mark current line location
    fn mark_location(&mut self, location: LineLocation);

    /// Declare a variable.
    fn declare_var(&mut self, name: &String, var_type: Type) -> IrError;

    /// Declare a function.
    fn declare_func(
        &mut self,
        name: &String,
        func_type: Vec<FuncArgType>,
        bind: HashMap<String, Traits>,
    ) -> IrError;

    /// End a function.
    fn end_func(&mut self);

    /// Load an int const to variable.
    fn load_int(&mut self, name: &String, value: i64) -> IrError;

    /// Load an float const to variable.
    fn load_float(&mut self, name: &String, value: f64) -> IrError;

    /// Load an string const to variable.
    fn load_string(&mut self, name: &String, value: String) -> IrError;

    /// Unpack a value from tuple to a variable
    fn load_unpack(&mut self, src: &String, index: usize, dst: &String) -> IrError;

    fn add(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;
    fn sub(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;
    fn mul(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;
    fn div(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;
    fn modulo(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;
    fn exp(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;
    fn range(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;
    fn or(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;
    fn and(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;
    fn lt(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;
    fn le(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;
    fn ne(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;
    fn eq(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;
    fn gt(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;
    fn ge(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;

    fn not(&mut self, src: &String, dst: &String) -> IrError;
    fn neg(&mut self, src: &String, dst: &String) -> IrError;

    /// Divide and round the result down to the nearest integer.
    fn div_floor(&mut self, src1: &String, src2: &String, dst: &String) -> IrError;

    /// Get a intermediate variable
    ///
    /// The ir builder is responsible for resolving the type of this variable
    fn get_intermediate(&mut self) -> String;
}
