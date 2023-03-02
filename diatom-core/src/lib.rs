//! Diatom Interpreter Core
mod file_manager;
mod frontend;
mod gc;
mod interpreter;
mod vm;

#[cfg(test)]
mod tests;

pub use interpreter::Interpreter;
pub use interpreter::StdCore;
pub use std::io::Write as IoWrite;

/// Diatom Foreign Function Interface
pub mod ffi {
    /// Unboxed Primitive Types
    pub use super::gc::Reg as DiatomValue;
    use super::interpreter::ffi;
    pub use ffi::DiatomList;
    pub use ffi::DiatomListMut;
    pub use ffi::DiatomObject;
    pub use ffi::DiatomObjectMut;
    pub use ffi::DiatomTable;
    pub use ffi::DiatomTableMut;
    pub use ffi::DiatomTuple;
    pub use ffi::DiatomTupleMut;
    pub use ffi::State;
    /// # Foreign Rust Function/Closure type
    ///
    /// This function does not accept due to potential recursive calls on a FnMut would violating
    /// borrow rules. You may want to use interior mutability if Fn is not flexible enough.
    /// External function should **NEVER PANIC**, otherwise it will crush the virtual machine.
    ///
    /// # External function parameters:
    /// * `State` - Access state and heap memory of the virtual machine.
    /// * `[DiatomValue]` - Parameters passed. The function is expected to check type and the
    /// number of parameters it received.
    /// * `Buffer` - Output buffer
    ///
    /// # External function return value:
    /// * Return a single unboxed value as return value. If the function does not intended to
    /// return anything, return an unit type `DiatomValue::Unit`.
    /// * If any unrecoverable error happens, return an `Err(String)` that illustrates the error.
    /// This will cause virtual machine to enter **panic mode** and stop execution.
    /// * If return value is `DiatomValue::Str` or `DiatomValue::Ref`, the reference id is checked.
    /// An invalid id would cause virtual machine to enter **panic mode** and stop execution.
    ///
    /// # Examples:
    /// ```
    /// # use diatom_core as diatom;
    /// use std::io::Write;
    /// use diatom::{Interpreter, DiatomValue};
    ///
    /// let buffer = Vec::<u8>::new();
    /// let mut interpreter = Interpreter::new(buffer);
    /// interpreter.add_extern_function(
    ///     "hello_world",
    ///     |state, parameters, out| {
    ///         if !parameters.is_empty(){
    ///             Err("Too many parameters!".to_string())
    ///         }else{
    ///             write!(out, "Hello, world!");
    ///             Ok(DiatomValue::Unit)
    ///         }
    ///     }
    /// );
    ///
    /// interpreter.exec("$hello_world()", "<test_code>", true).unwrap();
    /// let output = interpreter.replace_buffer(Vec::<u8>::new());
    /// let output = String::from_utf8(output).unwrap();
    /// assert_eq!(output, "Hello, world!")
    /// ```
    pub type ForeignFunction<Buffer> = dyn Fn(&mut State<Buffer>, &[DiatomValue], &mut Buffer) -> Result<DiatomValue, String>
        + Send
        + Sync;
}

/// Diatom rust extension
pub mod extension {
    pub use super::interpreter::Extension;
    pub use super::interpreter::ExtensionKind;
}
