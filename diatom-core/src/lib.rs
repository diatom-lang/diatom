//! Diatom Interpreter Core
mod file_manager;
mod frontend;
mod gc;
mod interpreter;
mod vm;

#[cfg(test)]
mod tests;

pub use interpreter::std_core::StdCore;
pub use interpreter::Interpreter;
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
    pub type ForeignFunction<Buffer> = dyn Fn(&mut State<Buffer>, &[DiatomValue], &mut Buffer) -> Result<DiatomValue, String>
        + Send
        + Sync;
}

/// Diatom rust extension
pub mod extension {
    pub use super::interpreter::std_core::Extension;
    pub use super::interpreter::std_core::ExtensionKind;
    pub use ahash::AHashMap;
}
