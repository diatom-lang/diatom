#[cfg(feature = "vm")]
mod vm;

#[cfg(feature = "vm")]
pub use vm::{runtime::VM, AsmFile, VM_VERSION};
