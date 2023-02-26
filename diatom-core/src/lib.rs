//! Diatom Interpreter Core
mod file_manager;
mod frontend;
mod gc;
mod interpreter;
mod prelude;
mod vm;

#[cfg(test)]
mod tests;

pub use gc::Reg as DiatomValue;
pub use interpreter::Interpreter;
pub use interpreter::State;
pub use std::io::Write as IoWrite;
