//! This crate implements an interpreter for Diatom.
//!
//! ## The Diatom programming language
//!
//! Diatom is a fast, elegant and secure language for writing dynamic script that will be
//! embedded in an application.
//!
//! For the syntax specification see [The Book(WIP)]().
//!
//! ## How to use this interpreter
//!
//! See examples in [Interpreter].
//!

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
/// The version of this build
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
