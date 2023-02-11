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

mod diagnostic;
mod frontend;
mod interpreter;
mod vm;
pub use interpreter::GcObject as DiatomObject;
pub use interpreter::Interpreter;
pub use interpreter::Reg as DiatomValue;
pub use interpreter::State;
pub use std::io::Write as IoWrite;
/// The version of this build
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(feature = "console")]
mod console;
#[cfg(feature = "console")]
pub use console::Console;
