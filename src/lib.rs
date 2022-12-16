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
//! See examples in [Parser].
//!
//!
mod backend;
mod diagnostic;
mod frontend;
pub use frontend::Parser;
/// The version of this build
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(feature = "console")]
mod console;
#[cfg(feature = "console")]
pub use console::Console;

#[cfg(feature = "vm")]
pub use backend::{AsmFile, VM, VM_VERSION};
