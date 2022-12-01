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
//! Work in progress...
//!
//!
mod backend;
mod console;
mod diagnostic;
mod frontend;
pub use console::Console;
pub use frontend::Parser;

#[cfg(feature = "vm")]
pub use backend::vm::runtime::VM;
