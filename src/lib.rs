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
mod frontend;
pub use frontend::{Lexer, Parser};

#[cfg(feature = "vm")]
pub use backend::vm::vm::VM;

#[cfg(feature = "vm")]
extern crate ahash;
#[cfg(feature = "vm")]
extern crate serde;
#[cfg(feature = "vm")]
extern crate serde_cbor;
