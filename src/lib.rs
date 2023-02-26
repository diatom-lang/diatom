#![doc = include_str!("../README.md")]

pub use diatom_core::*;

/// The version of this build
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
