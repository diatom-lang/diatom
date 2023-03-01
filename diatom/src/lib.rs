#![doc = include_str!("../../README.md")]

use std::{ffi::OsStr, io, path::PathBuf};

pub use diatom_core::{extension, ffi, IoWrite};

/// The version of this build
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

use diatom_core::Interpreter as __Interpreter;
use diatom_std_core::StdLibCore;

/// # The Diatom Interpreter
///
/// High performance interpreter for the diatom programming language. This interpreter compiles
/// diatom source code into byte code and executes the byte code with carefully tuned virtual
/// machine. Our benchmark shows it can match or even surpass the execution speed of Lua 5.4 .
///
/// # Example
///
/// ## 1. Run a piece of code
/// ```
/// # use diatom_core as diatom;
/// use diatom::Interpreter;
///
/// // Create a new instance of interpreter
/// // Enable colored output
/// let mut interpreter = Interpreter::with_color(std::io::stdout());
/// // Execute source code
/// let output = interpreter.exec(
///     "print('Hello, world!')",
///     "<test_code>", true
///     ).unwrap();
/// ```
///
/// ## 2. Add call back to the interpreter
/// ```
/// # use diatom_core as diatom;
/// use std::{cell::Cell, rc::Rc};
/// use diatom::{DiatomValue, Interpreter};
///
/// // this value will be modified
/// let value = Rc::new(Cell::new(0));
/// let value_capture = value.clone();
///
/// let mut interpreter = Interpreter::new(std::io::stdout());
/// // add a callback named "set_value"
/// interpreter.add_extern_function("set_value", move |_state, parameters, _out| {
///     if parameters.len() != 1 {
///         return Err("Expected 1 parameter!".to_string());
///     }
///     match parameters[0] {
///         DiatomValue::Int(i) => {
///             value_capture.set(i);
///             Ok(DiatomValue::Unit)
///         }
///         _ => Err("Invalid type".to_string()),
///     }
/// });
///
/// // change value to 5
/// interpreter.exec("$set_value(5)", "<test_code>", true).unwrap();
/// assert_eq!(value.get(), 5);
/// ```
pub struct Interpreter<Buffer: IoWrite>(__Interpreter<Buffer, StdLibCore>);

impl<Buffer: IoWrite> Interpreter<Buffer> {
    /// Create a new interpreter instance
    pub fn new(buffer: Buffer) -> Self {
        Self(__Interpreter::new(buffer))
    }

    /// Enable or disable REPL mode (print last value to output buffer)
    pub fn repl(&mut self, repl: bool) -> &mut Self {
        self.0.repl(repl);
        self
    }

    /// Add module search path
    pub fn with_search_path(&mut self, path: PathBuf) -> Result<(), io::Error> {
        self.0.with_search_path(path)
    }

    /// Enable ansi colored error message
    pub fn with_color(buffer: Buffer) -> Self {
        Self(__Interpreter::with_color(buffer))
    }

    /// Check if input is completeness
    ///
    /// Incomplete input usually contains unclosed parentheses, quotes or open expression.
    pub fn verify_input_completeness(&self, code: impl AsRef<str>) -> bool {
        self.0.verify_input_completeness(code)
    }

    /// Replace output buffer and get the old one
    pub fn replace_buffer(&mut self, buffer: Buffer) -> Buffer {
        self.0.replace_buffer(buffer)
    }

    /// Run a piece of diatom source code
    ///
    /// # Parameters
    /// * `code` - Source code
    /// * `source` - name of source code file or where it is from
    /// * `is_phony` - Whether source is a real path or a place holder
    ///
    /// # Return
    /// * Return the output of the program
    /// * If compilation failed or error occurs durning execution, an `Err(String)` that
    /// illustrates the error is returned.
    pub fn exec(
        &mut self,
        code: impl AsRef<str>,
        source: impl AsRef<OsStr>,
        is_phony: bool,
    ) -> Result<(), String> {
        self.0.exec(code, source, is_phony)
    }

    /// Show decompiled byte code for given source code.
    ///
    /// If compilation failed, `Err` will be returned.
    pub fn decompile(
        &mut self,
        code: impl AsRef<str>,
        source: impl AsRef<OsStr>,
        is_phony: bool,
    ) -> Result<String, String> {
        self.0.decompile(code, source, is_phony)
    }

    /// Load an rust extension.
    ///
    /// Return the extension if its namespace is already occupied
    pub fn load_ext(
        &mut self,
        extension: extension::Extension<Buffer>,
    ) -> Result<(), extension::Extension<Buffer>> {
        self.0.load_ext(extension)
    }
}
