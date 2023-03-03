//! # The Diatom Programming Language
//! ![Unit Tests](https://github.com/diatom-lang/diatom/actions/workflows/rust.yml/badge.svg)
//! ![doc](https://github.com/diatom-lang/diatom/actions/workflows/rustdoc.yml/badge.svg)
//! [![Crates.io][crates-badge]][crates-url]
//! [![license][license-badge]][crates-url]
//!
//! [![dependency status](https://deps.rs/repo/github/diatom-lang/diatom/status.svg)](https://deps.rs/repo/github/diatom-lang/diatom)
//! ![issue](https://img.shields.io/github/issues/diatom-lang/diatom)
//! ![pr](https://img.shields.io/github/issues-pr/diatom-lang/diatom)
//! ![coverage](https://img.shields.io/codecov/c/github/diatom-lang/diatom)
//!
//! [crates-badge]: https://img.shields.io/crates/v/diatom.svg
//! [crates-url]: https://crates.io/crates/diatom
//! [license-badge]: https://img.shields.io/crates/l/diatom
//!
//! A dynamic typed scripting language for embedded use in applications. This project is yet another attempt of being a "better" lua.
//!
//! **Warning**: Project is still in experimental stage and API is considered as unstable.
//! # Example
//!
//! ## 1. Run a piece of code
//! ```
//! use diatom::Interpreter;
//!
//! // Create a new instance of interpreter
//! // Enable colored output
//! let mut interpreter = Interpreter::with_color(std::io::stdout());
//! // Execute source code
//! let output = interpreter.exec(
//!     "print('Hello, world!')",
//!     "<test_code>", true
//!     ).unwrap();
//! ```
//!
//! ## 2. Create a custom extension
//! ```
//! use std::sync::{Arc, Mutex};
//! use diatom::{
//!     ffi::{DiatomValue, ForeignFunction},
//!     extension::{Extension, ExtensionKind, AHashMap},
//!     Interpreter,
//!     IoWrite,
//! };
//!
//! // this value will be modified
//! let value = Arc::new(Mutex::new(1));
//! let value_capture = value.clone();
//!
//! let mut interpreter = Interpreter::new(std::io::stdout());
//!
//! pub fn my_ext<Buffer: IoWrite>(value: Arc<Mutex<i64>>) -> Extension<Buffer> {
//!     let mut funcs: AHashMap<String, Arc<ForeignFunction<Buffer>>> = AHashMap::default();
//!     funcs.insert(
//!         "set_value".to_string(),
//!         Arc::new(move |_, parameters, _| {
//!             if parameters.len() == 1{
//!                 if let DiatomValue::Int(i) = parameters[0] {
//!                     *value.lock().unwrap() = i;
//!                     return Ok(DiatomValue::Unit);
//!                 }
//!             }
//!             Err("Expect an `Int` as parameter!".to_string())
//!         }),
//!     );
//!    
//!     Extension {
//!         name: "my_ext".to_string(),
//!         kind: ExtensionKind::ForeignFunctions(funcs),
//!     }
//! }
//!
//! // register extension
//! interpreter.load_ext(my_ext(value_capture)).unwrap();
//! // change value to 5
//! interpreter.exec(r#"
//!     import set_value from my_ext
//!     set_value(5)
//! "#, "<test_code>", true).unwrap();
//! assert_eq!(*value.lock().unwrap(), 5);
//! ```

use std::{ffi::OsStr, io, path::PathBuf};

pub use diatom_core::{extension, ffi, IoWrite};

/// The version of this build
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

use diatom_core::{extension::Extension, Interpreter as __Interpreter};
use diatom_std_core::{std_lib, StdLibCore};

/// # The Diatom Interpreter
///
/// High performance interpreter for the diatom programming language. This interpreter compiles
/// diatom source code into byte code and executes the byte code with carefully tuned virtual
/// machine. Our benchmark shows it can match or even surpass the execution speed of Lua 5.4 .
///
pub struct Interpreter<Buffer: IoWrite>(__Interpreter<Buffer, StdLibCore>);

impl<Buffer: IoWrite> Interpreter<Buffer> {
    fn load_std(&mut self) {
        #[allow(unused_mut)]
        let mut std_lib_exts = std_lib();
        #[cfg(feature = "std-os")]
        std_lib_exts.push(diatom_std_os::os_extension());

        let std = Extension {
            name: "std".to_string(),
            kind: extension::ExtensionKind::SubExtensions(std_lib_exts),
        };
        self.load_ext(std).unwrap();
    }

    /// Create a new interpreter instance
    pub fn new(buffer: Buffer) -> Self {
        let mut interpreter = Self(__Interpreter::new(buffer));
        interpreter.load_std();
        interpreter
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
        let mut interpreter = Self(__Interpreter::with_color(buffer));
        interpreter.load_std();
        interpreter
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

#[cfg(test)]
mod tests {
    use std::{fs, path::PathBuf};

    use crate::Interpreter;

    #[test]
    fn test_examples() {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let mut path = path.parent().unwrap().to_path_buf();
        path.push("examples");
        let dir = fs::read_dir(path).unwrap();
        dir.for_each(|entry| {
            let path = entry.unwrap().path();
            if !path.is_file() {
                return;
            }
            let code = fs::read_to_string(&path).unwrap();
            let mut interpreter = Interpreter::new(vec![]);
            interpreter
                .exec(&code, &path, false)
                .map_err(|err| println!("{err}"))
                .expect("Example test failed");
            let mut interpreter = Interpreter::new(vec![]);
            interpreter
                .decompile(&code, &path, false)
                .map_err(|err| println!("{err}"))
                .expect("Example test failed");
        });
    }

    #[test]
    fn test_overflow() {
        let mut interpreter = Interpreter::new(vec![]);
        interpreter
            .exec("Int::MIN.abs()", "test", true)
            .map_err(|err| println!("{err}"))
            .expect("Test failed");
    }

    #[test]
    fn test_for_macro() {
        let mut interpreter = Interpreter::new(vec![]);
        interpreter
            .exec("fn = begin for i in 1..5 do end end", "test", true)
            .map_err(|err| println!("{err}"))
            .expect("Test failed");
    }
}
