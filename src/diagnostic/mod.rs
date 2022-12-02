use codespan_reporting::{
    diagnostic::{self, Severity},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Chars,
    },
};
use std::{
    ffi::{OsStr, OsString},
    fmt::Display,
    fs,
    io::Error,
    ops::Range,
    rc::Rc,
};

pub type Diagnostic = diagnostic::Diagnostic<usize>;
pub type Loc = Range<usize>;

/// Manage and display diagnoses and opened files
pub struct Diagnoser {
    files: SimpleFiles<DisplayableOsString, SharedFile>,
    diagnoses: Vec<Diagnostic>,
    error_count: usize,
    warning_count: usize,
}

#[derive(Clone)]
pub struct DisplayableOsString {
    s: OsString,
}

impl Display for DisplayableOsString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.s.to_str().unwrap_or("[[Can not show non utf-8 path]]")
        )
    }
}

impl DisplayableOsString {
    pub fn new(s: OsString) -> Self {
        Self { s }
    }
}

impl From<&OsStr> for DisplayableOsString {
    fn from(s: &OsStr) -> Self {
        Self {
            s: s.to_os_string(),
        }
    }
}

/// A shared file under Reference counter
#[derive(Clone)]
pub struct SharedFile {
    file: Rc<String>,
}

impl AsRef<str> for SharedFile {
    fn as_ref(&self) -> &str {
        self.file.as_str()
    }
}

impl SharedFile {
    /// Read a file from given path
    pub fn new(path: &OsStr) -> Result<Self, Error> {
        let content = fs::read_to_string(path)?;
        Ok(Self {
            file: Rc::new(content),
        })
    }

    /// Create a file from `&str`
    pub fn from_str(content: &str) -> Self {
        Self {
            file: Rc::new(content.to_string()),
        }
    }
}

impl Diagnoser {
    pub fn new() -> Self {
        Self {
            files: SimpleFiles::new(),
            diagnoses: vec![],
            error_count: 0,
            warning_count: 0,
        }
    }

    pub fn new_file(&mut self, path: OsString, content: SharedFile) -> usize {
        self.files.add(DisplayableOsString::new(path), content)
    }

    pub fn push(&mut self, diag: Diagnostic) {
        use Severity::*;
        match diag.severity {
            Bug => todo!(),
            Error => self.error_count += 1,
            Warning => self.warning_count += 1,
            Note => todo!(),
            Help => todo!(),
        }
        self.diagnoses.push(diag)
    }

    /// Count all diagnoses
    pub fn count(&self) -> usize {
        self.diagnoses.len()
    }

    pub fn error_count(&self) -> usize {
        self.error_count
    }

    pub fn warning_count(&self) -> usize {
        self.warning_count
    }

    /// Print to stdout
    pub fn print(&self) {
        let writer = StandardStream::stdout(ColorChoice::Auto);
        let config = codespan_reporting::term::Config {
            chars: Chars::ascii(),
            ..Default::default()
        };
        for diagnostic in &self.diagnoses {
            let r = term::emit(&mut writer.lock(), &config, &self.files, diagnostic);
            if let Err(r) = r {
                println!("{:?}", r);
                println!("{:?}", diagnostic);
            }
        }
    }

    /// Clear current diagnoses
    pub fn clear(&mut self) {
        self.diagnoses.clear();
        self.error_count = 0;
        self.warning_count = 0;
    }
}
