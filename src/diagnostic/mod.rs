use codespan_reporting::{
    diagnostic::{self, Severity},
    files::SimpleFile,
    term::{self, termcolor::Buffer, Chars},
};
use std::{
    ffi::{OsStr, OsString},
    fmt::{Debug, Display},
    fs,
    io::{Error, Write},
    ops::Range,
    rc::Rc,
};

pub type Diagnostic = diagnostic::Diagnostic<()>;
pub type Loc = Range<usize>;

/// Manage and display diagnoses and opened files
pub struct Diagnoser {
    file: SimpleFile<DisplayableOsString, SharedFile>,
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

impl Debug for DisplayableOsString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
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
    pub fn new(path: OsString, content: SharedFile) -> Self {
        Self {
            file: SimpleFile::new(DisplayableOsString::new(path), content),
            diagnoses: vec![],
            error_count: 0,
            warning_count: 0,
        }
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
    pub fn error_count(&self) -> usize {
        self.error_count
    }

    /// Render error to string
    pub fn render(&self, color: bool) -> String {
        let mut writer = if color {
            Buffer::ansi()
        } else {
            Buffer::no_color()
        };
        let config = codespan_reporting::term::Config {
            chars: Chars::ascii(),
            ..Default::default()
        };
        for diagnostic in &self.diagnoses {
            let r = term::emit(&mut writer, &config, &self.file, diagnostic);
            if let Err(r) = r {
                let _ = writeln!(writer, "{r:?}");
                let _ = writeln!(writer, "{diagnostic:?}");
            }
        }
        String::from_utf8(writer.into_inner()).unwrap_or_else(|_| {
            format!(
                "{}:{}: Internal error(Invalid utf8 buffer)",
                file!(),
                line!()
            )
        })
    }
}
