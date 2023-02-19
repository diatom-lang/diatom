use codespan_reporting::{
    diagnostic::{self, Severity},
    files::SimpleFiles,
    term::{self, termcolor::Buffer, Chars},
};
use std::{
    ffi::{OsStr, OsString},
    fmt::{Debug, Display},
    io::Write,
    ops::{Add, Range},
    rc::Rc,
};

pub type Diagnostic = diagnostic::Diagnostic<usize>;

#[derive(Clone)]
pub struct Loc {
    pub start: usize,
    pub end: usize,
    pub fid: usize,
}

impl From<Loc> for Range<usize> {
    fn from(val: Loc) -> Self {
        val.start..val.end
    }
}

impl Add<Loc> for Loc {
    type Output = Self;
    fn add(self, rhs: Loc) -> Self::Output {
        assert_eq!(self.fid, rhs.fid);
        Loc {
            start: self.start,
            end: rhs.end,
            fid: self.fid,
        }
    }
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
    pub fn new(s: impl Into<OsString>) -> Self {
        Self { s: s.into() }
    }
}

impl From<&OsStr> for DisplayableOsString {
    fn from(s: &OsStr) -> Self {
        Self {
            s: s.to_os_string(),
        }
    }
}

struct SharedFile {
    file: Rc<String>,
}

impl SharedFile {
    fn new(file: String) -> Self {
        Self {
            file: Rc::new(file),
        }
    }
    fn get(&self) -> Rc<String> {
        self.file.clone()
    }
}

impl AsRef<str> for SharedFile {
    fn as_ref(&self) -> &str {
        self.file.as_ref().as_str()
    }
}

/// Manage and display diagnoses and opened files
pub struct FileManager {
    files: SimpleFiles<DisplayableOsString, SharedFile>,
    diagnoses: Vec<Diagnostic>,
    error_count: usize,
    has_eof_error: bool,
    has_non_eof_error: bool,
}

impl FileManager {
    pub fn new() -> Self {
        Self {
            files: SimpleFiles::new(),
            diagnoses: vec![],
            error_count: 0,
            has_eof_error: false,
            has_non_eof_error: false,
        }
    }

    pub fn add_file(&mut self, path: impl Into<OsString>, content: String) -> usize {
        self.files.add(DisplayableOsString::new(path), SharedFile::new(content))
    }

    pub fn get_file(&self, fid: usize) -> Rc<String> {
        self.files.get(fid).unwrap().source().get()
    }

    pub fn input_can_continue(&self) -> bool {
        self.has_eof_error && !self.has_non_eof_error
    }

    pub fn add_diagnostic(&mut self, diag: Diagnostic, is_eof: bool) {
        use Severity::*;
        match diag.severity {
            Bug => todo!(),
            Error => self.error_count += 1,
            Warning => todo!(),
            Note => todo!(),
            Help => todo!(),
        }
        if is_eof {
            self.has_eof_error = true;
        } else if diag.severity >= codespan_reporting::diagnostic::Severity::Error
            && !self.has_eof_error
        // prevent other error triggered by eof being recorded
        {
            self.has_non_eof_error = true;
        }
        self.diagnoses.push(diag)
    }

    pub fn error_count(&self) -> usize {
        self.error_count
    }

    pub fn clear_diagnoses(&mut self) {
        self.diagnoses.clear();
        self.error_count = 0;
        self.has_eof_error = false;
        self.has_non_eof_error = false;
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
            let r = term::emit(&mut writer, &config, &self.files, diagnostic);
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
