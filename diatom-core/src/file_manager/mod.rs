use ahash::{AHashMap, AHashSet};
use codespan_reporting::{
    diagnostic::{self, Severity},
    files::SimpleFiles,
    term::{self, termcolor::Buffer, Chars},
};
use std::{collections::BTreeMap, ffi::OsString, io::Write, sync::Arc};

use crate::frontend::parser::ast::Stmt;

pub type Diagnostic = diagnostic::Diagnostic<usize>;

mod util;
pub use util::Loc;
use util::{PathShow, SharedFile};

/// Manage and display diagnoses and opened files
pub struct FileManager {
    files: SimpleFiles<PathShow, SharedFile>,
    file_map: AHashMap<PathShow, usize>,
    ast_map: BTreeMap<usize, Arc<Vec<Stmt>>>,
    diagnoses: Vec<Diagnostic>,
    extensions: AHashSet<String>,
    error_count: usize,
    has_eof_error: bool,
    has_non_eof_error: bool,
}

impl FileManager {
    pub fn new() -> Self {
        Self {
            files: SimpleFiles::new(),
            file_map: AHashMap::new(),
            ast_map: BTreeMap::new(),
            diagnoses: vec![],
            extensions: AHashSet::new(),
            error_count: 0,
            has_eof_error: false,
            has_non_eof_error: false,
        }
    }

    pub fn new_ext(&mut self, name: String) -> bool{
        self.extensions.insert(name)
    }

    pub fn is_ext_name(&mut self, name: impl AsRef<str>) -> bool {
        self.extensions.get(name.as_ref()).is_some()
    }

    pub fn add_file(&mut self, path: impl Into<OsString>, file: String) -> usize {
        let path = PathShow::from(path.into());
        let fid = self.files.add(
            path.clone(),
            SharedFile {
                file: Arc::new(file),
            },
        );
        self.file_map.insert(path, fid);
        fid
    }

    pub fn look_up_fid(&self, path: impl Into<OsString>) -> Option<usize> {
        self.file_map.get(&PathShow::from(path.into())).cloned()
    }

    pub fn get_file(&self, fid: usize) -> Arc<String> {
        self.files.get(fid).unwrap().source().file.clone()
    }

    pub fn set_ast(&mut self, fid: usize, ast: Vec<Stmt>) {
        assert!(self.files.get(fid).is_ok());
        self.ast_map.insert(fid, Arc::new(ast));
    }

    pub fn get_ast(&self, fid: usize) -> Arc<Vec<Stmt>> {
        self.ast_map.get(&fid).unwrap().clone()
    }

    pub fn input_can_continue(&self) -> bool {
        self.has_eof_error && !self.has_non_eof_error
    }

    pub fn add_diagnostic(&mut self, diag: Diagnostic, is_eof: bool) {
        use Severity::*;
        match diag.severity {
            Error => self.error_count += 1,
            _ => unreachable!(),
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
