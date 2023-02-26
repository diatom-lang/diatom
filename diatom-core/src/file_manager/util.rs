use std::{
    cell::RefCell,
    ffi::OsString,
    fmt::Display,
    ops::{Add, Range},
    rc::Rc,
};

use crate::frontend::parser::ast::Stmt;

#[derive(Clone)]
#[cfg_attr(test, derive(Debug))]
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

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct PathShow {
    path: Rc<OsString>,
}

impl Display for PathShow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(s) = self.path.to_str() {
            write!(f, "{s}")
        } else {
            write!(f, "{:?}", self.path.as_os_str())
        }
    }
}

impl From<OsString> for PathShow {
    fn from(value: OsString) -> Self {
        PathShow {
            path: Rc::new(value),
        }
    }
}

pub struct SharedFile {
    pub file: Rc<String>,
    pub ast: Rc<RefCell<Vec<Stmt>>>,
}

impl AsRef<str> for SharedFile {
    fn as_ref(&self) -> &str {
        self.file.as_str()
    }
}
