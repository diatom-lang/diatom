use std::{
    ffi::OsString,
    fmt::Display,
    ops::{Add, Range},
    sync::Arc,
};

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
    path: Arc<OsString>,
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
            path: Arc::new(value),
        }
    }
}

pub struct SharedFile {
    pub file: Arc<String>,
}

impl AsRef<str> for SharedFile {
    fn as_ref(&self) -> &str {
        self.file.as_str()
    }
}
