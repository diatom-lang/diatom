use std::str::Chars;

use crate::file_manager::Loc;

use super::Token;

pub struct FileIterator<'a> {
    offset: usize,
    iterator: Chars<'a>,
    fid: usize,
}

impl<'a> FileIterator<'a> {
    pub fn new(file: &'a str, fid: usize) -> Self {
        Self {
            offset: 0,
            iterator: file.chars(),
            fid,
        }
    }

    /// Return file id
    pub fn fid(&self) -> usize {
        self.fid
    }

    /// Return next character. Useful to lookahead.
    pub fn peek(&self) -> Option<char> {
        let mut iter = self.iterator.clone();
        iter.next()
    }

    /// Return the next two character
    pub fn peek2(&self) -> (Option<char>, Option<char>) {
        let mut iter = self.iterator.clone();
        match iter.next() {
            c1 @ Some(_) => (c1, iter.next()),
            None => (None, None),
        }
    }

    /// Return current offset
    pub fn offset(&self) -> usize {
        self.offset
    }

    /// Return &str since current location
    pub fn as_str(&self) -> &str {
        self.iterator.as_str()
    }
}

impl<'a> Iterator for FileIterator<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let next_item = self.iterator.next();
        if let Some(c) = next_item {
            self.offset += c.len_utf8()
        };
        next_item
    }
}

pub struct TokenIterator<'a> {
    iter: std::slice::Iter<'a, (Token, Loc)>,
    loc: Loc,
}

impl<'a> TokenIterator<'a> {
    pub fn new(vec: &'a [(Token, Loc)]) -> Self {
        TokenIterator {
            iter: vec.iter(),
            loc: Loc {
                start: 0,
                end: 0,
                fid: usize::MAX,
            },
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        let mut iter = self.iter.clone();
        iter.next().map(|x| &x.0)
    }

    pub fn peek2(&self) -> (Option<&Token>, Option<&Token>) {
        let mut iter = self.iter.clone();
        match iter.next() {
            t1 @ Some(_) => {
                let t1 = t1.map(|x| &x.0);
                match iter.next() {
                    t2 @ Some(_) => {
                        let t2 = t2.map(|x| &x.0);
                        (t1, t2)
                    }
                    None => (t1, None),
                }
            }
            None => (None, None),
        }
    }

    pub fn loc(&self) -> Loc {
        self.loc.clone()
    }

    pub fn next_loc(&self) -> Loc {
        let mut iter = self.iter.clone();
        match iter.next() {
            Some((_, loc)) => loc.clone(),
            None => self.loc(),
        }
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|x| {
            self.loc = x.1.clone();
            &x.0
        })
    }
}
