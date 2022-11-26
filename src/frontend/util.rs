use std::{ops::Add, str::Chars, fmt::Debug};

use super::lexer::Token;

pub struct FileIterator<'a> {
    location: LineLocation,
    iterator: Chars<'a>,
}

impl<'a> FileIterator<'a> {
    pub fn new(file_content: &'a str) -> Self {
        Self {
            location: LineLocation::new(0, 0),
            iterator: file_content.chars(),
        }
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

    /// Return current location's clone.
    pub fn get_location(&self) -> LineLocation {
        self.location.clone()
    }
}

impl<'a> Iterator for FileIterator<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let next_item = self.iterator.next();
        match next_item {
            Some(c) => {
                if c == '\n' {
                    self.location.line += 1;
                    self.location.offset = 0;
                } else {
                    self.location.offset += 1;
                };
            }
            None => (),
        };
        next_item
    }
}

/// Indicate a location in a specific line.
#[derive(Clone, PartialEq, Eq, Default)]
pub struct LineLocation {
    pub line: usize,
    pub offset: usize,
}

impl LineLocation {
    pub fn new(line: usize, offset: usize) -> Self {
        Self { line, offset }
    }
}

impl Debug for LineLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.offset)
    }
}

/// Indicate location of a range of text in a specific file.
#[derive(Clone, Default)]
pub struct FileLocation {
    pub start: LineLocation,
    pub end: LineLocation,
}

impl FileLocation {
    pub fn new(start: LineLocation, end: LineLocation) -> Self {
        Self { start, end }
    }
}

impl Add for FileLocation{
    type Output = FileLocation;
    fn add(self, rhs: Self) -> Self::Output {
        Self{
            start: self.end,
            end: rhs.end,
        }
    }
}

impl Debug for FileLocation{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}-{:?}", self.start, self.end)
    }
}

pub struct TokenIterator<'a> {
    iterator: std::slice::Iter<'a, (Token, FileLocation)>,
    location: FileLocation,
}

impl<'a> TokenIterator<'a> {
    pub fn new(vec: &'a [(Token, FileLocation)]) -> Self {
        TokenIterator {
            iterator: vec.iter(),
            location: Default::default(),
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        let mut iter = self.iterator.clone();
        iter.next().map(|x| &x.0)
    }

    pub fn peek2(&self) -> (Option<&Token>, Option<&Token>) {
        let mut iter = self.iterator.clone();
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

    pub fn get_location(&self) -> FileLocation {
        self.location.clone()
    }

}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.iterator.next().map(|x| {
            self.location = x.1.clone();
            &x.0
        })
    }
}
