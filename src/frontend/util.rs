use std::str::Chars;

use crate::diagnostic::Loc;

use super::Token;

pub struct FileIterator<'a> {
    offset: usize,
    iterator: Chars<'a>,
}

impl<'a> FileIterator<'a> {
    pub fn new(file_content: &'a str) -> Self {
        Self {
            offset: 0,
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

    /// Return current offset
    pub fn offset(&self) -> usize {
        self.offset
    }
}

impl<'a> Iterator for FileIterator<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let next_item = self.iterator.next();
        if next_item.is_some() {
            self.offset += 1
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
            loc: 0..0,
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
