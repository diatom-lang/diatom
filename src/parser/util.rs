use std::str::Chars;

pub struct FileIterator<'a> {
    location: LineLocation,
    iterator: Chars<'a>,
}

impl<'a> FileIterator<'a> {
    pub fn new(file_content: &'a str) -> Self {
        Self {
            location: LineLocation::new(0, 0, 0),
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
            None => (None, None)
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
                self.location.global_offset += 1;
            }
            None => (),
        };
        next_item
    }
}

/// Indicate a location in a specific line.
#[derive(Clone)]
#[cfg_attr(test, derive(Debug))]
pub struct LineLocation {
    pub line: usize,
    pub offset: usize,
    pub global_offset: usize,
}

impl LineLocation {
    pub fn new(line: usize, offset: usize, global_offset: usize) -> Self {
        Self {
            line,
            offset,
            global_offset,
        }
    }
}

/// Indicate location of a range of text in a specific file.
#[cfg_attr(test, derive(Debug))]
pub struct FileLocation {
    pub start: LineLocation,
    pub end: LineLocation,
}

impl FileLocation {
    pub fn new(start: LineLocation, end: LineLocation) -> Self {
        Self { start, end }
    }
}
