pub enum ErrorType {}

/// This struct is used to generate nice error messages that the lexer & parser emitted.
pub struct ErrorReporter {
    errors: Vec<(&'static str, FileLocation, ErrorType)>,
}

impl ErrorReporter {
    /// Create a new error reporter.
    pub fn new() -> Self {
        Self { errors: vec![] }
    }

    /// Render a single error.
    ///
    /// This function will append render result to the string passed in so that we do not allocate
    /// a new string.
    fn render_one(
        error_string: &mut String,
        source: &str,
        location: &FileLocation,
        error: &ErrorType,
        file_content: &str,
    ) -> Result<(), ()> {
        todo!()
    }

    /// Render all errors as a string.
    ///
    /// The content of the file is passed as `&str`, if the location can not be found in the given
    /// content, an `Err(())` will be returned. Otherwise an `Ok(String)` is returned.
    pub fn render(&self, file_content: &str) -> Result<String, ()> {
        let mut result = String::new();
        for (source, location, error) in self.errors.iter() {
            ErrorReporter::render_one(&mut result, source, &location, &error, file_content)?;
        }
        Ok(result)
    }

    /// Clear all errors stored.
    pub fn clear(&mut self) {
        self.errors.clear();
    }

    /// Append a new error to existing ones.
    pub fn append(&mut self, source: &'static str, location: FileLocation, error: ErrorType) {
        self.errors.push((source, location, error))
    }

    /// Check if there is any error.
    pub fn is_empty(&self) -> bool{
        self.errors.is_empty()
    }
}

/// Indicate a location in a specific line.
#[derive(Clone)]
pub struct LineLocation {
    pub line: usize,
    pub offset: usize,
}

impl LineLocation {
    pub fn new(line: usize, offset: usize) -> Self {
        Self { line, offset }
    }
}

/// Indicate location of a range of text in a specific file.
pub struct FileLocation {
    pub start: LineLocation,
    pub end: LineLocation,
}

impl FileLocation {
    pub fn new(start: LineLocation, end: LineLocation) -> Self {
        Self { start, end }
    }
}
