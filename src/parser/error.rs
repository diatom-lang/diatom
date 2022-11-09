use super::util::FileLocation;

#[cfg_attr(test, derive(Debug))]
pub enum ErrorType {
    InvalidNum(String),
    InvalidStr(String),
}

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
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }
}
