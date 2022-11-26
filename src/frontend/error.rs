use super::util::FileLocation;

#[cfg_attr(test, derive(Debug))]
pub enum ErrorType {
    InvalidNum(String),
    InvalidStr(String),
    InvalidOp(String),
    UnexpectedToken((String, String)),
    IrError(String),
    EofError(),
}

/// Record and report errors.
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
    /// a new string. If an error is returned, the sting passed in will not be changed.
    fn render_one(
        error_string: &mut String,
        source: &str,
        location: &FileLocation,
        error: &ErrorType,
        file_content: &str,
        file_name: Option<&str>,
    ) -> Result<(), ()> {
        // Try to get the source code
        let mut code: Vec<String> = vec![];
        let mut line_count = 0;
        let mut iter = file_content.chars();
        // skip first n lines
        while line_count < location.start.line {
            match iter.next() {
                Some('\n') => {
                    line_count += 1;
                }
                Some(_) => (),
                None => return Err(()),
            }
        }
        code.push(String::new());
        let mut code_len = code.len();
        while line_count <= location.end.line {
            match iter.next() {
                Some('\n') => {
                    line_count += 1;
                    code.push(String::new());
                    code_len = code.len();
                }
                Some(c) => {
                    code[code_len - 1].push(c);
                }
                None => {
                    if code[code_len - 1].len() >= location.end.offset {
                        break;
                    } else {
                        return Err(());
                    }
                }
            }
        }
        // Render error head
        *error_string += format!("{source}: ").as_str();
        *error_string += match error {
            ErrorType::InvalidNum(s) => {
                format!("Invalid numeric literal: `{s}`")
            }
            ErrorType::InvalidStr(s) => {
                format!("Invalid string literal: `{s}`")
            }
            ErrorType::InvalidOp(s) => {
                format!("Invalid operator: `{s}`")
            }
            ErrorType::UnexpectedToken((s, msg)) => {
                format!("Unexpected Token: `{s}` is not allowed here. {msg}")
            }
            ErrorType::IrError(s) => {
                format!("Ir Builder Error: {s}")
            }
            ErrorType::EofError() => {
                format!("Unexpected end of file")
            }
        }
        .as_str();
        error_string.push('\n');
        let line_number_len = location.end.line.to_string().len();

        if let Some(s) = file_name {
            *error_string += format!(
                "{}--> {s}: {}:{} - {}:{}\n",
                " ".repeat(line_number_len).as_str(),
                location.start.line,
                location.start.offset,
                location.end.line,
                location.end.offset
            )
            .as_str();
        }
        // Render error with source code
        *error_string += format!("{} |\n", " ".repeat(line_number_len)).as_str();
        for line in location.start.line..=location.end.line {
            if location.end.offset == 0 && line == location.end.line && line != location.start.line
            {
                break;
            }
            // print code line
            *error_string += format!("{:<line_number_len$} | ", line,).as_str();
            for c in code[line - location.start.line].chars() {
                if c == '\t' {
                    // tab replace by 4 space
                    error_string.push_str("    ");
                } else {
                    error_string.push(c);
                }
            }
            error_string.push('\n');
            // print pointer to errors
            if location.start.line == location.end.line {
                *error_string += format!("{} | ", " ".repeat(line_number_len)).as_str();
                for (i, c) in code[line - location.start.line].chars().enumerate() {
                    let push_char;
                    if i < location.start.offset || i >= location.end.offset {
                        push_char = ' ';
                    } else if i == location.end.offset - 1 || i == location.start.offset {
                        push_char = '^'
                    } else {
                        push_char = '~'
                    }
                    error_string.push(push_char);
                    if c == '\t' {
                        error_string.push(push_char);
                        error_string.push(push_char);
                        error_string.push(push_char);
                    }
                }
            } else if line == location.start.line {
                *error_string += format!("{} | ", " ".repeat(line_number_len)).as_str();
                for (i, c) in code[line - location.start.line].chars().enumerate() {
                    let offset = location.start.offset;
                    let push_char = match i {
                        i if i < offset => ' ',
                        i if i == offset => '^',
                        _ => '~'
                    };
                    error_string.push(push_char);
                    if c == '\t' {
                        error_string.push(push_char);
                        error_string.push(push_char);
                        error_string.push(push_char);
                    }
                }
                if location.end.offset == 0 && location.end.line == location.start.line + 1 {
                    error_string.pop();
                    error_string.push('^');
                }
            } else if location.end.offset == 0 && line == location.end.line - 1 {
                *error_string += format!("{} | ", " ".repeat(line_number_len)).as_str();
                for c in code[line - location.start.line].chars() {
                    let push_char = '~';
                    error_string.push(push_char);
                    if c == '\t' {
                        error_string.push(push_char);
                        error_string.push(push_char);
                        error_string.push(push_char);
                    }
                }
                error_string.pop();
                error_string.push('^')
            } else if line == location.end.line {
                *error_string += format!("{} | ", " ".repeat(line_number_len)).as_str();
                for (i, c) in code[line - location.start.line].chars().enumerate() {
                    let push_char;
                    if i == location.end.offset - 1 {
                        push_char = '^';
                    } else if i >= location.end.offset {
                        push_char = ' '
                    } else {
                        push_char = '~'
                    }
                    error_string.push(push_char);
                    if c == '\t' {
                        error_string.push(push_char);
                        error_string.push(push_char);
                        error_string.push(push_char);
                    }
                }
            } else {
                *error_string += format!("{} | ", " ".repeat(line_number_len),).as_str();
                for c in code[line - location.start.line].chars() {
                    let push_char = '~';
                    error_string.push(push_char);
                    if c == '\t' {
                        error_string.push(push_char);
                        error_string.push(push_char);
                        error_string.push(push_char);
                    }
                }
            }
            error_string.push('\n');
        }

        Ok(())
    }

    /// Render all errors as a string.
    ///
    /// The content of the file is passed as `&str`, if the location can not be found in the given
    /// content, an `Err(())` will be returned. Otherwise an `Ok(String)` is returned.
    ///
    /// # Known Issue
    /// Some unicode character, emojis for example, takes more than one normal character width.
    /// This actually breaks the pointer system for the rest of the line.
    ///
    /// # Example output:
    /// ```sh
    /// Tester: Invalid string literal: `Test Error`
    ///  --> mod_test.dm: 0:0 - 0:4
    ///   |
    /// 0 | abcd
    ///   | ^~~^
    ///
    /// Tester: Invalid string literal: `Test Error`
    ///  --> mod_test.dm: 0:0 - 1:0
    ///   |
    /// 0 | abcd
    ///   | ^~~^
    ///
    /// Tester: Invalid string literal: `Test Error`
    ///  --> mod_test.dm: 0:0 - 1:3
    ///   |
    /// 0 | abcd
    ///   | ^~~~
    /// 1 |     10
    ///   | ~~~~~^
    ///
    /// Tester: Invalid string literal: `Test Error`
    ///  --> mod_test.dm: 0:0 - 3:3
    ///   |
    /// 0 | abcd
    ///   | ^~~~
    /// 1 |     10
    ///   | ~~~~~~
    /// 2 | 5 []
    ///   | ~~~~
    /// 3 | efg
    ///   | ~~^
    ///
    /// Tester: Invalid string literal: `Test Error`
    ///  --> mod_test.dm: 2:2 - 3:3
    ///   |
    /// 2 | 5 []
    ///   |   ^~
    /// 3 | efg
    ///   | ~~^
    /// ```
    pub fn render(&self, file_content: &str, file_name: Option<&str>) -> Result<String, ()> {
        let mut result = String::new();
        for (source, location, error) in self.errors.iter() {
            ErrorReporter::render_one(
                &mut result,
                source,
                location,
                error,
                file_content,
                file_name,
            )?;
            result.push('\n');
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::util::*;

    #[test]
    fn render_test() {
        let mut error_string = String::new();
        let error = ErrorType::InvalidStr("Test Error".to_string());
        let file_content = "abcd\n\t10\n5 []\nefg";
        let source = "Tester";
        let file_name = "mod_test.dm";
        let location = FileLocation::new(LineLocation::new(0, 0), LineLocation::new(0, 4));
        ErrorReporter::render_one(
            &mut error_string,
            source,
            &location,
            &error,
            file_content,
            Some(file_name),
        )
        .expect("Render should success.");
        println!("{error_string}");

        error_string.clear();
        let location = FileLocation::new(LineLocation::new(0, 0), LineLocation::new(1, 0));
        ErrorReporter::render_one(
            &mut error_string,
            source,
            &location,
            &error,
            file_content,
            Some(file_name),
        )
        .expect("Render should success.");
        println!("{error_string}");

        error_string.clear();
        let location = FileLocation::new(LineLocation::new(0, 0), LineLocation::new(1, 3));
        ErrorReporter::render_one(
            &mut error_string,
            source,
            &location,
            &error,
            file_content,
            Some(file_name),
        )
        .expect("Render should success.");
        println!("{error_string}");

        error_string.clear();
        let location = FileLocation::new(LineLocation::new(0, 0), LineLocation::new(3, 3));
        ErrorReporter::render_one(
            &mut error_string,
            source,
            &location,
            &error,
            file_content,
            Some(file_name),
        )
        .expect("Render should success.");
        println!("{error_string}");

        error_string.clear();
        let location = FileLocation::new(LineLocation::new(2, 2), LineLocation::new(3, 3));
        ErrorReporter::render_one(
            &mut error_string,
            source,
            &location,
            &error,
            file_content,
            Some(file_name),
        )
        .expect("Render should success");
        println!("{error_string}");

        error_string.clear();
        let location = FileLocation::new(LineLocation::new(2, 2), LineLocation::new(3, 4));
        ErrorReporter::render_one(
            &mut error_string,
            source,
            &location,
            &error,
            file_content,
            Some(file_name),
        )
        .expect_err("Render should fail.");
        assert_eq!(error_string, "");
    }
}
