mod error;
mod token;
use std::ffi::OsString;

use lazy_static::lazy_static;
use regex::Regex;
pub use token::{Keyword, Operator, Token};

use crate::diagnostic::{Diagnoser, Loc, SharedFile};

use self::error::{to_diagnostic, ErrorCode};

use super::util::{FileIterator, TokenIterator};

/// The lexical analyzer for Diatom.
///
/// # Errors
/// Error code `E0001` to `E0999` is reserved for lexer.
/// ```
pub struct Lexer {
    file_content: SharedFile,
    file_id: usize,
    diagnoser: Diagnoser,
    tokens: Vec<(Token, Loc)>,
    has_eof_error: bool,
    has_non_eof_error: bool,
}

impl Lexer {
    pub fn new(file_name: OsString, file_content: SharedFile) -> Self {
        let mut diagnoser = Diagnoser::new();
        let file_id = diagnoser.new_file(file_name, file_content.clone());
        let mut lexer = Self {
            file_content: SharedFile::from_str(""),
            diagnoser,
            file_id,
            tokens: vec![],
            has_eof_error: false,
            has_non_eof_error: false,
        };
        lexer.consume(&file_content);
        lexer.file_content = file_content;
        lexer
    }

    pub fn clear_diagnoses(&mut self) {
        self.has_non_eof_error = false;
        self.has_eof_error = false;
        self.diagnoser.clear();
    }

    pub fn has_eof_error(&self) -> bool {
        self.has_eof_error
    }

    pub fn has_non_eof_error(&self) -> bool {
        self.has_non_eof_error
    }

    pub fn print_diagnoses(&self) {
        self.diagnoser.print();
    }

    pub fn diagnostic_count(&self) -> usize {
        self.diagnoser.count()
    }

    pub fn error_count(&self) -> usize {
        self.diagnoser.error_count()
    }

    pub fn warning_count(&self) -> usize {
        self.diagnoser.warning_count()
    }

    /// Return an iterator tokens.
    pub(crate) fn iter(&self) -> TokenIterator {
        TokenIterator::new(&self.tokens)
    }

    fn add_diagnostic(&mut self, error: ErrorCode, loc: Loc) {
        match error {
            ErrorCode::OpenQuote => self.has_eof_error = true,
            _ => self.has_non_eof_error = true,
        }
        let diag = to_diagnostic(error, loc, self.file_id);
        self.diagnoser.push(diag);
    }

    /// Consume numeric types, aka int & float.
    fn consume_num(iter: &mut FileIterator) -> Result<(Token, Loc), (ErrorCode, Loc)> {
        fn consume_int(s: &str) -> Result<i64, ErrorCode> {
            let mut i: i64 = 0;
            if s.starts_with("0x") || s.starts_with("0X") {
                let mut iter = s.chars().skip(2);
                loop {
                    let c = iter.next();
                    match c {
                        Some(c) => {
                            let digit = match c {
                                '0'..='9' => c as i64 - '0' as i64,
                                'a'..='f' => c as i64 - 'a' as i64 + 10,
                                'A'..='F' => c as i64 - 'A' as i64 + 10,
                                _ => unreachable!(),
                            };
                            match i.checked_mul(16) {
                                Some(result) => {
                                    i = result;
                                }
                                None => {
                                    return Err(ErrorCode::IntegerOverflow);
                                }
                            };
                            match i.checked_add(digit) {
                                Some(result) => {
                                    i = result;
                                }
                                None => {
                                    return Err(ErrorCode::IntegerOverflow);
                                }
                            }
                        }
                        None => break,
                    }
                }
                return Ok(i);
            }

            if s.starts_with("0o") || s.starts_with("0O") {
                let mut iter = s.chars().skip(2);
                loop {
                    let c = iter.next();
                    match c {
                        Some(c) => match c {
                            '0'..='7' => {
                                let digit = c as i64 - '0' as i64;
                                match i.checked_mul(8) {
                                    Some(result) => {
                                        i = result;
                                    }
                                    None => {
                                        return Err(ErrorCode::IntegerOverflow);
                                    }
                                };
                                match i.checked_add(digit) {
                                    Some(result) => {
                                        i = result;
                                    }
                                    None => {
                                        return Err(ErrorCode::IntegerOverflow);
                                    }
                                }
                            }
                            _ => unreachable!(),
                        },
                        None => break,
                    }
                }
                return Ok(i);
            }
            if s.starts_with("0b") || s.starts_with("0B") {
                let mut iter = s.chars().skip(2);
                loop {
                    let c = iter.next();
                    match c {
                        Some(c) => match c {
                            '0'..='1' => {
                                let digit = c as i64 - '0' as i64;
                                match i.checked_mul(2) {
                                    Some(result) => {
                                        i = result;
                                    }
                                    None => {
                                        return Err(ErrorCode::IntegerOverflow);
                                    }
                                };
                                match i.checked_add(digit) {
                                    Some(result) => {
                                        i = result;
                                    }
                                    None => {
                                        return Err(ErrorCode::IntegerOverflow);
                                    }
                                }
                            }
                            _ => unreachable!(),
                        },
                        None => break,
                    }
                }
                return Ok(i);
            }

            let mut iter = s.chars();
            loop {
                let c = iter.next();
                match c {
                    Some(c) => match c {
                        '0'..='9' => {
                            let digit = c as i64 - '0' as i64;
                            match i.checked_mul(10) {
                                Some(result) => {
                                    i = result;
                                }
                                None => {
                                    return Err(ErrorCode::IntegerOverflow);
                                }
                            };
                            match i.checked_add(digit) {
                                Some(result) => {
                                    i = result;
                                }
                                None => {
                                    return Err(ErrorCode::IntegerOverflow);
                                }
                            }
                        }
                        _ => unreachable!(),
                    },
                    None => break,
                }
            }
            Ok(i)
        }
        let start = iter.offset();

        lazy_static!(
            static ref RE: Regex =
            Regex::new("^(([0][Xx][_0-9a-fA-F]+)|([0][Bb][_0-1]+)|([0][Oo][_0-7]+)|([0-9][_0-9]*(\\.[_0-9]+){0,1}([Ee][\\+\\-]{0,1}[0-9_]*){0, 1}))")
                .unwrap();
            static ref RE_INT: Regex = Regex::new("^(([0][Xx][_0-9a-fA-F]+)|([0][Bb][_0-1]+)|([0][Oo][_0-7]+)|([0-9][_0-9]*))$").unwrap();
        );

        let s = iter.as_str();
        let num = if let Some(m) = RE.find(s) {
            let s = s[..m.end()].to_string();
            let s = s.replace('_', "");
            (0..m.end()).for_each(|_| {
                iter.next();
            });
            s
        } else {
            let mut error_s = String::new();
            loop {
                match iter.peek() {
                    Some(c @ ('0'..='9' | 'a'..='z' | 'A'..='Z' | '.')) => error_s.push(c),
                    Some(_) => break,
                    None => break,
                }
                iter.next();
            }
            return Err((ErrorCode::InvalidNum(error_s), start..iter.offset()));
        };
        let end = iter.offset();
        let loc = start..end;

        let float_flag = !RE_INT.is_match(&num);

        if float_flag {
            let float = num.parse::<f64>();
            match float {
                Ok(f) => Ok((Token::Float(f), loc)),
                Err(e) => Err((
                    ErrorCode::ParseFloatError(format!("Cannot parse float literal: `{e}`")),
                    loc,
                )),
            }
        } else {
            let int = consume_int(&num);
            match int {
                Ok(i) => Ok((Token::Integer(i), loc)),
                Err(e) => Err((e, loc)),
            }
        }
    }

    /// Consume keyword or Identifier
    fn consume_id_or_key(iter: &mut FileIterator) -> Result<(Token, Loc), (ErrorCode, Loc)> {
        let mut name = String::new();
        let start = iter.offset();
        loop {
            match iter.peek() {
                Some('_') => name.push('_'),
                Some(' ' | '\r' | '\n' | '\t' | '!'..='/' | ':'..='@' | '['..='`' | '{'..='~') => {
                    break
                }
                Some(c) => name.push(c),
                None => break,
            }
            iter.next();
        }
        let loc = start..iter.offset();
        match name.as_str() {
            "and" => Ok((Token::Op(Operator::And), loc)),
            "or" => Ok((Token::Op(Operator::Or), loc)),
            "not" => Ok((Token::Op(Operator::Not), loc)),
            "true" => Ok((Token::Key(Keyword::True), loc)),
            "false" => Ok((Token::Key(Keyword::False), loc)),
            "do" => Ok((Token::Key(Keyword::Do), loc)),
            "where" => Ok((Token::Key(Keyword::Where), loc)),
            "until" => Ok((Token::Key(Keyword::Until), loc)),
            "end" => Ok((Token::Key(Keyword::End), loc)),
            "if" => Ok((Token::Key(Keyword::If), loc)),
            "then" => Ok((Token::Key(Keyword::Then), loc)),
            "else" => Ok((Token::Key(Keyword::Else), loc)),
            "elsif" => Ok((Token::Key(Keyword::Elsif), loc)),
            "case" => Ok((Token::Key(Keyword::Case), loc)),
            "in" => Ok((Token::Key(Keyword::In), loc)),
            "for" => Ok((Token::Key(Keyword::For), loc)),
            "nil" => Ok((Token::Key(Keyword::Nil), loc)),
            "return" => Ok((Token::Key(Keyword::Return), loc)),
            "assert" => Ok((Token::Key(Keyword::Assert), loc)),
            "continue" => Ok((Token::Key(Keyword::Continue), loc)),
            "break" => Ok((Token::Key(Keyword::Break), loc)),
            "loop" => Ok((Token::Key(Keyword::Loop), loc)),
            "class" => Ok((Token::Key(Keyword::Class), loc)),
            "def" => Ok((Token::Key(Keyword::Def), loc)),
            "begin" => Ok((Token::Key(Keyword::Begin), loc)),
            _ => Ok((Token::Id(name), loc)),
        }
    }

    /// Consume string token
    fn consume_string(iter: &mut FileIterator) -> Result<(Token, Loc), (ErrorCode, Loc)> {
        fn consume_escape(iter: &mut FileIterator) -> Result<char, ()> {
            /// Consume a hex escape sequence with n character exactly
            fn consume_hex_escape(iter: &mut FileIterator, count: u8) -> Result<char, ()> {
                let mut x: u32 = 0;
                for _ in 0..count {
                    x *= 16;
                    match iter.peek() {
                        Some(c @ '0'..='9') => x += c as u32 - '0' as u32,
                        Some(c @ 'a'..='f') => x += c as u32 - 'a' as u32 + 10,
                        Some(c @ 'A'..='F') => x += c as u32 - 'A' as u32 + 10,
                        _ => return Err(()),
                    }
                    iter.next();
                }
                match char::from_u32(x as u32) {
                    Some(c) => Ok(c),
                    None => Err(()),
                }
            }
            let c = iter.next();
            let c = match c {
                Some(c) => c,
                None => unreachable!(),
            };
            match c {
                '\\' => Ok('\\'),
                't' => Ok('\t'),
                'n' => Ok('\n'),
                'r' => Ok('\r'),
                '\"' => Ok('\"'),
                '\'' => Ok('\''),
                'x' => consume_hex_escape(iter, 2),
                'u' => consume_hex_escape(iter, 4),
                'U' => consume_hex_escape(iter, 8),
                _ => Err(()),
            }
        }
        let start = iter.offset();
        let start_char = iter.next();
        let mut result = String::new();
        let mut invalid = false;

        let is_single_quote = match start_char {
            Some('"') => false,
            Some('\'') => true,
            _ => unreachable!(),
        };
        loop {
            let c = iter.next();
            match c {
                Some(c) => match c {
                    '\\' => match consume_escape(iter) {
                        Ok(c) => result.push(c),
                        Err(()) => invalid = true,
                    },
                    '\'' if is_single_quote => {
                        if !invalid {
                            return Ok((Token::Str(result), start..iter.offset()));
                        } else {
                            return Err((ErrorCode::InvalidEscapeSequence, start..iter.offset()));
                        }
                    }
                    '"' if !is_single_quote => {
                        if !invalid {
                            return Ok((Token::Str(result), start..iter.offset()));
                        } else {
                            return Err((ErrorCode::InvalidEscapeSequence, start..iter.offset()));
                        }
                    }
                    c => result.push(c),
                },
                None => return Err((ErrorCode::OpenQuote, start..iter.offset())),
            }
        }
    }

    /// Consume operators
    fn consume_op(iter: &mut FileIterator) -> Result<(Token, Loc), (ErrorCode, Loc)> {
        fn consume_next_2_char(iter: &mut FileIterator) -> Loc {
            let start = iter.offset();
            iter.next();
            iter.next();
            let end = iter.offset();
            start..end
        }
        match iter.peek2() {
            (Some('/'), Some('/')) => {
                Ok((Token::Op(Operator::DivFloor), consume_next_2_char(iter)))
            }
            (Some('*'), Some('*')) => Ok((Token::Op(Operator::Exp), consume_next_2_char(iter))),
            (Some('.'), Some('.')) => Ok((Token::Op(Operator::Range), consume_next_2_char(iter))),
            (Some('>'), Some('=')) => Ok((Token::Op(Operator::Ge), consume_next_2_char(iter))),
            (Some('<'), Some('=')) => Ok((Token::Op(Operator::Le), consume_next_2_char(iter))),
            (Some('='), Some('=')) => Ok((Token::Op(Operator::Eq), consume_next_2_char(iter))),
            (Some('<'), Some('>')) => Ok((Token::Op(Operator::Ne), consume_next_2_char(iter))),
            (Some('|'), Some('>')) => {
                Ok((Token::Op(Operator::Pipeline), consume_next_2_char(iter)))
            }
            (Some(c), _) => {
                let start = iter.offset();
                iter.next();
                let loc = start..iter.offset();
                match c {
                    '+' => Ok((Token::Op(Operator::Plus), loc)),
                    '-' => Ok((Token::Op(Operator::Minus), loc)),
                    '*' => Ok((Token::Op(Operator::Mul), loc)),
                    '/' => Ok((Token::Op(Operator::Div), loc)),
                    '%' => Ok((Token::Op(Operator::Mod), loc)),
                    '=' => Ok((Token::Op(Operator::Assign), loc)),
                    ',' => Ok((Token::Op(Operator::Comma), loc)),
                    '.' => Ok((Token::Op(Operator::Member), loc)),
                    '>' => Ok((Token::Op(Operator::Gt), loc)),
                    '<' => Ok((Token::Op(Operator::Lt), loc)),
                    '(' => Ok((Token::Op(Operator::LPar), loc)),
                    ')' => Ok((Token::Op(Operator::RPar), loc)),
                    '[' => Ok((Token::Op(Operator::LBrk), loc)),
                    ']' => Ok((Token::Op(Operator::RBrk), loc)),
                    '{' => Ok((Token::Op(Operator::LBrc), loc)),
                    '}' => Ok((Token::Op(Operator::RBrc), loc)),
                    ':' => Ok((Token::Op(Operator::Colon), loc)),
                    '$' => Ok((Token::Op(Operator::Call), loc)),
                    c => Err((ErrorCode::InvalidOp(c), loc)),
                }
            }
            (None, _) => unreachable!(),
        }
    }

    /// Consume all tokens
    fn consume(&mut self, file_content: &SharedFile) {
        let mut iter = FileIterator::new(file_content.as_ref());
        // Ignore shebang (#!...) at the beginning of the file
        if let (Some('#'), Some('!')) = iter.peek2() {
            loop {
                if let Some('\n') = iter.next() {
                    break;
                }
            }
        }
        // Start consuming characters
        loop {
            // Match some pattern requires 2 lookahead
            match iter.peek2() {
                (Some('-'), Some('-')) => {
                    // Ignore comment
                    loop {
                        let c = iter.peek();
                        match c {
                            Some(c) => {
                                if c == '\n' {
                                    break;
                                };
                            }
                            None => break,
                        }
                        iter.next();
                    }
                    continue;
                }
                (Some(c), _) => {
                    let result = match c {
                        '0'..='9' => Some(Self::consume_num(&mut iter)),
                        '"' | '\'' => Some(Self::consume_string(&mut iter)),
                        ' ' | '\t' | '\r' | '\n' => {
                            iter.next();
                            None
                        } // Ignore whitespace
                        c @ ('!'..='/' | ':'..='@' | '['..='`' | '{'..='~') if c != '_' => {
                            Some(Self::consume_op(&mut iter))
                        }
                        _ => Some(Self::consume_id_or_key(&mut iter)),
                    };
                    if let Some(result) = result {
                        match result {
                            Ok(x) => self.tokens.push(x),
                            Err((error, loc)) => {
                                self.add_diagnostic(error, loc);
                            }
                        }
                    }
                }
                (None, _) => {
                    break;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn test_consume_int() {
        fn test_helper(s: &str, i: i64, should_fail: bool) {
            let mut iter = FileIterator::new(s);
            let result = Lexer::consume_num(&mut iter);
            if should_fail {
                assert!(
                    result.is_err(),
                    "Expected parse success! source = {s}, result = {:?}",
                    result
                );
            } else if let Ok((Token::Integer(j), _)) = result {
                if i != j {
                    println!("{s}");
                };
                assert_eq!(i, j);
            } else {
                panic!(
                    "Expected parse failure! source = {s} , result = {:?}",
                    result
                );
            }
        }

        test_helper("1_23", 123, false);
        test_helper("0xf_f", 0xff, false);
        test_helper("0Xff", 0xff, false);
        test_helper("0b1__00011", 0b100011, false);
        test_helper("0o776_610_", 0o776610, false);
        test_helper("999+3", 999, false);
        test_helper("9_223_372_036_854_775_808", 0, true); // Overflow i64
        test_helper(
            "9_223_372_036_854_775_807",
            9_223_372_036_854_775_807,
            false,
        ); // Overflow i64
        test_helper("0x8000000000000000", 0, true); // Overflow i64
        test_helper("0x7fffffffffffffff", 9_223_372_036_854_775_807, false); // Overflow i64
        test_helper("0o1000000000000000000000", 0, true); // Overflow i64
        test_helper("0o777777777777777777777", 9_223_372_036_854_775_807, false); // Overflow i64
        test_helper(
            "0b1000000000000000000000000000000000000000000000000000000000000000",
            0,
            true,
        ); // Overflow i64
        test_helper(
            "0b111111111111111111111111111111111111111111111111111111111111111",
            9_223_372_036_854_775_807,
            false,
        ); // Overflow i64
        test_helper("0xffabcde", 0xffabcde, false);
        test_helper("0O999", 0, false);
        test_helper("123y", 123, false);
        // This must not be parsed as a float as '123..int$()' is ambiguous
        // It can be '123. ' then call 'int$()' on a float
        // Also may be a range from '123' to 'int$()'
        test_helper("123.", 123, false);
    }

    #[test]
    fn test_consume_float() {
        fn test_helper(s: &str, i: f64, should_fail: bool) {
            let mut iter = FileIterator::new(s);
            let result = Lexer::consume_num(&mut iter);
            if should_fail {
                assert!(
                    result.is_err(),
                    "Expected parse success! source = {s}, result = {:?}",
                    result
                );
            } else if let Ok((Token::Float(j), _)) = result {
                assert_eq!(i, j);
            } else {
                panic!(
                    "Expected parse failure! source = {s} , result = {:?}",
                    result
                );
            }
        }

        test_helper("123e14", 123e14, false);
        test_helper("123.0.", 123.0, false);
        test_helper("1_23e_14", 123e14, false);
        test_helper("123E-14", 123e-14, false);
        test_helper("0.01__2", 0.012, false);
        test_helper("123e1y", 1230., false);
    }

    #[test]
    fn test_consume_string() {
        fn test_helper(s: &str, i: &str, should_fail: bool) {
            let mut iter = FileIterator::new(s);
            let result = Lexer::consume_string(&mut iter);
            if should_fail {
                assert!(
                    result.is_err(),
                    "Expected parse success! source = {s}, result = {:?}",
                    result
                );
            } else if let Ok((Token::Str(j), _)) = result {
                assert_eq!(i, j);
            } else {
                panic!(
                    "Expected parse failure! source = {s} , result = {:?}",
                    result
                );
            }
        }

        test_helper(
            r#""abs__0a™£´∂`'`c\'\"k\n\rkk\x09'""#,
            "abs__0a™£´∂`'`c\'\"k\n\rkk\x09'",
            false,
        );
        test_helper(
            r#"'rrr__0a™£´∂`"`c\'\"k\n\rkk\u00E9"'"#,
            "rrr__0a™£´∂`\"`c\'\"k\n\rkk\u{00E9}\"",
            false,
        );
        test_helper("'\\xaz'", "", true);
        test_helper("'\\u@0'", "", true);
        test_helper("'\\U0000fFFf'", "\u{ffff}", false);
        test_helper("'\\ufFff'", "\u{ffff}", false);
        test_helper("'\\UdFFf'", "", true);
        test_helper("'\\uDfff'", "", true);
        test_helper("'", "", true);
    }

    fn test_str(code: &str, should_fail: bool) {
        let file = SharedFile::from_str(code);
        let lexer = Lexer::new(OsString::from_str("").unwrap(), file);
        for token in lexer.tokens.iter() {
            println!("{:?}", token.0);
        }

        if !should_fail && lexer.diagnostic_count() > 0 {
            lexer.print_diagnoses();
        }
        if should_fail {
            assert!(lexer.diagnostic_count() > 0);
        } else {
            assert!(lexer.diagnostic_count() == 0);
        }
    }

    #[test]
    fn test_dispatch() {
        let code = r#"#!/bin/diatom -c 
            def fac(a)
                b = 1 c =2 -- @@@@
                if a> 0 then return a *fac(a - 1) elsif 
                a == 0 then return 1 else return 0
                end end
            ß = fac(5) å = ß**3//0x11f |> fac 
            set = {'s', 'ma\u00E9', 65e52, 0b00110} dict = {:}.insert(('key', 98)) 
            🐶🐱 <> "Doa\x09 and cat'?'" 
            "#;
        test_str(code, false);
    }

    #[test]
    fn test_valid() {
        let code = "____";
        test_str(code, false);
    }
}
