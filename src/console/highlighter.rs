use lazy_static::lazy_static;
use nu_ansi_term::{Color, Style};
use reedline::{Highlighter, StyledText};
use regex::{self, Regex};

const KEYWORDS: [&str; 20] = [
    "true", "false", "do", "end", "if", "then", "else", "elsif", "case", "in", "for", "nil",
    "assert", "return", "break", "continue", "loop", "class", "def", "begin",
];

#[derive(Default)]
pub struct DiatomHighlighter;

fn is_key(s: &str) -> bool {
    KEYWORDS.iter().any(|k| *k == s)
}

impl Highlighter for DiatomHighlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> reedline::StyledText {
        lazy_static! {
            static ref KEY_STYLE: Style = Style::new().fg(Color::Red);
            static ref NUM_STYLE: Style = Style::new().fg(Color::Cyan);
            static ref STR_STYLE: Style = Style::new().fg(Color::Magenta);
            static ref DEFAULT_STYLE: Style = Style::default();
            // Match valid integer or float point bumber
            static ref RE_NUM: Regex =
                Regex::new("^(([0][Xx][_0-9a-fA-F]+)|([0][Bb][_0-1]+)|([0][Oo][_0-7]+)|([0-9][_0-9]*(\\.[_0-9]+){0,1}([Ee][\\+\\-]{0,1}[0-9_]*){0, 1}))")
                    .unwrap();
            // Match quoted string or unterminated quoted string
            static ref RE_STR: Regex =
                Regex::new(r#"^(("(\\.|[^\\"])*")|('(\\.|[^\\'])*')|("(\\.|[^\\"])*[\\]{0,1}$)|('(\\.|[^\\'])*[\\]{0,1}$))"#).unwrap();
            static ref RE_ID: Regex = Regex::new("^[_a-zA-Z]{1}[_0-9a-zA-Z]*").unwrap();
            static ref RE_ANY: Regex = Regex::new(r#"(.|\n)"#).unwrap();
        }
        let mut styled_text = StyledText::new();
        let mut index = 0;
        while index < line.len() {
            if let Some(m) = RE_ID.find(&line[index..]) {
                let end = index + m.end();
                let id = &line[index..end];
                if is_key(id) {
                    styled_text.push((KEY_STYLE.clone(), id.to_string()));
                } else {
                    styled_text.push((DEFAULT_STYLE.clone(), id.to_string()));
                }
                index = end;
            } else if let Some(m) = RE_STR.find(&line[index..]) {
                let end = index + m.end();
                styled_text.push((STR_STYLE.clone(), line[index..end].to_string()));
                index = end;
            } else if let Some(m) = RE_NUM.find(&line[index..]) {
                let end = index + m.end();
                styled_text.push((NUM_STYLE.clone(), line[index..end].to_string()));
                index = end;
            } else if let Some(m) = RE_ANY.find(&line[index..]) {
                let end = index + m.end();
                styled_text.push((DEFAULT_STYLE.clone(), line[index..end].to_string()));
                index = end;
            } else {
                unreachable!()
            }
        }
        styled_text
    }
}