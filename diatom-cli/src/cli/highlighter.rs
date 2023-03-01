use lazy_static::lazy_static;
use nu_ansi_term::{Color, Style};
use reedline::{Highlighter, StyledText};
use regex::{self, Regex};

const KEYWORDS: [&str; 23] = [
    "until", "end", "if", "then", "else", "elsif", "in", "for", "do", "return", "break",
    "continue", "loop", "def", "begin", "and", "or", "not", "fn", "is", "import", "from", "as",
];

const KEY_VALUES: [&str; 3] = ["true", "false", "self"];

const BUILT_IN_FUNC: [&str; 5] = ["println", "print", "panic", "unreachable", "todo"];

#[derive(Default)]
pub struct DiatomHighlighter;

fn is_key(s: &str) -> bool {
    KEYWORDS.iter().any(|k| *k == s)
}

fn is_key_value(s: &str) -> bool {
    KEY_VALUES.iter().any(|k| *k == s)
}

fn is_func(s: &str) -> bool {
    BUILT_IN_FUNC.iter().any(|k| *k == s)
}

lazy_static! {
    static ref COMMENT_STYLE: Style = Style::new().fg(Color::DarkGray);
    static ref KEY_STYLE: Style = Style::new().fg(Color::LightRed);
    static ref KEY_VALUE_STYLE: Style = Style::new().fg(Color::Yellow);
    static ref NUM_STYLE: Style = Style::new().fg(Color::Green);
    static ref STR_STYLE: Style = Style::new().fg(Color::LightMagenta);
    static ref FUNC_STYLE: Style = Style::new().fg(Color::Blue);
    static ref DEFAULT_STYLE: Style = Style::default();
    // Match valid integer or float point number
    static ref RE_NUM: Regex =
        Regex::new("^(([0][Xx][_0-9a-fA-F]+)|([0][Bb][_0-1]+)|([0][Oo][_0-7]+)|([0-9][_0-9]*(\\.[_0-9]+){0,1}([Ee][\\+\\-]{0,1}[0-9_]*){0, 1}))")
            .unwrap();
    // Match quoted string or unterminated quoted string
    static ref RE_STR: Regex =
        Regex::new(r#"^(("(\\.|[^\\"])*")|('(\\.|[^\\'])*')|("(\\.|[^\\"])*[\\]{0,1}$)|('(\\.|[^\\'])*[\\]{0,1}$))"#).unwrap();
    static ref RE_ID: Regex = Regex::new("^[_a-zA-Z]{1}[_0-9a-zA-Z]*").unwrap();
    static ref RE_COMMENT: Regex = Regex::new("^\\-\\-.*").unwrap();
    static ref RE_ANY: Regex = Regex::new(r#"(.|\n)"#).unwrap();
}

impl Highlighter for DiatomHighlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> reedline::StyledText {
        let mut styled_text = StyledText::new();
        let mut index = 0;
        while index < line.len() {
            if let Some(m) = RE_COMMENT.find(&line[index..]) {
                let end = index + m.end();
                let id = &line[index..end];
                styled_text.push((*COMMENT_STYLE, id.to_string()));
                index = end;
            } else if let Some(m) = RE_ID.find(&line[index..]) {
                let end = index + m.end();
                let id = &line[index..end];
                if is_key(id) {
                    styled_text.push((*KEY_STYLE, id.to_string()));
                } else if is_key_value(id) {
                    styled_text.push((*KEY_VALUE_STYLE, id.to_string()));
                } else if is_func(id) {
                    styled_text.push((*FUNC_STYLE, id.to_string()));
                } else {
                    styled_text.push((*DEFAULT_STYLE, id.to_string()));
                }
                index = end;
            } else if let Some(m) = RE_STR.find(&line[index..]) {
                let end = index + m.end();
                styled_text.push((*STR_STYLE, line[index..end].to_string()));
                index = end;
            } else if let Some(m) = RE_NUM.find(&line[index..]) {
                let end = index + m.end();
                styled_text.push((*NUM_STYLE, line[index..end].to_string()));
                index = end;
            } else if let Some(m) = RE_ANY.find(&line[index..]) {
                let end = index + m.end();
                styled_text.push((*DEFAULT_STYLE, line[index..end].to_string()));
                index = end;
            } else {
                unreachable!()
            }
        }
        styled_text
    }
}

#[test]
fn test_regex() {
    assert!(RE_ANY.find("\n").is_some());
    assert_eq!(RE_NUM.find("123.4e-90").unwrap().end(), 9);
    assert_eq!(RE_STR.find("\"asdf\\\"\naaa").unwrap().end(), 11);
    assert_eq!(RE_STR.find("\"asdf\\\"\"\naaa").unwrap().end(), 8);
    assert_eq!(RE_STR.find("'asdf\\'\naaa").unwrap().end(), 11);
    assert_eq!(RE_STR.find("'asdf\\''\naaa").unwrap().end(), 8);

    let highlighter = DiatomHighlighter::default();
    highlighter.highlight("if a then 1.234 else \"asdf\" end ", 0);
}
