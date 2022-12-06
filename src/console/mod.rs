use std::ffi::OsStr;

use crate::Parser;

mod highlighter;
mod prompt;
mod validator;

const VERSION: &str = env!("CARGO_PKG_VERSION");

/// An interactive console for Diatom
pub struct Console {
    parser: Parser,
    color: bool,
}

impl Console {
    pub fn new(color: bool) -> Self {
        Self {
            parser: Parser::new(),
            color,
        }
    }

    /// Run this console
    pub fn run(&mut self) {
        use std::io::stdout;

        use reedline::{
            default_emacs_keybindings, EditCommand, Emacs, KeyCode, KeyModifiers, Reedline,
            ReedlineEvent, Signal,
        };

        use self::validator::DiatomValidator;

        use self::highlighter::DiatomHighlighter;

        use self::prompt::DiatomPrompt;

        // Lock std for fast print
        let _ = stdout().lock();
        println!("\nDiatom Console v{}\n", VERSION);

        // Highlight syntax
        let highlighter = Box::new(DiatomHighlighter::default());

        // Replace tab with 4 spaces
        let mut keybindings = default_emacs_keybindings();
        keybindings.add_binding(
            KeyModifiers::NONE,
            KeyCode::Tab,
            ReedlineEvent::Edit(vec![EditCommand::InsertString("    ".to_string())]),
        );
        keybindings.add_binding(
            KeyModifiers::NONE,
            KeyCode::Enter,
            ReedlineEvent::SubmitOrNewline,
        );
        let edit_mode = Box::new(Emacs::new(keybindings));

        // Validator
        let validator = Box::new(DiatomValidator::default());

        let mut line_editor = Reedline::create()
            .with_highlighter(highlighter)
            .with_edit_mode(edit_mode)
            .with_validator(validator);
        let prompt = DiatomPrompt::default();

        loop {
            let sig = line_editor.read_line(&prompt);
            match sig {
                Ok(Signal::Success(buffer)) => {
                    self.parser.parse_str(OsStr::new("stdin"), &buffer);
                    if self.parser.diagnostic_count() > 0 {
                        print!("{}", self.parser.render_diagnoses(self.color));
                        self.parser.clear_diagnoses();
                        let _ = self.parser.get_incremental();
                    } else {
                        println!("{:?}", self.parser.get_incremental().collect::<Vec<_>>());
                    }
                }
                Ok(Signal::CtrlC) => {
                    line_editor.run_edit_commands(&[EditCommand::Clear]);
                }
                Ok(Signal::CtrlD) => {
                    break;
                }
                Err(err) => {
                    println!("Event: {:?}", err);
                    break;
                }
            }
        }
    }
}
