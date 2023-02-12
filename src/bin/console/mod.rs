use std::{
    env,
    ffi::OsStr,
    io::{self, Stdout},
};

use crate::Interpreter;

mod highlighter;
mod prompt;
mod validator;

const VERSION: &str = env!("CARGO_PKG_VERSION");

/// An interactive console for Diatom
pub struct Console {
    interpreter: Interpreter<Stdout>,
    color: bool,
}

impl Console {
    pub fn new(color: bool) -> Self {
        Self {
            interpreter: Interpreter::new(io::stdout()),
            color,
        }
    }

    /// Run this console
    pub fn run(&mut self, inspect: bool) {
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
        println!("Diatom Interactive Console v{VERSION}");

        // Highlight syntax
        let highlighter = Box::<DiatomHighlighter>::default();

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
        let validator = Box::<DiatomValidator>::default();

        let mut line_editor = Reedline::create()
            .with_highlighter(highlighter)
            .with_edit_mode(edit_mode)
            .with_validator(validator);
        let prompt = DiatomPrompt::default();

        loop {
            let sig = line_editor.read_line(&prompt);
            match sig {
                Ok(Signal::Success(buffer)) => {
                    if inspect {
                        match self.interpreter.decompile(
                            buffer,
                            OsStr::new("<interactive>"),
                            self.color,
                        ) {
                            Ok(s) => print!("{s}"),
                            Err(s) => eprint!("{s}"),
                        }
                    } else if let Err(e) = self.interpreter.exec_repl(buffer, self.color) {
                        eprint!("{e}")
                    };
                }
                Ok(Signal::CtrlC) => {
                    line_editor.run_edit_commands(&[EditCommand::Clear]);
                }
                Ok(Signal::CtrlD) => {
                    break;
                }
                Err(err) => {
                    println!("Event: {err:?}");
                    break;
                }
            }
        }
    }
}
