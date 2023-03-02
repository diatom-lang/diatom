use std::{
    env,
    ffi::OsStr,
    io::Stdout,
    sync::{Arc, Mutex},
};

use diatom::Interpreter;
use diatom::VERSION;

mod highlighter;
mod prompt;
mod validator;

/// An interactive console for Diatom
pub struct Cli {
    interpreter: Arc<Mutex<Interpreter<Stdout>>>,
}

impl Cli {
    pub fn new(interpreter: Interpreter<Stdout>) -> Self {
        Self {
            interpreter: Arc::new(Mutex::new(interpreter)),
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
        let validator = DiatomValidator {
            interpreter: self.interpreter.clone(),
        };

        let mut line_editor = Reedline::create()
            .with_highlighter(highlighter)
            .with_edit_mode(edit_mode)
            .with_validator(Box::new(validator));
        let prompt = DiatomPrompt::default();

        // Enable repl
        let mut locked = self.interpreter.lock().unwrap();
        locked.repl(true);
        if let Ok(work_dir) = env::current_dir() {
            let _ = locked.with_search_path(work_dir);
        }
        std::mem::drop(locked);

        loop {
            let sig = line_editor.read_line(&prompt);
            match sig {
                Ok(Signal::Success(buffer)) => {
                    let mut locked = self.interpreter.lock().unwrap();
                    if inspect {
                        match locked.decompile(buffer, OsStr::new("<interactive>"), true) {
                            Ok(s) => print!("{s}"),
                            Err(s) => eprint!("{s}"),
                        }
                    } else if let Err(e) = locked.exec(buffer, OsStr::new("<interactive>"), true) {
                        eprint!("{e}")
                    };
                    std::mem::drop(locked);
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
