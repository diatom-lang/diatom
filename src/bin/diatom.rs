use crossterm::tty::IsTty;
use diatom::Interpreter;
use std::{fs, io, path::PathBuf};

use clap::{ColorChoice, Parser};

#[cfg(feature = "console")]
mod console;
#[cfg(feature = "console")]
pub use console::Console;

#[derive(Parser)]
#[command(name = "Diatom Interpreter")]
#[command(author = "Terence Ng")]
#[command(version)]
#[command(help_template = "\
{name} v{version} by {author-with-newline}
{usage-heading} {usage}

{all-args}{after-help}
")]
struct Args {
    #[arg(long, default_value_t = ColorChoice::Auto)]
    color: ColorChoice,
    #[arg(short, long)]
    /// Show decompiled bytecode instead of execution
    inspect: bool,
    /// File to be executed, using REPL mode if leaving empty
    path: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();

    let color = match args.color {
        ColorChoice::Auto => io::stdout().is_tty(),
        ColorChoice::Always => true,
        ColorChoice::Never => false,
    };
    let mut interpreter = if color {
        Interpreter::with_color(io::stdout())
    } else {
        Interpreter::new(io::stdout())
    };

    match (&args.path, args.inspect) {
        #[cfg(feature = "console")]
        (None, inspect) => {
            let mut console = Console::new(interpreter);
            console.run(inspect);
        }
        #[cfg(not(feature = "console"))]
        (None, _) => {
            eprintln!("Error: Diatom is not compiled with console enabled")
        }
        (Some(path), false) => {
            let code = fs::read_to_string(path).expect("Error: File can not be read!");
            match interpreter.exec(code, path.as_os_str()) {
                Ok(_) => (),
                Err(s) => print!("{s}"),
            };
        }
        (Some(path), true) => {
            let code = fs::read_to_string(path).expect("Error: File can not be read!");
            let result = match interpreter.decompile(code, path.as_os_str()) {
                Ok(s) | Err(s) => s,
            };
            print!("{result}");
        }
    }
}
