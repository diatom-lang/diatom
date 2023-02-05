use diatom::{Console, Interpreter};
use std::{fs, path::PathBuf};

use clap::Parser;

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
    #[arg(long)]
    /// Disable colored output
    no_color: bool,
    #[arg(short, long)]
    /// Show decompiled bytecode instead of execution
    inspect: bool,
    /// File to be executed, using REPL mode if leaving empty
    path: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();

    match (&args.path, args.inspect) {
        (None, inspect) => {
            let mut console = Console::new(!args.no_color);
            console.run(inspect);
        }
        (Some(path), false) => {
            let code = fs::read_to_string(path).expect("Error: File can not be read!");
            let mut interpreter = Interpreter::new();
            let result = match interpreter.exec(code, path.as_os_str(), !args.no_color) {
                Ok(s) | Err(s) => s,
            };
            print!("{result}");
        }
        (Some(path), true) => {
            let code = fs::read_to_string(path).expect("Error: File can not be read!");
            let mut interpreter = Interpreter::new();
            let result = match interpreter.decompile(code, path.as_os_str(), !args.no_color) {
                Ok(s) | Err(s) => s,
            };
            print!("{result}");
        }
    }
}
