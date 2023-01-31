use diatom::{Console, Interpreter};
use std::{fs, path::PathBuf};

use clap::Parser as ArgParser;

#[derive(ArgParser)]
#[command(version)]
#[command(about = "The diatom interpreter and compiler")]
struct Args {
    /// Path to target file
    file: Option<PathBuf>,
    /// Print decompiled bytecode and immediately exit
    #[arg(short, long)]
    inspect: bool,
}

fn main() {
    let args = Args::parse();

    if let Some(path) = args.file {
        let code = fs::read_to_string(path).unwrap();
        let mut interpreter = Interpreter::new();
        let result = if args.inspect {
            match interpreter.decompile(code, true) {
                Ok(s) | Err(s) => s,
            }
        } else {
            match interpreter.exec(code, true) {
                Ok(s) | Err(s) => s,
            }
        };
        print!("{result}");
    } else {
        let mut console = Console::new(true);
        console.run();
    }
}
