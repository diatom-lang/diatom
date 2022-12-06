use diatom::{Console, Parser};
use std::path::PathBuf;

use clap::Parser as ArgParser;

#[derive(ArgParser)]
#[command(version)]
#[command(about = "The diatom interpreter and compiler")]
struct Args {
    /// Path to target file
    file: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();

    if let Some(path) = args.file {
        let mut parser = Parser::new();
        parser.parse(path.as_os_str());
        print!("{}", parser.render_diagnoses(true));
    } else {
        let mut console = Console::new(true);
        console.run();
    }
}
