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
        parser.print_diagnoses();
        parser.print_diagnoses_summary();
    } else {
        let mut console = Console::new();
        console.run();
    }
}
