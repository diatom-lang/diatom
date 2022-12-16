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
        let ast = parser.parse(path.as_os_str());
        print!("{}", ast.diagnoser.render(true));
        println!("{:#?}", ast.statements);
        for (path, ast) in parser.modules() {
            println!(
                "{}:\n{:#?}",
                path.to_str().unwrap_or("Path can not be displayed"),
                ast.statements
            );
        }
    } else {
        let mut console = Console::new(true);
        console.run();
    }
}
