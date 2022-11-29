use std::{
    ffi::OsStr,
    io::{self, Stdin, Write},
};

use diatom::Parser;

const VERSION: &str = env!("CARGO_PKG_VERSION");

pub struct Console {
    parser: Parser,
    stdin: Stdin,
}

impl Console {
    pub fn new() -> Self {
        Self {
            parser: Parser::new(),
            stdin: io::stdin(),
        }
    }

    pub fn run(&mut self) {
        println!("\nDiatom Console v{}\n", VERSION);
        let mut line = String::new();
        loop {
            print!("diatom > ");
            io::stdout().flush().unwrap();
            let result = self.stdin.read_line(&mut line);
            match result {
                Ok(0) => {
                    println!("");
                    return;
                }
                Ok(_) => {
                    self.parser.parse_str(OsStr::new("stdin"), &line);
                    if self.parser.diagnostic_count() > 0 {
                        self.parser.print_diagnoses();
                        self.parser.clear_diagnoses();
                    }
                    line.clear();
                }
                Err(err) => {
                    println!("IoError: {:?}", err);
                    return;
                }
            }
        }
    }
}

fn main() {
    let mut console = Console::new();
    console.run();
}
