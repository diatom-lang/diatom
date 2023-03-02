use super::interpreter::{std_core::LibDummy, Interpreter as __Interpreter};
use std::{fs, path::PathBuf};

type Interpreter<Buffer> = __Interpreter<Buffer, LibDummy>;

#[test]
fn test_import_ok() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("src");
    path.push("tests");
    path.push("resources");
    path.push("import_ok.dm");
    let code = fs::read_to_string(&path).unwrap();
    let mut interpreter = Interpreter::new(vec![]);
    interpreter
        .exec(code, path, false)
        .map_err(|err| println!("{err}"))
        .expect("Example test failed");
}

#[test]
fn test_import_cycle() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("src");
    path.push("tests");
    path.push("resources");
    path.push("import_cycle.dm");
    let code = fs::read_to_string(&path).unwrap();
    let mut interpreter = Interpreter::new(vec![]);
    interpreter
        .exec(code, path, false)
        .expect_err("Example test not failing");
}

#[test]
fn test_import_non_table() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("src");
    path.push("tests");
    path.push("resources");
    path.push("import_non_table.dm");
    let code = fs::read_to_string(&path).unwrap();
    let mut interpreter = Interpreter::new(vec![]);
    interpreter
        .exec(code, path, false)
        .expect_err("Example test not failing");
}
