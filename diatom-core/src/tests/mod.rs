use std::{fs, path::PathBuf};

use crate::Interpreter;

#[test]
fn test_examples() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let mut path = path.parent().unwrap().to_path_buf();
    path.push("examples");
    let dir = fs::read_dir(path).unwrap();
    dir.for_each(|entry| {
        let path = entry.unwrap().path();
        if !path.is_file() {
            return;
        }
        let code = fs::read_to_string(&path).unwrap();
        let mut interpreter = Interpreter::new(vec![]);
        interpreter
            .exec(&code, &path, false)
            .map_err(|err| println!("{err}"))
            .expect("Example test failed");
        let mut interpreter = Interpreter::new(vec![]);
        interpreter
            .decompile(&code, &path, false)
            .map_err(|err| println!("{err}"))
            .expect("Example test failed");
    });
}

#[test]
fn test_overflow() {
    let mut interpreter = Interpreter::new(vec![]);
    interpreter
        .exec("Int::MIN.abs()", "test", true)
        .map_err(|err| println!("{err}"))
        .expect("Test failed");
}

#[test]
fn test_for_macro() {
    let mut interpreter = Interpreter::new(vec![]);
    interpreter
        .exec("fn = begin for i in 1..5 do end end", "test", true)
        .map_err(|err| println!("{err}"))
        .expect("Test failed");
}

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
