use include_dir::{include_dir, Dir};

use crate::Interpreter;

static EXAMPLES: Dir = include_dir!("$CARGO_MANIFEST_DIR/examples");

#[test]
fn test_examples() {
    for example in EXAMPLES.find("*.dm").unwrap() {
        let example = example.as_file().unwrap();
        let code = example.contents_utf8().unwrap();
        let mut interpreter = Interpreter::new(vec![]);
        interpreter
            .exec(code, example.path().as_os_str(), false)
            .expect("Example test failed");
    }
}
