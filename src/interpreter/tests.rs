use super::*;

macro_rules! test_ok {
    ($code: literal, $output_expected: literal) => {
        let mut interpreter = Interpreter::new();
        let output = interpreter.exec($code, false).expect("Execution failed!");
        assert_eq!(output, $output_expected)
    };
}

macro_rules! test_err {
    ($code: literal) => {
        let mut interpreter = Interpreter::new();
        let _ = interpreter
            .exec($code, false)
            .expect_err("Execution failed!");
    };
}

#[test]
fn test_binary_op() {
    test_ok!("1+2-3*10/5 + (1 + 2.234**3)", "9.149348904");
    test_ok!("8//5", "1");
    test_ok!("8%5", "3");
    test_ok!("true > false", "true");
    test_ok!("true or false", "true");
    test_ok!("1.024 > 1", "true");
    test_ok!(" false == false", "true");
    test_ok!(" false >= false", "true");
    test_ok!("1>=3", "false");
    test_ok!("'abc' > 'abcd'", "false");
    test_ok!("'a'*3", "aaa");
    test_ok!("'a' + 'b'", "ab");

    test_err!("1.5 >= 2.3");
    test_err!("false -1");
}

#[test]
fn test_assignment() {
    test_ok!("a = 5 b = 1 a", "5");
}
