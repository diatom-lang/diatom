use super::*;

macro_rules! test_ok {
    ($code: literal, $output_expected: literal) => {
        let mut interpreter = Interpreter::new(Vec::<u8>::new());
        interpreter
            .exec_repl($code, false)
            .expect("Execution failed!");

        let output = interpreter.replace_buffer(Vec::<u8>::new());
        let mut output = String::from_utf8(output).unwrap();
        // pop newline
        output.pop();
        assert_eq!(output, $output_expected)
    };
}

macro_rules! test_err {
    ($code: literal) => {
        let mut interpreter = Interpreter::new(Vec::<u8>::new());
        let _ = interpreter
            .exec($code, OsStr::new("<test>"), false)
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
    test_ok!("1<1", "false");
    test_ok!("1>1", "false");
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

#[test]
fn test_loop() {
    test_ok!("a = 0 until a > 100 do a = a + 1 end a", "101");
    test_ok!("a = 0 until false do a = 5 break end a", "5");
    test_err!("break");
    test_err!("continue");
}

#[test]
fn test_closure() {
    // fn return () by default
    test_ok!(
        r#"
    f = fn = begin 
        a = 0 
        until a > 0 do
            a = a + 1
        end
    end
    f$()
    "#,
        ""
    );
    // fn capture variable
    test_ok!(
        r#"
    a = 1
    f = fn x = begin 
       a = x
    end
    f$(3)
    a
    "#,
        "3"
    );
    // fn share capture
    test_ok!(
        r#"
    x1 = ()
    x2 = ()
    f = fn = begin
        a = 1
        x1 = fn = a
        x2 = fn x = begin 
            a = x
        end
    end
    f$()
    x2$(10)
    assert$(x1$() == 10)
    "#,
        ""
    );
}
