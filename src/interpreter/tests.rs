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

macro_rules! test_ok_ignore {
    ($code: literal) => {
        let mut interpreter = Interpreter::new(Vec::<u8>::new());
        interpreter
            .exec_repl($code, false)
            .expect("Execution failed!");

        let output = interpreter.replace_buffer(Vec::<u8>::new());
        let mut output = String::from_utf8(output).unwrap();
        // pop newline
        output.pop();
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

#[test]
fn test_if() {
    test_ok!(
        r#"
    x = 0
    if x > 0 then
        1
    elsif x < 0 then
        2
    else 
        3
    end
    "#,
        "3"
    );
}

#[test]
fn test_table() {
    test_ok!("{}", "{}");
    test_ok!("{a = 1}", "{a = 1}");
    test_ok!("x = {a = 1} x.a = 100 x.a", "100");
    test_ok!("x = {a = {i = 1}} x.a.i = 'Hello' x.a.i", "Hello");
    test_err!("{a = 1, a = 1}");
    test_err!("a.b");
    test_err!("a.'hello'");
}

#[test]
fn test_recursive() {
    test_ok!(
        r#"
        def fib n = 
            if n <= 1 then
                n
            else
                fib$(n-1) + fib$(n-2)
            end
        end
        fib$(10)
    "#,
        "55"
    );
}

#[test]
fn test_compile_with_target() {
    test_ok!("def add a b = a + b end add$(add$(1,2), begin 3 end)", "6");
}

#[test]
fn test_tuple() {
    test_ok!("a = (1,2,3) a.2", "3");
    test_ok!("a = (1,2, {}) a.2.idx='hello' b = a.2 b.idx", "hello");
}

#[test]
fn test_method() {
    test_ok!("a = {println = println} a::println$(1)", "1");
    test_ok_ignore!("a = {println = println} a.println$(1)");
    test_ok!("a = (1, println) a.1$(1)", "1");
}

#[test]
fn test_meta_table() {
    test_ok_ignore!(
        r#"
        meta_table = {name = 'abc'}
        table = {} <- meta_table
        assert$(table.name == 'abc')
    "#
    );
}

#[test]
fn test_list() {
    test_ok!("a = [1,2,3] a$[0]", "1");
    test_ok!("a = [1,2,3] a$[2]", "3");
    test_ok!("a = [1,2,3] a$[-1]", "3");
    test_ok!("a = [1,2,3] a$[-3]", "1");
    test_err!("a = [1,2,3] a$[-4]");
    test_err!("a = [1,2,3] a$[3]");
    test_err!("a = [] a$[0]");
}
