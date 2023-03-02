use super::*;

fn test_str(code: &str, should_fail: bool) {
    let mut file_manager = FileManager::new();
    let paths = vec![];
    let mut parser = Parser::new(&mut file_manager, &paths);
    let _ = parser.parse_file("test", code);
    if !should_fail && file_manager.error_count() > 0 {
        print!("{}", file_manager.render(true));
    }
    if should_fail {
        assert!(file_manager.error_count() > 0);
    } else {
        assert!(file_manager.error_count() == 0);
    }
}

#[test]
fn test_expr_postfix_ambiguous() {
    let code = "0,a(1,2,3); (3,4)[2-1]+0.333//[0, 1, 2][1]; [v]";
    let mut file_manager = FileManager::new();
    let paths = vec![];
    let mut parser = Parser::new(&mut file_manager, &paths);
    let fid = parser.parse_file("test", code);
    if file_manager.error_count() > 0 {
        print!("{}", file_manager.render(true));
    }
    assert_eq!(file_manager.error_count(), 0);
    assert_eq!(file_manager.get_ast(fid).len(), 3);
}

#[test]
fn test_valid() {
    test_str("a()", false);
    test_str("a(1, 2, [2, 2])", false);
    test_str("[]", false);
    test_str("", false);
    test_str("(1..) [2.., 1..]", false);
    test_str("(1..)", false);
    test_str("def f = 1.. end", false);
    test_str(";;a+1; def a = fn =1; end;;;", false);
}

#[test]
fn test_invalid() {
    test_str(">> <<", true);
    test_str("a[]", true);
    test_str("[1 2,]", true);
    test_str("[1 2]", true);
    test_str("a([1 2], [])", true);
}

#[test]
fn test_if() {
    test_str(
        "if a then b elsif c then 0.92 a = 0 b[0,1,2] else end",
        false,
    );
    test_str("if a then else () end", false);
    test_str("if a then else end", false);
    test_str("if a then c end", false);
    test_str("if a then end", false);
    test_str("if a then elsif c then end", false);
    test_str("if a else end", true);
    test_str("if a elsif b else end", true);
}

#[test]
fn test_def() {
    test_str("def a a+1 end", true);
    test_str("def a a+1", true);
    test_str("def a+1 end", true);
    test_str("def a a = a+1 end", false);
    test_str("def x a b c = a+b+1 fn x = x end", false);
    test_str("def x.a a b c = a+b+1 fn x = x end", false);
    test_str("def x::a a b c = a+b+1 fn x = x end", false);
}

#[test]
fn test_fn() {
    test_str("fn = 1", false);
    test_str("fn x y z= x + y + z", false);
    test_str("fn _ = 1", false);
    test_str("fn x = begin x = x + 1 1 end", false);
}

#[test]
fn test_statement() {
    test_str("begin return end", false);
    test_str("begin return [1, 2, 3] return fn = [] end", false);
    test_str("begin break continue end", false);
    test_str("if false then return else end", false);
    test_str(
        "if false then return [1, 2, 3] return fn = [] else end",
        false,
    );
    test_str("if false then break continue else end", false);
    test_str("return", false);
    test_str("def x = return 1 end", false);
    test_str("def x = return end ", false);
    test_str("fn x = x <= 1", false);
}

#[test]
fn test_loop() {
    test_str("loop end", false);
    test_str("loop continue false true return end", false);
    test_str("until end", true);
    test_str("until iii end", true);
    test_str("until true do end", false);
    test_str("until true do () end", false);
    test_str("until [1, 2, 3] do xxx yyy break end", false);
}

#[test]
fn test_table() {
    test_str("{}", false);
    test_str("{ a = 1, b= 3, c= 'abc'}", false);
    test_str("{loop = 1}", true);
}
