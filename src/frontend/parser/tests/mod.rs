use super::*;

fn test_str(code: &str, should_fail: bool) {
    let mut resource_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    resource_path.push("src/frontend/parser/tests/resources");
    let mut parser = Parser::new()._with_path(resource_path);
    let ast = parser.parse_str(OsStr::new(file!()), code);
    if !should_fail && ast.diagnoser.error_count() > 0 {
        print!("{}", ast.diagnoser.render(true));
    }
    if should_fail {
        assert!(ast.diagnoser.error_count() > 0);
    } else {
        assert!(ast.diagnoser.error_count() == 0);
    }
}

#[test]
fn test_expr_postfix_ambiguous() {
    let code = "0,a $ (1,2,3) (3,4)$[2-1]+0.333//[0, 1, 2]$[1][v]";
    let mut parser = Parser::new();
    let ast = parser.parse_str(OsStr::new(file!()), code);
    if ast.diagnoser.error_count() > 0 {
        print!("{}", ast.diagnoser.render(true));
    }
    assert_eq!(ast.diagnoser.error_count(), 0);
    assert_eq!(ast.statements.len(), 3);
}

#[test]
fn test_valid() {
    test_str("a$()", false);
    test_str("a$(1, 2, [2, 2])", false);
    test_str("[]", false);
    test_str("", false);
}

#[test]
fn test_invalid() {
    test_str(">> <<", true);
    test_str("a$[]", true);
    test_str("[1 2,]", true);
    test_str("[1 2]", true);
}

#[test]
fn test_if() {
    test_str(
        "if a then b elsif c then 0.92 a = 0 b$[0,1,2] else end",
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
    test_str("{ a = 1 b= 3 c= 'abc'}", false);
    test_str("{loop = 1}", true);
}

#[test]
fn test_require() {
    test_str("require 'a+'", true);
    test_str("require 'c'", true);
    test_str("require '\\'", true);
    test_str("require 'asdfase'", true);
    test_str("require 'a'", false);
    test_str("require 'a-'", false);
    test_str("require 'b.b1'", false);
    test_str("require 'b'", false);
    test_str("require 'circular'", false);
}
