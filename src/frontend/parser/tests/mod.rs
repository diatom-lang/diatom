use super::*;

fn test_str(code: &str, should_fail: bool) {
    let mut resource_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    resource_path.push("src/frontend/parser/tests/resources");
    let mut parser = Parser::new().with_path(resource_path);
    let ast = parser.parse_str(OsStr::new(file!()), code);
    println!("{:#?}", ast.statements);
    if !should_fail && ast.diagnoser.count() > 0 {
        print!("{}", ast.diagnoser.render(true));
    }
    if should_fail {
        assert!(ast.diagnoser.count() > 0);
    } else {
        assert!(ast.diagnoser.count() == 0);
    }
}

#[test]
fn test_expr_postfix_ambiguous() {
    let code = "0,a $ (1,2,3) (3,4)$[2-1]+0.333//[0 1 2]$[1][v]";
    let mut parser = Parser::new();
    let ast = parser.parse_str(OsStr::new(file!()), code);
    println!("{:#?}", ast.statements);
    if ast.diagnoser.count() > 0 {
        print!("{}", ast.diagnoser.render(true));
    }
    assert_eq!(ast.diagnoser.count(), 0);
    assert_eq!(ast.statements.len(), 3);
}

#[test]
fn test_valid() {
    test_str("a$()", false);
    test_str("a$(1 2 [2 2])", false);
    test_str("[]", false);
    test_str("", false);
}

#[test]
fn test_invalid() {
    test_str(">> <<", true);
    test_str("a$[]", true);
    test_str("[1 2,]", true);
}

#[test]
fn test_if() {
    test_str(
        "if a then b elsif c then 0.92 a = 0 b$[0,1,2] else end",
        false,
    );
    test_str("if a then else nil end", false);
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
    test_str("def a() a+1", true);
    test_str("def () a+1 end", true);
    test_str("def a(a) = a+1 end", false);
    test_str("def x (a b c) = a+b+1 fn x = x end", false);
    test_str("def x () = g$() where end", false);
    test_str("def f() = g$(1) where g = fn x = f$() end", false);
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
    test_str("begin return [1 2 3] return fn = [] end", false);
    test_str("begin break continue end", false);
    test_str("if false then return else end", false);
    test_str(
        "if false then return [1 2 3] return fn = [] else end",
        false,
    );
    test_str("if false then break continue else end", false);
    test_str("return", false);
    test_str("def x () = return 1 end", false);
    test_str("def x () = return end ", false);
    test_str("fn x = x <= 1", false);
}

#[test]
fn test_data() {
    test_str("data  = X def new() end", true);
    test_str("data Maybe = end", true);
    test_str("data Maybe = | X end", true);
    test_str("data Maybe = X | end", true);
    test_str("data Maybe = Just a | Nothing end", false);
    test_str("data Maybe = Just a end", false);
    test_str("data Maybe = Nothing end", false);
    test_str(
        "data Maybe = Nothing def new() = x end def map(f) = f$(x) end end",
        false,
    );
    test_str("data Shape = Circle r | Rect x y | Tri a b c end", false);
    test_str("Just${1} Just${x: 1}", false);
}

#[test]
fn test_loop() {
    test_str("loop end", false);
    test_str("loop continue false true return end", false);
    test_str("until end", true);
    test_str("until iii end", true);
    test_str("until true do end", false);
    test_str("until true do nil end", false);
    test_str("until [1 2 3] do xxx yyy break end", false);
}

#[test]
fn test_dict_set() {
    test_str("{:}", false);
    test_str("{}", false);
    test_str("{ a }", false);
    test_str("{123 567 nil}", false);
    test_str("{123:1 567:2 'asdf': 7+8}", false);
    test_str("{break}", true);
    test_str("{c:break}", true);
    test_str("{c:1 b:3<=2 d}", true);
    test_str("{c:1 b:3<=2 }", false);
}

#[test]
fn test_case() {
    test_str(
        r#"
        case i of
            1 | 2,3,4| y@x@Just${_ g@Nothing} => break
            true => 1
            std.io.error${s} => s
            a@std.io.error${s} if m>= 0 => s
            _ if t == "s" => "what ever"
            "str", 1 | false | 1e4 => panic$()
            (1,2), (3,4) | 0 if false => 1
        end"#,
        false,
    );
    test_str(
        r#"
        case i of
            | x => 1
        end"#,
        true,
    );
    test_str(
        r#"
        case i of
            1 $ { c } => 1
        end"#,
        true,
    );
    test_str(
        r#"
        case i of
            1 @ 1 => 1
        end"#,
        true,
    );
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
