pub mod ast;
mod error;
#[cfg(test)]
mod tests;

use crate::diagnostic::{DisplayableOsString, Loc, SharedFile};

use self::error::{to_diagnostic, ErrorCode};

use super::{
    lexer::{Keyword, Operator, Token},
    util::TokenIterator,
    Lexer,
};
use ahash::AHashMap;
use ast::{
    Ast, Const, ConstPattern, Expr, Expr_, OpInfix, OpPostfix, OpPrefix, Pattern, Pattern_, Stmt,
    Stmt_,
};
use lazy_static::lazy_static;
use regex::Regex;
use std::{
    ffi::{OsStr, OsString},
    fs,
    mem::Discriminant,
    path::{Path, PathBuf},
};

const fn precedence_infix(op: OpInfix) -> (u16, u16) {
    use OpInfix::*;
    match op {
        Assign => (2, 1),
        Comma => (3, 4),
        Range => (5, 6),
        Or => (7, 8),
        And => (9, 10),
        Eq | Ne | Le | Lt | Gt | Ge => (11, 12),
        Plus | Minus => (13, 14),
        Mul | Div | DivFloor | Mod => (15, 16),
        Exp => (17, 18),
        Member => (21, 22),
    }
}

const fn precedence_prefix() -> u16 {
    19
}

const fn precedence_postfix() -> u16 {
    20
}

/// A pattern match all possible start of an expression
macro_rules! expr_start_pattern {
    () => {
        Token::Key(
            Keyword::Fn
                | Keyword::Require
                | Keyword::Case
                | Keyword::If
                | Keyword::Begin
                | Keyword::Nil
                | Keyword::True
                | Keyword::False,
        ) | Token::Op(_)
            | Token::Id(_)
            | Token::Integer(_)
            | Token::Float(_)
            | Token::Str(_)
    };
}

/// The parser for Diatom.
///
/// # Errors
/// Error code `E1000` to `E1999` is reserved for `Parser`
///
/// # Example
///
/// ```rust, no_run
/// use diatom::Parser;
/// use std::ffi::OsStr;
///
/// // Create a new parser
/// let mut parser = Parser::default();
///
/// // Parse a File
/// let ast = parser.parse(OsStr::new("code.dm"));
/// // Parse a string
/// let ast = parser.parse_str(OsStr::new("Custom name"), "a = 1 b = 2");
///
/// // Print diagnostics with color
/// println!("{}", ast.diagnoser.render(true));
/// ```
///
pub struct Parser {
    search_path: Vec<PathBuf>,
    modules: AHashMap<OsString, Ast>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            search_path: vec![],
            modules: AHashMap::new(),
        }
    }

    /// add module search path
    pub fn with_path(mut self, path: PathBuf) -> Self {
        self.search_path.push(path);
        self
    }

    /// Get all loaded modules
    pub fn modules(&self) -> &AHashMap<OsString, Ast> {
        &self.modules
    }

    /// Parse a file.
    ///
    /// Return true if errors are encountered.
    pub fn parse(&mut self, filepath: &OsStr) -> Ast {
        let path = fs::canonicalize(filepath);
        let path = match path {
            Ok(path) => path,
            Err(err) => {
                let mut ast = Ast::new(OsString::new(), SharedFile::from_str(""));
                ast.add_diagnostic(to_diagnostic(
                    ErrorCode::NoSuchFile(DisplayableOsString::from(filepath), err),
                    0..0,
                ));
                return ast;
            }
        };
        let path_str = path.as_os_str();
        let content = SharedFile::new(path_str);
        let content = match content {
            Ok(content) => content,
            Err(err) => {
                let mut ast = Ast::new(OsString::new(), SharedFile::from_str(""));
                ast.add_diagnostic(to_diagnostic(
                    ErrorCode::NoSuchFile(DisplayableOsString::from(path_str), err),
                    0..0,
                ));
                return ast;
            }
        };
        self.parse_file(path_str, content)
    }

    /// Parse a string and append to current parse tree
    ///
    /// This function does not modify search path
    pub fn parse_str(&mut self, path: &OsStr, content: &str) -> Ast {
        let content = SharedFile::from_str(content);
        let mut ast = Ast::new(path.to_os_string(), content);
        let token_stream = Lexer::lex(&mut ast);
        let mut iter = token_stream.iter();

        while iter.peek().is_some() {
            let stmt = self.consume_stmt(&mut iter, &mut ast, None);
            ast.statements.push(stmt);
        }
        ast
    }

    /// Parse a file and temporarily add its root to search path
    ///
    /// Note that this function assumes the path is a valid path to the file
    fn parse_file(&mut self, path: &OsStr, content: SharedFile) -> Ast {
        let mut ast = Ast::new(path.to_os_string(), content);
        let token_stream = Lexer::lex(&mut ast);
        let mut iter = token_stream.iter();
        self.search_path.insert(
            0,
            Path::new(path)
                .parent()
                .unwrap_or_else(|| {
                    panic!(
                        "Internal error: Call parse file without a valid path! path: {}",
                        path.to_str().unwrap_or("Can not display")
                    )
                })
                .to_path_buf(),
        );

        while iter.peek().is_some() {
            let stmt = self.consume_stmt(&mut iter, &mut ast, None);
            ast.statements.push(stmt);
        }
        self.search_path.remove(0);
        ast
    }

    fn consume_require(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Expr {
        use Token::*;
        iter.next();
        let start = iter.loc();
        let mod_paths = if let Some(Str(s)) = iter.peek() {
            let s = s.clone();
            iter.next();
            lazy_static! {
                static ref RE: Regex = Regex::new("^[\\.\\-0-9a-zA-Z_]*$").unwrap();
            }
            let s_split = s.split('.').map(|x| x.to_string()).collect::<Vec<_>>();
            if !RE.is_match(&s) || s_split.is_empty() || s.starts_with('.') | s.ends_with('.') {
                ast.add_diagnostic(to_diagnostic(ErrorCode::InvalidModuleString, iter.loc()));
                return Expr {
                    loc: iter.loc(),
                    val: Expr_::Error,
                };
            };
            s_split
        } else {
            ast.add_diagnostic(to_diagnostic(
                ErrorCode::RequireWrongArgument,
                iter.next_loc(),
            ));
            return Expr {
                loc: iter.loc(),
                val: Expr_::Error,
            };
        };
        // search for file
        for mut path in self.search_path.clone() {
            mod_paths.iter().for_each(|s| path.push(s));
            fn check_path(path: &Path) -> Option<SharedFile> {
                let path = match fs::canonicalize(path) {
                    Ok(path) => path,
                    Err(_) => return None,
                };

                SharedFile::new(path.as_os_str()).ok()
            }

            let mut file;
            let mut file_path = path.clone();
            file_path.set_extension("dm");
            file = check_path(file_path.as_path());

            if file.is_none() {
                path.push("mod.dm");
                file = check_path(path.as_path());
            } else {
                path = file_path;
            }

            let content = match file {
                Some(file) => file,
                None => continue,
            };

            // Check if module is cached
            if self.modules.get(path.as_os_str()).is_none() {
                // Prevent infinite recursion
                self.modules.insert(
                    path.as_os_str().to_os_string(),
                    Ast::new(OsString::new(), SharedFile::from_str("")),
                );
                let mod_ast = self.parse_file(path.as_os_str(), content);
                if mod_ast.diagnoser.error_count() > 0 {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::InvalidModule, iter.loc()));
                    self.modules.remove(path.as_os_str());
                } else {
                    self.modules
                        .insert(path.as_os_str().to_os_string(), mod_ast);
                }
            }

            return Expr {
                loc: start.start..iter.loc().end,
                val: Expr_::Module(DisplayableOsString::new(path.as_os_str().to_os_string())),
            };
        }

        ast.add_diagnostic(to_diagnostic(
            ErrorCode::ModuleNotFound(
                mod_paths
                    .last()
                    .expect("At least one should exists")
                    .clone(),
            ),
            iter.loc(),
        ));

        Expr {
            loc: iter.loc(),
            val: Expr_::Error,
        }
    }

    fn consume_stmt(
        &mut self,
        iter: &mut TokenIterator,
        ast: &mut Ast,
        not_take_on_error: Option<Token>,
    ) -> Stmt {
        use Keyword::*;
        use Token::*;
        let start = iter.next_loc();
        match iter.peek() {
            Some(Key(Break)) => {
                iter.next();
                Stmt {
                    loc: start,
                    val: Stmt_::Break,
                }
            }
            Some(Key(Continue)) => {
                iter.next();
                Stmt {
                    loc: start,
                    val: Stmt_::Break,
                }
            }
            Some(Key(Return)) => {
                iter.next();
                match iter.peek() {
                    Some(expr_start_pattern!()) => {
                        let expr = self.consume_expr(iter, ast, 0, not_take_on_error);
                        let end = iter.loc();
                        Stmt {
                            loc: start.start..end.end,
                            val: Stmt_::Return(Some(expr)),
                        }
                    }
                    _ => Stmt {
                        loc: start,
                        val: Stmt_::Return(None),
                    },
                }
            }
            Some(Key(Def)) => self.consume_def(iter, ast),
            Some(expr_start_pattern!()) => {
                let expr = self.consume_expr(iter, ast, 0, not_take_on_error);
                let end = iter.loc();
                Stmt {
                    loc: start.start..end.end,
                    val: Stmt_::Expr(expr),
                }
            }
            Some(Key(Data)) => self.consume_data(iter, ast),
            Some(Key(Loop | Until)) => self.consume_loop(iter, ast),
            Some(Key(For)) => self.consume_for(iter, ast),
            Some(token) => {
                let token = token.clone();
                iter.next();
                ast.add_diagnostic(to_diagnostic(
                    ErrorCode::UnexpectedToken(Some(token), None, None),
                    start.clone(),
                ));
                Stmt {
                    loc: start,
                    val: Stmt_::Error,
                }
            }
            None => {
                ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, start.clone()));
                Stmt {
                    loc: start,
                    val: Stmt_::Error,
                }
            }
        }
    }

    /// Consume an iterator to an expected operator or EOF
    /// Errors are written to `self.diagnoser`
    /// Return true if eof met otherwise false
    fn consume_to_op(
        &mut self,
        iter: &mut TokenIterator,
        ast: &mut Ast,
        expected: Operator,
        previous: Option<(Token, Loc)>,
    ) -> bool {
        fn test_match(op_type: Discriminant<Operator>, iter: &TokenIterator) -> bool {
            if let Some(Token::Op(op)) = iter.peek() {
                if op_type == std::mem::discriminant(op) {
                    return true;
                }
            }
            false
        }
        let op_type = std::mem::discriminant(&expected);

        if test_match(op_type, iter) {
            iter.next();
            return false;
        } else {
            let t = iter.next().cloned();
            let loc_now = iter.loc();
            ast.add_diagnostic(to_diagnostic(
                ErrorCode::UnexpectedToken(t, Some(Token::Op(expected)), previous),
                loc_now,
            ));
        }
        loop {
            if test_match(op_type, iter) {
                iter.next();
                return false;
            }
            match iter.next() {
                Some(_) => (),
                None => {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                    return true;
                }
            }
        }
    }

    /// Consume an iterator to an expected keyword or EOF
    /// Errors are written to `self.diagnoser`
    /// Return true if eof met otherwise false
    fn consume_to_key(
        &mut self,
        iter: &mut TokenIterator,
        ast: &mut Ast,
        expected: Keyword,
        previous: Option<(Token, Loc)>,
    ) -> bool {
        fn test_match(key_type: Discriminant<Keyword>, iter: &TokenIterator) -> bool {
            if let Some(Token::Key(k)) = iter.peek() {
                if key_type == std::mem::discriminant(k) {
                    return true;
                }
            }
            false
        }
        let key_type = std::mem::discriminant(&expected);

        if test_match(key_type, iter) {
            iter.next();
            return false;
        } else {
            let t = iter.next().cloned();
            let loc_now = iter.loc();
            ast.add_diagnostic(to_diagnostic(
                ErrorCode::UnexpectedToken(t, Some(Token::Key(expected)), previous),
                loc_now,
            ));
        }
        loop {
            if test_match(key_type, iter) {
                iter.next();
                return false;
            }
            match iter.next() {
                Some(_) => (),
                None => {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                    return true;
                }
            }
        }
    }

    fn consume_cond_then(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Option<Expr> {
        use Keyword::*;
        use Token::*;
        let start = iter.loc();
        // match `condition`
        let condition = self.consume_expr(iter, ast, 0, Some(Key(Then)));
        // match `then`
        if !self.consume_to_key(iter, ast, Then, Some((Key(If), start))) {
            Some(condition)
        } else {
            None
        }
    }

    fn consume_if(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Expr {
        use Keyword::*;
        use Token::*;
        iter.next();
        let start = iter.loc();
        let mut exprs: Vec<Expr> = vec![];
        match self.consume_cond_then(iter, ast) {
            Some(expr) => exprs.push(expr),
            None => {
                let end = iter.loc();
                return Expr {
                    loc: start.start..end.end,
                    val: Expr_::Error,
                };
            }
        }
        // match block
        let mut block: Vec<Stmt> = vec![];
        let mut block_start = iter.next_loc();
        loop {
            match iter.peek() {
                Some(Key(Elsif)) => {
                    exprs.push(Expr {
                        loc: block_start.start..iter.loc().end,
                        val: Expr_::Block(block),
                    });
                    block = vec![];
                    iter.next();
                    match self.consume_cond_then(iter, ast) {
                        Some(expr) => {
                            exprs.push(expr);
                        }
                        None => {
                            let end = iter.loc();
                            return Expr {
                                loc: start.start..end.end,
                                val: Expr_::Error,
                            };
                        }
                    }
                    block_start = iter.next_loc();
                }
                Some(Key(Else)) => {
                    exprs.push(Expr {
                        loc: block_start.start..iter.loc().end,
                        val: Expr_::Block(block),
                    });
                    block = vec![];
                    iter.next();
                    loop {
                        match iter.peek() {
                            Some(Key(End)) => {
                                exprs.push(Expr {
                                    loc: block_start.start..iter.loc().end,
                                    val: Expr_::Block(block),
                                });
                                iter.next();
                                let end = iter.loc();
                                return Expr {
                                    loc: start.start..end.end,
                                    val: Expr_::If(exprs),
                                };
                            }
                            Some(_) => {
                                let stmt = self.consume_stmt(iter, ast, Some(Key(End)));
                                block.push(stmt);
                            }
                            None => {
                                let end = iter.loc();
                                ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, end));
                                return Expr {
                                    loc: 0..0,
                                    val: Expr_::Error,
                                };
                            }
                        }
                    }
                }
                Some(Key(End)) => {
                    exprs.push(Expr {
                        loc: block_start.start..iter.loc().end,
                        val: Expr_::Block(block),
                    });
                    iter.next();
                    let end = iter.loc();
                    return Expr {
                        loc: start.start..end.end,
                        val: Expr_::If(exprs),
                    };
                }
                Some(_) => {
                    let stmt = self.consume_stmt(iter, ast, Some(Key(Else)));
                    block.push(stmt);
                }
                None => {
                    let end = iter.loc();
                    ast.add_diagnostic(to_diagnostic(
                        ErrorCode::UnexpectedToken(None, Some(Key(Else)), Some((Key(If), start))),
                        end,
                    ));
                    return Expr {
                        loc: 0..0,
                        val: Expr_::Error,
                    };
                }
            }
        }
    }

    fn consume_for(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Stmt {
        use Keyword::*;
        use Token::*;
        iter.next();
        let start = iter.loc();
        let vars = self.consume_expr(iter, ast, 0, Some(Key(In)));
        // Consume "in"
        if self.consume_to_key(iter, ast, In, Some((Key(For), start.clone()))) {
            return Stmt {
                loc: start.start..iter.loc().end,
                val: Stmt_::Error,
            };
        };
        let iterator = self.consume_expr(iter, ast, 0, Some(Key(Do)));
        if self.consume_to_key(iter, ast, Do, Some((Key(For), start.clone()))) {
            return Stmt {
                loc: start.start..iter.loc().end,
                val: Stmt_::Error,
            };
        };
        let mut stmts = vec![];
        loop {
            match iter.peek() {
                Some(Key(End)) => {
                    iter.next();
                    return Stmt {
                        loc: start.start..iter.loc().end,
                        val: Stmt_::For(Box::new(vars), Box::new(iterator), stmts),
                    };
                }
                Some(_) => stmts.push(self.consume_stmt(iter, ast, Some(Key(End)))),
                None => {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                    return Stmt {
                        loc: start.start..iter.loc().end,
                        val: Stmt_::Error,
                    };
                }
            }
        }
    }

    fn consume_loop(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Stmt {
        use Keyword::*;
        use Token::*;
        let key = iter.next().cloned();
        let start = iter.loc();
        let cond = match key {
            Some(Key(Loop)) => None,
            Some(Key(Until)) => {
                let stmt = self.consume_expr(iter, ast, 0, None);
                if matches!(iter.peek(), None) {
                    return Stmt {
                        loc: start.start..iter.loc().end,
                        val: Stmt_::Error,
                    };
                };
                self.consume_to_key(iter, ast, Do, Some((Key(Until), start.clone())));
                Some(stmt)
            }
            _ => unreachable!(),
        };
        let mut stmts = vec![];
        loop {
            match iter.peek() {
                Some(Key(End)) => {
                    iter.next();
                    return Stmt {
                        loc: start.start..iter.loc().end,
                        val: Stmt_::Loop(cond, stmts),
                    };
                }
                Some(_) => stmts.push(self.consume_stmt(iter, ast, Some(Key(End)))),
                None => {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                    return Stmt {
                        loc: start.start..iter.loc().end,
                        val: Stmt_::Error,
                    };
                }
            }
        }
    }

    fn consume_data(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Stmt {
        use Keyword::*;
        use Operator::*;
        use Token::*;

        iter.next();
        let start = iter.loc();
        let name = match iter.peek() {
            Some(Id(name)) => {
                let name = name.clone();
                iter.next();
                name
            }
            Some(Key(End)) => {
                iter.next();
                let end = iter.loc();
                ast.add_diagnostic(to_diagnostic(
                    ErrorCode::MissingStmt(start.clone()),
                    end.clone(),
                ));
                return Stmt {
                    loc: start.start..end.end,
                    val: Stmt_::Error,
                };
            }
            Some(token) => {
                let token = token.clone();
                iter.next();
                let loc = iter.loc();
                ast.add_diagnostic(to_diagnostic(
                    ErrorCode::UnexpectedToken(
                        Some(token),
                        Some(Id("<name of this data type>".to_string())),
                        Some((Key(Keyword::Data), start.clone())),
                    ),
                    loc,
                ));
                "Error".to_string()
            }
            None => {
                ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                let end = iter.loc();
                return Stmt {
                    loc: start.start..end.end,
                    val: Stmt_::Error,
                };
            }
        };

        self.consume_to_op(iter, ast, Assign, Some((Key(Data), start.clone())));

        let mut subtypes = vec![];
        let mut current_subtype: Option<(String, Vec<_>)> = None;

        loop {
            match iter.peek() {
                Some(Id(name)) => {
                    if let Some(subtype) = &mut current_subtype {
                        subtype.1.push(name.clone());
                    } else {
                        current_subtype = Some((name.clone(), vec![]));
                    }
                    iter.next();
                }
                Some(Op(BitOr)) => {
                    iter.next();
                    if let Some(subtype) = current_subtype {
                        subtypes.push(subtype);
                        current_subtype = None;
                    } else {
                        ast.add_diagnostic(to_diagnostic(
                            ErrorCode::MissingConstructor,
                            iter.loc(),
                        ));
                    }
                }
                Some(Key(Def | End)) => {
                    break;
                }
                Some(token) => {
                    let token = token.clone();
                    iter.next();
                    let loc = iter.loc();
                    ast.add_diagnostic(to_diagnostic(
                        ErrorCode::UnexpectedToken(
                            Some(token),
                            None,
                            Some((Key(Data), start.clone())),
                        ),
                        loc,
                    ));
                    iter.next();
                }
                None => {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                    let end = iter.loc();
                    return Stmt {
                        loc: start.start..end.end,
                        val: Stmt_::Error,
                    };
                }
            }
        }

        if let Some(subtype) = current_subtype {
            subtypes.push(subtype);
        } else {
            ast.add_diagnostic(to_diagnostic(ErrorCode::MissingConstructor, iter.loc()));
        }

        let mut associated_funcs = vec![];
        loop {
            match iter.peek() {
                Some(Key(Def)) => {
                    let stmt = self.consume_def(iter, ast);
                    associated_funcs.push(stmt);
                }
                Some(Key(End)) => {
                    iter.next();
                    return Stmt {
                        loc: start.start..iter.loc().end,
                        val: Stmt_::Data(name, subtypes, associated_funcs),
                    };
                }
                Some(token) => {
                    let token = token.clone();
                    iter.next();
                    ast.add_diagnostic(to_diagnostic(
                        ErrorCode::UnexpectedToken(
                            Some(token),
                            Some(Key(Def)),
                            Some((Key(Data), start.clone())),
                        ),
                        iter.loc(),
                    ));
                }
                None => {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                    return Stmt {
                        loc: start.start..iter.loc().end,
                        val: Stmt_::Error,
                    };
                }
            }
        }
    }

    fn consume_case(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Expr {
        use Keyword::*;
        use Operator::*;
        use Token::*;
        iter.next();
        let start = iter.loc();
        let match_expr = self.consume_expr(iter, ast, 0, Some(Key(Of)));
        self.consume_to_key(iter, ast, Of, Some((Key(Case), start.clone())));
        let mut arms = vec![];
        loop {
            match iter.peek() {
                Some(Key(End)) => {
                    iter.next();
                    return Expr {
                        loc: start.start..iter.loc().end,
                        val: Expr_::Case(Box::new(match_expr), arms),
                    };
                }
                Some(_) => {
                    let pattern = self.consume_pattern(iter, ast, 0);
                    let guard = if let Some(Key(If)) = iter.peek() {
                        iter.next();
                        Some(self.consume_expr(iter, ast, 0, Some(Op(Arm))))
                    } else {
                        None
                    };
                    self.consume_to_op(iter, ast, Arm, None);
                    let stmt = self.consume_stmt(iter, ast, None);
                    arms.push((pattern, guard, stmt));
                }
                None => {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                    return Expr {
                        loc: start.start..iter.loc().end,
                        val: Expr_::Error,
                    };
                }
            }
        }
    }

    fn consume_fn(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Expr {
        use Keyword::*;
        use Operator::*;
        use Token::*;
        iter.next();
        let start = iter.loc();
        let mut parameters = vec![];
        loop {
            match iter.peek() {
                Some(Id(name)) => {
                    parameters.push(name.clone());
                    iter.next();
                }
                Some(Op(Assign)) => {
                    iter.next();
                    break;
                }
                Some(token) => {
                    ast.add_diagnostic(to_diagnostic(
                        ErrorCode::UnexpectedToken(
                            Some(token.clone()),
                            Some(Id("<parameter>".to_string())),
                            Some((Key(Fn), start.clone())),
                        ),
                        iter.loc(),
                    ));
                    iter.next();
                }
                None => {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                    return Expr {
                        loc: start.start..iter.loc().end,
                        val: Expr_::Error,
                    };
                }
            }
        }
        let expr = self.consume_expr(iter, ast, 0, None);
        Expr {
            loc: start.start..iter.loc().end,
            val: Expr_::Fn(parameters, Box::new(expr)),
        }
    }

    fn consume_def(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Stmt {
        use Keyword::*;
        use Operator::*;
        use Token::*;
        iter.next();
        let start = iter.loc();
        let name = match iter.peek() {
            Some(Id(name)) => {
                let name = name.clone();
                iter.next();
                Some(name)
            }
            Some(token) => {
                ast.add_diagnostic(to_diagnostic(
                    ErrorCode::UnexpectedToken(
                        Some(token.clone()),
                        Some(Id("<function name>".to_string())),
                        Some((Key(Def), start.clone())),
                    ),
                    start.clone(),
                ));
                None
            }
            None => {
                ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                return Stmt {
                    loc: start.start..iter.loc().end,
                    val: Stmt_::Error,
                };
            }
        };
        let eof = self.consume_to_op(iter, ast, LPar, Some((Key(Def), start.clone())));
        if eof {
            return Stmt {
                loc: start.start..iter.loc().end,
                val: Stmt_::Error,
            };
        }
        let mut decl = vec![];
        loop {
            match iter.peek() {
                Some(Id(name)) => {
                    let name = name.clone();
                    iter.next();
                    decl.push(name);
                }
                Some(Op(RPar)) => {
                    iter.next();
                    self.consume_to_op(iter, ast, Assign, Some((Key(Def), start.clone())));
                    break;
                }
                Some(token) => {
                    ast.add_diagnostic(to_diagnostic(
                        ErrorCode::UnexpectedToken(Some(token.clone()), None, None),
                        iter.loc(),
                    ));
                }
                None => {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                    return Stmt {
                        loc: start.start..iter.loc().end,
                        val: Stmt_::Error,
                    };
                }
            }
        }
        let mut stmts = vec![];
        let mut binds = vec![];
        loop {
            match iter.peek() {
                Some(Key(End)) => {
                    iter.next();
                    return if let Some(name) = name {
                        Stmt {
                            loc: start.start..iter.loc().end,
                            val: Stmt_::Def(name, decl, stmts, binds),
                        }
                    } else {
                        Stmt {
                            loc: start.start..iter.loc().end,
                            val: Stmt_::Error,
                        }
                    };
                }
                Some(Key(Where)) => {
                    iter.next();
                    loop {
                        match iter.peek2() {
                            (Some(Id(name)), Some(Op(Assign))) => {
                                let name = name.clone();
                                iter.next();
                                iter.next();
                                let expr = self.consume_expr(iter, ast, 0, Some(Key(End)));
                                binds.push((name.clone(), expr));
                            }
                            (Some(Key(End)), _) => {
                                iter.next();
                                return if let Some(name) = name {
                                    Stmt {
                                        loc: start.start..iter.loc().end,
                                        val: Stmt_::Def(name, decl, stmts, binds),
                                    }
                                } else {
                                    Stmt {
                                        loc: start.start..iter.loc().end,
                                        val: Stmt_::Error,
                                    }
                                };
                            }
                            (Some(token), _) => {
                                let token = token.clone();
                                iter.next();
                                ast.add_diagnostic(to_diagnostic(
                                    ErrorCode::UnexpectedToken(Some(token), None, None),
                                    iter.loc(),
                                ));
                            }
                            (None, _) => {
                                ast.add_diagnostic(to_diagnostic(
                                    ErrorCode::UnexpectedEof,
                                    iter.loc(),
                                ));
                                return Stmt {
                                    loc: start.start..iter.loc().end,
                                    val: Stmt_::Error,
                                };
                            }
                        }
                    }
                }
                Some(_) => stmts.push(self.consume_stmt(iter, ast, Some(Token::Key(Keyword::End)))),
                None => {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                    return Stmt {
                        loc: start.start..iter.loc().end,
                        val: Stmt_::Error,
                    };
                }
            }
        }
    }

    fn consume_block(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Expr {
        iter.next();
        let start = iter.loc();
        let mut stmts: Vec<Stmt> = vec![];
        loop {
            match iter.peek() {
                Some(Token::Key(Keyword::End)) => {
                    iter.next();
                    let end = iter.loc();
                    return Expr {
                        loc: start.start..end.end,
                        val: Expr_::Block(stmts),
                    };
                }
                Some(_) => {
                    let stmt = self.consume_stmt(iter, ast, Some(Token::Key(Keyword::End)));
                    stmts.push(stmt);
                }
                None => {
                    let end = iter.loc();
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, end.clone()));
                    return Expr {
                        loc: start.start..end.end,
                        val: Expr_::Block(stmts),
                    };
                }
            }
        }
    }

    fn consume_set_or_dict(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Expr {
        use Operator::*;
        use Token::*;
        iter.next();
        let start = iter.loc();
        let mut keys = vec![];
        let mut vals = vec![];
        let next2 = iter.peek2();
        if let (Some(Op(Colon)), Some(Op(RBrc))) = next2 {
            iter.next();
            iter.next();
            return Expr {
                loc: start.start..iter.loc().end,
                val: Expr_::Const(Const::Dict(vec![], vec![])),
            };
        };
        loop {
            match iter.peek() {
                Some(Op(RBrc)) => {
                    iter.next();
                    return if vals.is_empty() {
                        Expr {
                            loc: start.start..iter.loc().end,
                            val: Expr_::Const(Const::Set(keys)),
                        }
                    } else {
                        Expr {
                            loc: start.start..iter.loc().end,
                            val: Expr_::Const(Const::Dict(keys, vals)),
                        }
                    };
                }
                Some(_) => {
                    let key = self.consume_expr(iter, ast, 0, Some(Op(RBrc)));
                    match (keys.len(), vals.len(), iter.peek()) {
                        (x, y, Some(Op(Colon))) if x == y => {
                            iter.next();
                            let val = self.consume_expr(iter, ast, 0, Some(Op(RBrc)));
                            keys.push(key);
                            vals.push(val);
                        }
                        (0, 0, _) => {
                            keys.push(key);
                        }
                        (x, y, _) if x == y => {
                            ast.add_diagnostic(to_diagnostic(
                                ErrorCode::MissingMapValue,
                                iter.loc(),
                            ));
                        }
                        _ => {
                            keys.push(key);
                        }
                    }
                }
                None => {
                    return Expr {
                        loc: start.start..iter.loc().end,
                        val: Expr_::Error,
                    };
                }
            }
        }
    }

    /// Consume an expression
    ///
    /// not_take_on_error: do not consume a specific token type if an UnexpectedToken is
    /// encountered. This is used to prevent multiple diagnostics when an expression is missing or
    /// invalid between `if .. then` or `[ .. ]` and so on.
    fn consume_expr(
        &mut self,
        iter: &mut TokenIterator,
        ast: &mut Ast,
        min_precedence: u16,
        not_take_on_error: Option<Token>,
    ) -> Expr {
        use Keyword::*;
        use Operator::*;
        use Token::*;
        let start = iter.next_loc();
        let mut lhs = match iter.peek() {
            Some(Id(s)) => {
                let s = s.clone();
                iter.next();
                Expr {
                    loc: start.clone(),
                    val: Expr_::Id(s),
                }
            }
            Some(Key(Nil)) => {
                iter.next();
                Expr {
                    loc: start.clone(),
                    val: Expr_::Const(Const::Nil),
                }
            }
            Some(Key(Fn)) => self.consume_fn(iter, ast),
            Some(Str(s)) => {
                let s = s.clone();
                iter.next();
                Expr {
                    loc: start.clone(),
                    val: Expr_::Const(Const::Str(s)),
                }
            }
            Some(Float(f)) => {
                let f = *f;
                iter.next();
                Expr {
                    loc: start.clone(),
                    val: Expr_::Const(Const::Float(f)),
                }
            }
            Some(Integer(i)) => {
                let i = *i;
                iter.next();
                Expr {
                    loc: start.clone(),
                    val: Expr_::Const(Const::Int(i)),
                }
            }
            Some(Key(val @ (True | False))) => {
                let val = matches!(val, Keyword::True);
                iter.next();
                Expr {
                    loc: start.clone(),
                    val: Expr_::Const(Const::Bool(val)),
                }
            }
            Some(Op(LPar)) => {
                iter.next();
                let lhs = self.consume_expr(iter, ast, 0, Some(Op(RPar)));
                self.consume_to_op(iter, ast, RPar, Some((Op(LPar), start.clone())));
                Expr {
                    loc: start.start..iter.loc().end,
                    val: Expr_::Parentheses(Box::new(lhs)),
                }
            }
            Some(Op(op @ (Not | Minus))) => {
                let op = match op {
                    Not => OpPrefix::Not,
                    Minus => OpPrefix::Neg,
                    _ => unreachable!(),
                };
                iter.next();
                let rhs =
                    self.consume_expr(iter, ast, precedence_prefix(), not_take_on_error.clone());
                let end = iter.loc();
                Expr {
                    loc: start.start..end.end,
                    val: Expr_::Prefix(op, Box::new(rhs)),
                }
            }
            Some(Key(If)) => self.consume_if(iter, ast),
            Some(Key(Case)) => self.consume_case(iter, ast),
            Some(Key(Begin)) => self.consume_block(iter, ast),
            Some(Key(Require)) => self.consume_require(iter, ast),
            Some(Op(LBrk)) => {
                iter.next();
                let mut exprs = vec![];
                loop {
                    match iter.peek() {
                        Some(Op(RBrk)) => {
                            iter.next();
                            break Expr {
                                loc: start.start..iter.loc().end,
                                val: Expr_::Const(Const::List(exprs)),
                            };
                        }
                        Some(_) => exprs.push(self.consume_expr(iter, ast, 0, Some(Op(RBrk)))),
                        None => {
                            ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                            break Expr {
                                loc: start.start..iter.loc().end,
                                val: Expr_::Error,
                            };
                        }
                    }
                }
            }
            Some(Op(LBrc)) => self.consume_set_or_dict(iter, ast),
            Some(token) => {
                let should_not_consume = if let Some(t_avoid) = not_take_on_error {
                    match (token, t_avoid) {
                        (Op(op), Op(op_avoid)) => {
                            std::mem::discriminant(op) == std::mem::discriminant(&op_avoid)
                        }
                        (Key(k), Key(k_avoid)) => {
                            std::mem::discriminant(k) == std::mem::discriminant(&k_avoid)
                        }
                        _ => false, // Other check is useless
                    }
                } else {
                    false
                };
                if should_not_consume {
                    // Note that `start` is actually the location of this token
                    ast.add_diagnostic(to_diagnostic(
                        ErrorCode::MissingExpr(iter.loc()),
                        start.clone(),
                    ));
                } else {
                    let token = token.clone();
                    iter.next();
                    ast.add_diagnostic(to_diagnostic(
                        ErrorCode::UnexpectedToken(Some(token), None, None),
                        start.clone(),
                    ));
                }
                return Expr {
                    loc: start,
                    val: Expr_::Error,
                };
            }
            None => {
                ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, start.clone()));
                return Expr {
                    loc: start,
                    val: Expr_::Error,
                };
            }
        };

        loop {
            let op = match iter.peek2() {
                (Some(Op(Call)), op) => {
                    let op = match op {
                        Some(Op(LPar)) => OpPostfix::Call,
                        Some(Op(LBrk)) => OpPostfix::Index,
                        Some(Op(LBrc)) => OpPostfix::Construct,
                        Some(token) => {
                            let token = token.clone();
                            iter.next();
                            let loc = iter.loc();
                            iter.next();
                            let loc_token = iter.loc();
                            ast.add_diagnostic(to_diagnostic(
                                ErrorCode::UnexpectedToken(
                                    Some(token),
                                    None,
                                    Some((Op(Call), loc)),
                                ),
                                loc_token,
                            ));
                            return lhs;
                        }
                        None => {
                            iter.next();
                            let loc = iter.loc();
                            ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, loc));
                            return lhs;
                        }
                    };
                    let precedence = precedence_postfix();
                    if precedence > min_precedence {
                        match op {
                            OpPostfix::Call => {
                                iter.next();
                                iter.next();
                                let mut exprs = vec![];
                                loop {
                                    match iter.peek() {
                                        Some(Op(RPar)) => {
                                            iter.next();
                                            break;
                                        }
                                        Some(_) => exprs.push(self.consume_expr(
                                            iter,
                                            ast,
                                            0,
                                            Some(Op(RBrk)),
                                        )),
                                        None => {
                                            ast.add_diagnostic(to_diagnostic(
                                                ErrorCode::UnexpectedEof,
                                                iter.loc(),
                                            ));
                                            break;
                                        }
                                    }
                                }
                                lhs = Expr {
                                    loc: start.start..iter.loc().end,
                                    val: Expr_::Call(Box::new(lhs), exprs),
                                };
                                continue;
                            }
                            OpPostfix::Index => {
                                iter.next();
                                iter.next();
                                let match_loc = iter.loc();
                                let expr = self.consume_expr(iter, ast, 0, Some(Op(RBrk)));
                                self.consume_to_op(iter, ast, RBrk, Some((Op(LBrk), match_loc)));
                                lhs = Expr {
                                    loc: start.start..iter.loc().end,
                                    val: Expr_::Index(Box::new(lhs), Box::new(expr)),
                                };
                                continue;
                            }
                            OpPostfix::Construct => {
                                iter.next();
                                let init = self.consume_set_or_dict(iter, ast);
                                lhs = Expr {
                                    loc: start.start..iter.loc().end,
                                    val: Expr_::Construct(Box::new(lhs), Box::new(init)),
                                };
                                continue;
                            }
                        };
                    } else {
                        return lhs;
                    }
                }
                (Some(op), _) => match op {
                    Op(Assign) => OpInfix::Assign,
                    Op(Range) => OpInfix::Range,
                    Op(Or) => OpInfix::Or,
                    Op(And) => OpInfix::And,
                    Op(Eq) => OpInfix::Eq,
                    Op(Ne) => OpInfix::Ne,
                    Op(Le) => OpInfix::Le,
                    Op(Lt) => OpInfix::Lt,
                    Op(Ge) => OpInfix::Ge,
                    Op(Gt) => OpInfix::Gt,
                    Op(Plus) => OpInfix::Plus,
                    Op(Minus) => OpInfix::Minus,
                    Op(Mul) => OpInfix::Mul,
                    Op(Div) => OpInfix::Div,
                    Op(DivFloor) => OpInfix::DivFloor,
                    Op(Mod) => OpInfix::Mod,
                    Op(Exp) => OpInfix::Exp,
                    Op(Comma) => OpInfix::Comma,
                    Op(Member) => OpInfix::Member,
                    _ => return lhs,
                },
                _ => return lhs,
            };

            let precedence = precedence_infix(op);
            if precedence.0 < min_precedence {
                break;
            }

            iter.next();

            let rhs = self.consume_expr(iter, ast, precedence.1, not_take_on_error.clone());

            lhs = Expr {
                loc: start.start..iter.loc().end,
                val: Expr_::Infix(op, Box::new(lhs), Box::new(rhs)),
            };
        }

        lhs
    }

    /// Consume a pattern
    ///
    /// Precedence: "|" => 0 "," => 1
    fn consume_pattern(
        &mut self,
        iter: &mut TokenIterator,
        ast: &mut Ast,
        min_precedence: u16,
    ) -> Pattern {
        use Keyword::*;
        use Operator::*;
        use Token::*;
        let start = iter.next_loc();

        let mut lhs = match iter.peek() {
            Some(Id(name)) => {
                let mut id = vec![name.clone()];
                iter.next();
                if let Some(Op(At)) = iter.peek() {
                    iter.next();
                    let bind = self.consume_pattern(iter, ast, 2);
                    Pattern::new(
                        start.start..iter.loc().end,
                        Pattern_::Bind(
                            id.into_iter().next().expect("This must have an item here"),
                            Box::new(bind),
                        ),
                    )
                } else {
                    while let (Some(Op(Member)), Some(Id(name))) = iter.peek2() {
                        id.push(name.clone());
                        iter.next();
                        iter.next();
                    }
                    if let (Some(Op(Call)), Some(Op(LBrc))) = iter.peek2() {
                        iter.next();
                        iter.next();
                        let mut inners = vec![];
                        loop {
                            match iter.peek() {
                                Some(Op(RBrc)) => {
                                    iter.next();
                                    break;
                                }
                                Some(_) => inners.push(self.consume_pattern(iter, ast, 0)),
                                None => {
                                    ast.add_diagnostic(to_diagnostic(
                                        ErrorCode::UnexpectedEof,
                                        iter.loc(),
                                    ));
                                    return Pattern::new(
                                        start.start..iter.loc().end,
                                        Pattern_::Error,
                                    );
                                }
                            }
                        }
                        Pattern::new(start.start..iter.loc().end, Pattern_::Inner(id, inners))
                    } else {
                        Pattern::new(start.start..iter.loc().end, Pattern_::Id(id))
                    }
                }
            }
            Some(Integer(i)) => {
                let i = *i;
                iter.next();
                Pattern::new(start.clone(), Pattern_::Const(ConstPattern::Int(i)))
            }
            Some(Float(f)) => {
                let f = *f;
                iter.next();
                Pattern::new(start.clone(), Pattern_::Const(ConstPattern::Float(f)))
            }
            Some(Str(s)) => {
                let s = s.clone();
                iter.next();
                Pattern::new(start.clone(), Pattern_::Const(ConstPattern::Str(s)))
            }
            Some(Key(key @ (True | False))) => {
                let val = matches!(key, True);
                iter.next();
                Pattern::new(start.clone(), Pattern_::Const(ConstPattern::Bool(val)))
            }
            Some(Op(LPar)) => {
                iter.next();
                let prev_loc = iter.loc();
                let pattern = self.consume_pattern(iter, ast, 0);
                self.consume_to_op(iter, ast, RPar, Some((Op(LPar), prev_loc)));
                Pattern::new(
                    start.start..iter.loc().end,
                    Pattern_::Parentheses(Box::new(pattern)),
                )
            }
            Some(token) => {
                let token = token.clone();
                iter.next();
                ast.add_diagnostic(to_diagnostic(
                    ErrorCode::UnexpectedToken(Some(token), None, None),
                    iter.loc(),
                ));
                return Pattern::new(start.start..iter.loc().end, Pattern_::Error);
            }
            None => {
                ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                return Pattern::new(start, Pattern_::Error);
            }
        };

        loop {
            match iter.peek() {
                Some(Op(Comma)) if min_precedence <= 1 => {
                    iter.next();
                    let rhs = self.consume_pattern(iter, ast, 1);
                    lhs = Pattern::new(
                        start.start..iter.loc().end,
                        Pattern_::And(Box::new(lhs), Box::new(rhs)),
                    );
                }
                Some(Op(BitOr)) if min_precedence == 0 => {
                    iter.next();
                    let rhs = self.consume_pattern(iter, ast, 0);
                    lhs = Pattern::new(
                        start.start..iter.loc().end,
                        Pattern_::Or(Box::new(lhs), Box::new(rhs)),
                    );
                }
                _ => return lhs,
            }
        }
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}
