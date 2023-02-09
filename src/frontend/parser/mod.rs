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
use ast::{Ast, Const, Expr, OpInfix, OpPostfix, OpPrefix, Stmt};
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
        Comma => (1, 2),
        Assign => (4, 3),
        Range => (5, 6),
        Or => (7, 8),
        And => (9, 10),
        Eq | Ne | Le | Lt | Gt | Ge => (11, 12),
        Plus | Minus => (13, 14),
        Mul | Div | DivFloor | Rem => (15, 16),
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
                | Keyword::If
                | Keyword::Begin
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
pub struct Parser {
    search_path: Vec<PathBuf>,
    relative_search_path: Option<PathBuf>,
    modules: AHashMap<OsString, Ast>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            search_path: vec![],
            relative_search_path: None,
            modules: AHashMap::new(),
        }
    }

    /// add module search path
    pub fn _with_path(mut self, path: PathBuf) -> Self {
        self.search_path.push(path);
        self
    }

    /// Get all loaded modules
    pub fn _modules(&self) -> &AHashMap<OsString, Ast> {
        &self.modules
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
        let prev_relative_path = self.relative_search_path.replace(
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
        self.relative_search_path = prev_relative_path;
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
                return Expr::Error;
            };
            s_split
        } else {
            ast.add_diagnostic(to_diagnostic(
                ErrorCode::RequireWrongArgument,
                iter.next_loc(),
            ));
            return Expr::Error;
        };
        // search for file
        let mut search_path = self.search_path.clone();
        if let Some(path) = &self.relative_search_path {
            search_path.insert(0, path.clone());
        }
        for mut path in search_path {
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

            return Expr::Module {
                loc: start.start..iter.loc().end,
                path: DisplayableOsString::new(path.as_os_str().to_os_string()),
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

        Expr::Error
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
                Stmt::Break { loc: start }
            }
            Some(Key(Continue)) => {
                iter.next();
                Stmt::Continue { loc: start }
            }
            Some(Key(Return)) => {
                iter.next();
                match iter.peek() {
                    Some(expr_start_pattern!()) => {
                        let expr = self.consume_expr(iter, ast, 0, not_take_on_error);
                        let end = iter.loc();
                        Stmt::Return {
                            loc: start.start..end.end,
                            value: Some(expr),
                        }
                    }
                    _ => Stmt::Return {
                        loc: start,
                        value: None,
                    },
                }
            }
            Some(Key(Def)) => self.consume_def(iter, ast),
            Some(expr_start_pattern!()) => {
                let expr = self.consume_expr(iter, ast, 0, not_take_on_error);
                let end = iter.loc();
                Stmt::Expr {
                    loc: start.start..end.end,
                    expr,
                }
            }
            Some(Key(Loop | Until)) => self.consume_loop(iter, ast),
            Some(Key(For)) => self.consume_for(iter, ast),
            Some(token) => {
                let token = token.clone();
                iter.next();
                ast.add_diagnostic(to_diagnostic(
                    ErrorCode::UnexpectedToken(Some(token), None, None),
                    start,
                ));
                Stmt::Error
            }
            None => {
                ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, start));
                Stmt::Error
            }
        }
    }

    /// Consume an iterator to an expected operator or EOF
    /// Errors are written to `self.diagnoser`
    /// Return true if eof met otherwise false
    #[must_use]
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
    #[must_use]
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

    fn consume_condition_then(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Option<Expr> {
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
        let mut conditional = vec![];
        let mut condition;
        match self.consume_condition_then(iter, ast) {
            Some(expr) => condition = expr,
            None => {
                return Expr::Error;
            }
        }
        // match block
        let mut block: Vec<Stmt> = vec![];
        loop {
            match iter.peek() {
                Some(Key(Elsif)) => {
                    iter.next();
                    match self.consume_condition_then(iter, ast) {
                        Some(expr) => {
                            conditional.push((condition, block));
                            condition = expr;
                            block = vec![];
                        }
                        None => {
                            return Expr::Error;
                        }
                    }
                }
                Some(Key(Else)) => {
                    conditional.push((condition, block));
                    block = vec![];
                    iter.next();
                    loop {
                        match iter.peek() {
                            Some(Key(End)) => {
                                iter.next();
                                let end = iter.loc();
                                return Expr::If {
                                    loc: start.start..end.end,
                                    conditional,
                                    default: Some(block),
                                };
                            }
                            Some(_) => {
                                let stmt = self.consume_stmt(iter, ast, Some(Key(End)));
                                block.push(stmt);
                            }
                            None => {
                                let end = iter.loc();
                                ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, end));
                                return Expr::Error;
                            }
                        }
                    }
                }
                Some(Key(End)) => {
                    conditional.push((condition, block));
                    iter.next();
                    let end = iter.loc();
                    return Expr::If {
                        loc: start.start..end.end,
                        conditional,
                        default: None,
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
                    return Expr::Error;
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
            return Stmt::Error;
        };
        let iterator = self.consume_expr(iter, ast, 0, Some(Key(Do)));
        if self.consume_to_key(iter, ast, Do, Some((Key(For), start.clone()))) {
            return Stmt::Error;
        };
        let mut body = vec![];
        loop {
            match iter.peek() {
                Some(Key(End)) => {
                    iter.next();
                    return Stmt::For {
                        loc: start.start..iter.loc().end,
                        loop_variable: Box::new(vars),
                        iterator: Box::new(iterator),
                        body,
                    };
                }
                Some(_) => body.push(self.consume_stmt(iter, ast, Some(Key(End)))),
                None => {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                    return Stmt::Error;
                }
            }
        }
    }

    fn consume_loop(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Stmt {
        use Keyword::*;
        use Token::*;
        let key = iter.next().cloned();
        let start = iter.loc();
        let condition = match key {
            Some(Key(Loop)) => None,
            Some(Key(Until)) => {
                let stmt = self.consume_expr(iter, ast, 0, None);
                if matches!(iter.peek(), None) {
                    return Stmt::Error;
                };
                if self.consume_to_key(iter, ast, Do, Some((Key(Until), start.clone()))) {
                    return Stmt::Error;
                };
                Some(stmt)
            }
            _ => unreachable!(),
        };
        let mut body = vec![];
        loop {
            match iter.peek() {
                Some(Key(End)) => {
                    iter.next();
                    return Stmt::Loop {
                        loc: start.start..iter.loc().end,
                        condition,
                        body,
                    };
                }
                Some(_) => body.push(self.consume_stmt(iter, ast, Some(Key(End)))),
                None => {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                    return Stmt::Error;
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
                    parameters.push((name.clone(), iter.next_loc()));
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
                    return Expr::Error;
                }
            }
        }
        let expr = self.consume_expr(iter, ast, 0, None);
        Expr::Fn {
            loc: start.start..iter.loc().end,
            parameters,
            body: Box::new(expr),
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
                return Stmt::Error;
            }
        };
        let mut parameters = vec![];
        loop {
            match iter.peek() {
                Some(Id(name)) => {
                    let name = name.clone();
                    iter.next();
                    let loc = iter.loc();
                    parameters.push((name, loc));
                }
                Some(Op(Assign)) => {
                    iter.next();
                    break;
                }
                Some(token) => {
                    ast.add_diagnostic(to_diagnostic(
                        ErrorCode::UnexpectedToken(Some(token.clone()), None, None),
                        iter.loc(),
                    ));
                    iter.next();
                }
                None => {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                    return Stmt::Error;
                }
            }
        }
        let mut body = vec![];
        loop {
            match iter.peek() {
                Some(Key(End)) => {
                    iter.next();
                    return if let Some(name) = name {
                        Stmt::Def {
                            loc: start.start..iter.loc().end,
                            name,
                            parameters,
                            body,
                        }
                    } else {
                        Stmt::Error
                    };
                }
                Some(_) => body.push(self.consume_stmt(iter, ast, Some(Token::Key(Keyword::End)))),
                None => {
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, iter.loc()));
                    return Stmt::Error;
                }
            }
        }
    }

    fn consume_block(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Expr {
        iter.next();
        let start = iter.loc();
        let mut body: Vec<Stmt> = vec![];
        loop {
            match iter.peek() {
                Some(Token::Key(Keyword::End)) => {
                    iter.next();
                    let end = iter.loc();
                    return Expr::Block {
                        loc: start.start..end.end,
                        body,
                    };
                }
                Some(_) => {
                    let stmt = self.consume_stmt(iter, ast, Some(Token::Key(Keyword::End)));
                    body.push(stmt);
                }
                None => {
                    let end = iter.loc();
                    ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, end));
                    return Expr::Error;
                }
            }
        }
    }

    fn consume_table(&mut self, iter: &mut TokenIterator, ast: &mut Ast) -> Expr {
        use Operator::*;
        use Token::*;
        iter.next();
        let start = iter.loc();

        if let Some(Op(RBrc)) = iter.peek() {
            iter.next();
            let end = iter.loc();
            return Expr::Const {
                loc: start.start..end.end,
                value: Const::Table(vec![]),
            };
        }
        let mut key_vals = vec![];
        let mut content = self.consume_expr(iter, ast, 0, Some(Op(RBrc)));
        if self.consume_to_op(iter, ast, RBrc, Some((Op(LBrc), start.clone()))) {
            return Expr::Error;
        };

        loop {
            match content {
                Expr::Infix {
                    loc,
                    op: OpInfix::Assign,
                    lhs,
                    rhs,
                } => {
                    let lhs = *lhs;
                    let name = match lhs {
                        Expr::Id { loc: _, name } => name,
                        _ => {
                            ast.add_diagnostic(to_diagnostic(
                                ErrorCode::InvalidTableKey,
                                lhs.get_loc(),
                            ));
                            return Expr::Error;
                        }
                    };
                    key_vals.push((name, *rhs, loc));
                    break;
                }
                Expr::Infix {
                    loc,
                    op: OpInfix::Comma,
                    lhs,
                    rhs,
                } => {
                    match *rhs {
                        Expr::Infix {
                            loc,
                            op: OpInfix::Assign,
                            lhs,
                            rhs,
                        } => {
                            let lhs = *lhs;
                            let name = match lhs {
                                Expr::Id { loc: _, name } => name,
                                _ => {
                                    ast.add_diagnostic(to_diagnostic(
                                        ErrorCode::InvalidTableKey,
                                        lhs.get_loc(),
                                    ));
                                    return Expr::Error;
                                }
                            };
                            key_vals.push((name, *rhs, loc));
                        }
                        _ => {
                            ast.add_diagnostic(to_diagnostic(ErrorCode::InvalidTableFormat, loc));
                            return Expr::Error;
                        }
                    }
                    content = *lhs;
                }
                Expr::Error => return content,
                _ => {
                    ast.add_diagnostic(to_diagnostic(
                        ErrorCode::InvalidTableFormat,
                        content.get_loc(),
                    ));
                    return Expr::Error;
                }
            }
        }

        let end = iter.loc();
        key_vals.reverse();
        Expr::Const {
            loc: start.start..end.end,
            value: Const::Table(key_vals),
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
                Expr::Id {
                    loc: start.clone(),
                    name: s,
                }
            }
            Some(Key(Fn)) => self.consume_fn(iter, ast),
            Some(Str(s)) => {
                let s = s.clone();
                iter.next();
                Expr::Const {
                    loc: start.clone(),
                    value: Const::Str(s),
                }
            }
            Some(Float(f)) => {
                let f = *f;
                iter.next();
                Expr::Const {
                    loc: start.clone(),
                    value: Const::Float(f),
                }
            }
            Some(Integer(i)) => {
                let i = *i;
                iter.next();
                Expr::Const {
                    loc: start.clone(),
                    value: Const::Int(i),
                }
            }
            Some(Key(val @ (True | False))) => {
                let val = matches!(val, Keyword::True);
                iter.next();
                Expr::Const {
                    loc: start.clone(),
                    value: Const::Bool(val),
                }
            }
            Some(Op(LPar)) => {
                iter.next();
                if matches!(iter.peek(), Some(Op(RPar))) {
                    iter.next();
                    Expr::Const {
                        loc: start.clone(),
                        value: Const::Unit,
                    }
                } else {
                    let lhs = self.consume_expr(iter, ast, 0, Some(Op(RPar)));
                    if self.consume_to_op(iter, ast, RPar, Some((Op(LPar), start.clone()))) {
                        return Expr::Error;
                    };
                    Expr::Parentheses {
                        loc: start.clone(),
                        content: Box::new(lhs),
                    }
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
                Expr::Prefix {
                    loc: start.start..end.end,
                    op,
                    rhs: Box::new(rhs),
                }
            }
            Some(Key(If)) => self.consume_if(iter, ast),
            Some(Key(Begin)) => self.consume_block(iter, ast),
            Some(Key(Require)) => self.consume_require(iter, ast),
            Some(Op(LBrk)) => {
                iter.next();
                let previous_loc = iter.loc();
                let mut exprs = vec![];
                if let Some(Op(RBrk)) = iter.peek() {
                    iter.next();
                } else {
                    let mut parameters = self.consume_expr(iter, ast, 0, Some(Op(RBrk)));
                    loop {
                        match parameters {
                            Expr::Infix {
                                loc: _,
                                op: OpInfix::Comma,
                                lhs,
                                rhs,
                            } => {
                                exprs.push(*rhs);
                                parameters = *lhs;
                            }
                            expr => {
                                exprs.push(expr);
                                break;
                            }
                        }
                    }
                    // reverse to normal order
                    exprs.reverse();
                    if self.consume_to_op(iter, ast, RBrk, Some((Op(LBrk), previous_loc))) {
                        return Expr::Error;
                    };
                }
                Expr::Const {
                    loc: start.start..iter.loc().end,
                    value: Const::List(exprs),
                }
            }
            Some(Op(LBrc)) => self.consume_table(iter, ast),
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
                    ast.add_diagnostic(to_diagnostic(ErrorCode::MissingExpr(iter.loc()), start));
                } else {
                    let token = token.clone();
                    iter.next();
                    ast.add_diagnostic(to_diagnostic(
                        ErrorCode::UnexpectedToken(Some(token), None, None),
                        start,
                    ));
                }
                return Expr::Error;
            }
            None => {
                ast.add_diagnostic(to_diagnostic(ErrorCode::UnexpectedEof, start));
                return Expr::Error;
            }
        };

        loop {
            let op = match iter.peek2() {
                (Some(Op(Call)), op) => {
                    let op = match op {
                        Some(Op(LPar)) => OpPostfix::Call,
                        Some(Op(LBrk)) => OpPostfix::Index,
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
                                let previous_loc = iter.loc();
                                let mut exprs = vec![];
                                if let Some(Op(RPar)) = iter.peek() {
                                    iter.next();
                                } else {
                                    let mut parameters =
                                        self.consume_expr(iter, ast, 0, Some(Op(RPar)));
                                    loop {
                                        match parameters {
                                            Expr::Infix {
                                                loc: _,
                                                op: OpInfix::Comma,
                                                lhs,
                                                rhs,
                                            } => {
                                                exprs.push(*rhs);
                                                parameters = *lhs;
                                            }
                                            expr => {
                                                exprs.push(expr);
                                                break;
                                            }
                                        }
                                    }
                                    // reverse to normal order
                                    exprs.reverse();
                                    if self.consume_to_op(
                                        iter,
                                        ast,
                                        RPar,
                                        Some((Op(LPar), previous_loc)),
                                    ) {
                                        return Expr::Error;
                                    };
                                }
                                lhs = Expr::Call {
                                    loc: start.start..iter.loc().end,
                                    lhs: Box::new(lhs),
                                    parameters: exprs,
                                };
                                continue;
                            }
                            OpPostfix::Index => {
                                iter.next();
                                iter.next();
                                let match_loc = iter.loc();
                                let expr = self.consume_expr(iter, ast, 0, Some(Op(RBrk)));
                                if self.consume_to_op(iter, ast, RBrk, Some((Op(LBrk), match_loc)))
                                {
                                    return Expr::Error;
                                };
                                lhs = Expr::Index {
                                    loc: start.start..iter.loc().end,
                                    lhs: Box::new(lhs),
                                    rhs: Box::new(expr),
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
                    Op(Rem) => OpInfix::Rem,
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

            lhs = Expr::Infix {
                loc: start.start..iter.loc().end,
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }

        lhs
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}
