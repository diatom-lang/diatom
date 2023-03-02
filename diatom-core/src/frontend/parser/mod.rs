pub mod ast;
mod error;
mod path_resolver;
#[cfg(test)]
mod tests;

use crate::file_manager::{Diagnostic, FileManager, Loc};
use crate::frontend::parser::ast::ImportItem;

use self::{error::ErrorCode, path_resolver::try_get_mod};

use super::{
    lexer::{Keyword, Operator, Token},
    util::TokenIterator,
    Lexer,
};

use ast::{Const, Expr, OpInfix, OpPostfix, OpPrefix, Stmt};
use codespan_reporting::diagnostic::Label;
use std::collections::BTreeMap;
use std::{ffi::OsString, mem::Discriminant, path::PathBuf};

const fn precedence_infix(op: OpInfix) -> (u16, u16) {
    use OpInfix::*;
    match op {
        Comma => (1, 2),
        Assign => (4, 3),
        Range => (5, 6),
        Or => (7, 8),
        And => (9, 10),
        Eq | Ne | Le | Lt | Gt | Ge | Is => (11, 12),
        Plus | Minus => (13, 14),
        Mul | Div | DivFloor | Rem => (15, 16),
        Exp => (17, 18),
        LArrow => (21, 22),
        DoubleColon => (23, 24),
        Member => (23, 24),
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
        Token::Key(Keyword::Fn | Keyword::If | Keyword::Begin | Keyword::True | Keyword::False)
            | Token::Op(Operator::LBrc)
            | Token::Op(Operator::LBrk)
            | Token::Op(Operator::LPar)
            | Token::Op(Operator::Minus)
            | Token::Id(_)
            | Token::Integer(_)
            | Token::Float(_)
            | Token::Str(_)
    };
}

/// The parser for Diatom.
pub struct Parser<'a> {
    file_manager: &'a mut FileManager,
    relative_path: Option<PathBuf>,
    search_path: &'a [PathBuf],
    import_stack: BTreeMap<usize, Option<Loc>>,
    fid: usize,
}

impl<'a> Parser<'a> {
    pub fn new(file_manager: &'a mut FileManager, search_path: &'a [PathBuf]) -> Self {
        Self {
            file_manager,
            import_stack: Default::default(),
            relative_path: None,
            search_path,
            fid: 0,
        }
    }

    /// Parse a file
    pub fn parse_file(&mut self, path: impl Into<OsString>, content: impl Into<String>) -> usize {
        let path = path.into();
        let fid = self.file_manager.add_file(path.clone(), content.into());
        let path = PathBuf::from(path);
        if let Some(path) = path.parent() {
            self.relative_path = Some(PathBuf::from(path))
        };
        self.parse_fid(fid, None);
        fid
    }

    /// Parse a phony file
    pub fn parse_file_phony(
        &mut self,
        path: impl Into<OsString>,
        content: impl Into<String>,
    ) -> usize {
        let path = path.into();
        let fid = self.file_manager.add_file(path, content.into());
        self.parse_fid(fid, None);
        fid
    }

    fn parse_fid(&mut self, fid: usize, loc: Option<Loc>) {
        self.fid = fid;
        self.import_stack.insert(fid, loc);
        let token_stream = Lexer::lex(self.file_manager, fid);
        let mut iter = token_stream.iter();
        let mut stmts = vec![];

        while iter.peek().is_some() {
            let stmt = self.consume_stmt(&mut iter, None);
            stmts.push(stmt);
        }

        self.import_stack.remove(&fid);
        self.file_manager.set_ast(self.fid, stmts);
    }

    fn consume_stmt(&mut self, iter: &mut TokenIterator, not_take_on_error: Option<Token>) -> Stmt {
        use Keyword::*;
        use Token::*;
        while let Some(Token::Op(Operator::SemiColon)) = iter.peek() {
            iter.next();
            continue;
        }
        let start = iter.next_loc();
        let stmt = match iter.peek() {
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
                        let expr = self.consume_expr(iter, 0, not_take_on_error);
                        let end = iter.loc();
                        Stmt::Return {
                            loc: start + end,
                            value: Some(expr),
                        }
                    }
                    _ => Stmt::Return {
                        loc: start,
                        value: None,
                    },
                }
            }
            Some(Key(Def)) => self.consume_def(iter),
            Some(expr_start_pattern!()) => {
                let expr = self.consume_expr(iter, 0, not_take_on_error);
                let end = iter.loc();
                Stmt::Expr {
                    loc: start + end,
                    expr,
                }
            }
            Some(Key(Import)) => self.consume_import(iter),
            Some(Key(Loop | Until)) => self.consume_loop(iter),
            Some(Key(For)) => self.consume_for(iter),
            Some(token) => {
                let token = token.clone();
                iter.next();
                self.add_diagnostic(ErrorCode::UnexpectedToken(Some(token), None, None), start);
                Stmt::Error
            }
            None => {
                self.add_diagnostic(ErrorCode::UnexpectedEof, start);
                Stmt::Error
            }
        };
        while let Some(Token::Op(Operator::SemiColon)) = iter.peek() {
            iter.next();
            continue;
        }
        stmt
    }

    /// Consume an iterator to an expected operator or EOF
    /// Errors are written to `self.diagnoser`
    /// Return true if eof met otherwise false
    #[must_use]
    fn consume_to_op(
        &mut self,
        iter: &mut TokenIterator,
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
            self.add_diagnostic(
                ErrorCode::UnexpectedToken(t, Some(Token::Op(expected)), previous),
                loc_now,
            );
        }
        loop {
            if test_match(op_type, iter) {
                iter.next();
                return false;
            }
            match iter.next() {
                Some(_) => (),
                None => {
                    self.add_diagnostic(ErrorCode::UnexpectedEof, iter.loc());
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
            self.add_diagnostic(
                ErrorCode::UnexpectedToken(t, Some(Token::Key(expected)), previous),
                loc_now,
            );
        }
        loop {
            if test_match(key_type, iter) {
                iter.next();
                return false;
            }
            match iter.next() {
                Some(_) => (),
                None => {
                    self.add_diagnostic(ErrorCode::UnexpectedEof, iter.loc());
                    return true;
                }
            }
        }
    }

    fn consume_condition_then(&mut self, iter: &mut TokenIterator) -> Option<Expr> {
        use Keyword::*;
        use Token::*;
        let start = iter.loc();
        // match `condition`
        let condition = self.consume_expr(iter, 0, Some(Key(Then)));
        // match `then`
        if !self.consume_to_key(iter, Then, Some((Key(If), start))) {
            Some(condition)
        } else {
            None
        }
    }

    fn consume_if(&mut self, iter: &mut TokenIterator) -> Expr {
        use Keyword::*;
        use Token::*;
        iter.next();
        let start = iter.loc();
        let mut conditional = vec![];
        let mut condition;
        match self.consume_condition_then(iter) {
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
                    match self.consume_condition_then(iter) {
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
                                    loc: start + end,
                                    conditional,
                                    default: Some(block),
                                };
                            }
                            Some(_) => {
                                let stmt = self.consume_stmt(iter, Some(Key(End)));
                                block.push(stmt);
                            }
                            None => {
                                let end = iter.loc();
                                self.add_diagnostic(ErrorCode::UnexpectedEof, end);
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
                        loc: start + end,
                        conditional,
                        default: None,
                    };
                }
                Some(_) => {
                    let stmt = self.consume_stmt(iter, Some(Key(Else)));
                    block.push(stmt);
                }
                None => {
                    let end = iter.loc();
                    self.add_diagnostic(
                        ErrorCode::UnexpectedToken(None, Some(Key(Else)), Some((Key(If), start))),
                        end,
                    );
                    return Expr::Error;
                }
            }
        }
    }

    fn convert_expr_to_import(&mut self, mut expr: Expr) -> Result<Vec<String>, ()> {
        let mut item = vec![];
        loop {
            match expr {
                Expr::Id { name, .. } => {
                    item.push(name);
                    item.reverse();
                    return Ok(item);
                }
                Expr::Infix {
                    op: OpInfix::Member,
                    lhs,
                    rhs,
                    ..
                } => {
                    if let Expr::Id { name, .. } = *rhs {
                        item.push(name);
                        expr = *lhs;
                    } else {
                        return Err(());
                    }
                }
                _ => return Err(()),
            }
        }
    }

    fn consume_import_item(&mut self, iter: &mut TokenIterator) -> Result<ImportItem, ()> {
        use Keyword::*;
        use Token::*;
        let start = iter.next_loc();
        let path = self.consume_expr(iter, 3, None);
        let path = self.convert_expr_to_import(path)?;
        let mut alias = None;
        if let (Some(Key(As)), Some(Id(name))) = iter.peek2() {
            alias = Some(name.clone());
            iter.next();
            iter.next();
        }
        let end = iter.loc();
        Ok(ImportItem {
            loc: start + end,
            alias,
            path,
        })
    }

    fn resolve_mod(&mut self, mod_path: &[String]) -> Option<(usize, PathBuf)> {
        if self.file_manager.is_ext_name(&mod_path[0]) {
            return try_get_mod(&PathBuf::new(), mod_path, self.file_manager);
        }

        if let Some(f) = self
            .relative_path
            .as_ref()
            .and_then(|path| try_get_mod(path, mod_path, self.file_manager))
        {
            return Some(f);
        }

        self.search_path
            .iter()
            .find_map(|path| try_get_mod(path, mod_path, self.file_manager))
    }

    fn consume_import(&mut self, iter: &mut TokenIterator) -> Stmt {
        use Keyword::*;
        use Operator::*;
        use Token::*;
        iter.next();
        let start = iter.loc();
        let mut import_items = vec![];
        match iter.peek() {
            Some(Op(LBrc)) => {
                iter.next();
                loop {
                    match iter.peek2() {
                        (Some(Op(Comma)), Some(Op(RBrc))) => {
                            iter.next();
                            iter.next();
                            break;
                        }
                        (Some(Op(RBrc)), _) => {
                            iter.next();
                            break;
                        }
                        (None, _) => {
                            self.add_diagnostic(ErrorCode::UnexpectedEof, iter.loc());
                            return Stmt::Error;
                        }
                        _ => {
                            if let Some(Op(Comma)) = iter.peek() {
                                iter.next();
                            }
                            if let Ok(item) = self.consume_import_item(iter) {
                                import_items.push(item);
                            }
                        }
                    }
                }
            }
            None => {
                self.add_diagnostic(ErrorCode::UnexpectedEof, iter.loc());
                return Stmt::Error;
            }
            _ => {
                if let Ok(item) = self.consume_import_item(iter) {
                    if let Some(Key(From)) = iter.peek() {
                        import_items.push(item);
                    } else {
                        let end = iter.loc();

                        let module = self.resolve_mod(&item.path);
                        let (fid, path) = match module {
                            Some(module) => module,
                            None => {
                                self.add_diagnostic(ErrorCode::ModuleNotFound, start + end);
                                return Stmt::Error;
                            }
                        };

                        if let Some(loc) = self.import_stack.get(&fid) {
                            self.add_diagnostic(
                                ErrorCode::CircularImport(loc.clone()),
                                start + end,
                            );
                            return Stmt::Error;
                        }

                        let prev_path = if let Some(path) = path.parent() {
                            std::mem::replace(&mut self.relative_path, Some(PathBuf::from(path)))
                        } else {
                            None
                        };

                        let loc = start + end;
                        let fid_prev = self.fid;
                        self.parse_fid(fid, Some(loc.clone()));
                        self.fid = fid_prev;

                        self.relative_path = prev_path;

                        return Stmt::Import {
                            loc,
                            fid,
                            items: vec![item],
                            direct_import_mod: true,
                        };
                    }
                } else {
                    return Stmt::Error;
                }
            }
        }

        if self.consume_to_key(iter, From, Some((Key(Import), start.clone()))) {
            return Stmt::Error;
        };

        let from = self.consume_expr(iter, 0, None);
        let loc = from.get_loc();
        let from = self.convert_expr_to_import(from);
        let from = match from {
            Ok(from) => from,
            Err(()) => {
                self.add_diagnostic(ErrorCode::InvalidImport, loc);
                return Stmt::Error;
            }
        };
        let import_loc = start + iter.loc();

        let module = self.resolve_mod(&from);
        let (fid, path) = match module {
            Some(module) => module,
            None => {
                self.add_diagnostic(ErrorCode::ModuleNotFound, import_loc);
                return Stmt::Error;
            }
        };

        if let Some(loc) = self.import_stack.get(&fid) {
            self.add_diagnostic(ErrorCode::CircularImport(loc.clone()), import_loc);
            return Stmt::Error;
        }

        let prev_path = if let Some(path) = path.parent() {
            std::mem::replace(&mut self.relative_path, Some(PathBuf::from(path)))
        } else {
            None
        };

        let fid_prev = self.fid;
        self.parse_fid(fid, Some(import_loc.clone()));
        self.fid = fid_prev;

        self.relative_path = prev_path;

        Stmt::Import {
            loc: import_loc,
            fid,
            items: import_items,
            direct_import_mod: false,
        }
    }

    fn consume_for(&mut self, iter: &mut TokenIterator) -> Stmt {
        use Keyword::*;
        use Token::*;
        iter.next();
        let start = iter.loc();
        let vars = self.consume_expr(iter, 0, Some(Key(In)));
        // Consume "in"
        if self.consume_to_key(iter, In, Some((Key(For), start.clone()))) {
            return Stmt::Error;
        };
        let iterator = self.consume_expr(iter, 0, Some(Key(Do)));
        if self.consume_to_key(iter, Do, Some((Key(For), start.clone()))) {
            return Stmt::Error;
        };
        let mut body = vec![];
        loop {
            match iter.peek() {
                Some(Key(End)) => {
                    iter.next();
                    return Stmt::For {
                        loc: start + iter.loc(),
                        loop_variable: Box::new(vars),
                        iterator: Box::new(iterator),
                        body,
                    };
                }
                Some(_) => body.push(self.consume_stmt(iter, Some(Key(End)))),
                None => {
                    self.add_diagnostic(ErrorCode::UnexpectedEof, iter.loc());
                    return Stmt::Error;
                }
            }
        }
    }

    fn consume_loop(&mut self, iter: &mut TokenIterator) -> Stmt {
        use Keyword::*;
        use Token::*;
        let key = iter.next().cloned();
        let start = iter.loc();
        let condition = match key {
            Some(Key(Loop)) => None,
            Some(Key(Until)) => {
                let stmt = self.consume_expr(iter, 0, None);
                if matches!(iter.peek(), None) {
                    return Stmt::Error;
                };
                if self.consume_to_key(iter, Do, Some((Key(Until), start.clone()))) {
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
                        loc: start + iter.loc(),
                        condition,
                        body,
                    };
                }
                Some(_) => body.push(self.consume_stmt(iter, Some(Key(End)))),
                None => {
                    self.add_diagnostic(ErrorCode::UnexpectedEof, iter.loc());
                    return Stmt::Error;
                }
            }
        }
    }

    fn consume_fn(&mut self, iter: &mut TokenIterator) -> Expr {
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
                    self.add_diagnostic(
                        ErrorCode::UnexpectedToken(
                            Some(token.clone()),
                            Some(Id("<parameter>".to_string())),
                            Some((Key(Fn), start.clone())),
                        ),
                        iter.loc(),
                    );
                    iter.next();
                }
                None => {
                    self.add_diagnostic(ErrorCode::UnexpectedEof, iter.loc());
                    return Expr::Error;
                }
            }
        }
        let expr = self.consume_expr(iter, 3, None);
        Expr::Fn {
            loc: start + iter.loc(),
            parameters,
            body: Box::new(expr),
        }
    }

    fn consume_def(&mut self, iter: &mut TokenIterator) -> Stmt {
        use Keyword::*;
        use Operator::*;
        use Token::*;
        iter.next();
        let start = iter.loc();
        let variable = self.consume_expr(iter, 23, None);
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
                    self.add_diagnostic(
                        ErrorCode::UnexpectedToken(Some(token.clone()), None, None),
                        iter.next_loc(),
                    );
                    iter.next();
                }
                None => {
                    self.add_diagnostic(ErrorCode::UnexpectedEof, iter.loc());
                    return Stmt::Error;
                }
            }
        }
        let mut body = vec![];
        loop {
            match iter.peek() {
                Some(Key(End)) => {
                    iter.next();
                    return Stmt::Def {
                        loc: start + iter.loc(),
                        variable: Box::new(variable),
                        parameters,
                        body,
                    };
                }
                Some(_) => body.push(self.consume_stmt(iter, Some(Token::Key(Keyword::End)))),
                None => {
                    self.add_diagnostic(ErrorCode::UnexpectedEof, iter.loc());
                    return Stmt::Error;
                }
            }
        }
    }

    fn consume_block(&mut self, iter: &mut TokenIterator) -> Expr {
        iter.next();
        let start = iter.loc();
        let mut body: Vec<Stmt> = vec![];
        loop {
            match iter.peek() {
                Some(Token::Key(Keyword::End)) => {
                    iter.next();
                    let end = iter.loc();
                    return Expr::Block {
                        loc: start + end,
                        body,
                    };
                }
                Some(_) => {
                    let stmt = self.consume_stmt(iter, Some(Token::Key(Keyword::End)));
                    body.push(stmt);
                }
                None => {
                    let end = iter.loc();
                    self.add_diagnostic(ErrorCode::UnexpectedEof, end);
                    return Expr::Error;
                }
            }
        }
    }

    fn consume_table(&mut self, iter: &mut TokenIterator) -> Expr {
        use Operator::*;
        use Token::*;
        iter.next();
        let start = iter.loc();

        if let Some(Op(RBrc)) = iter.peek() {
            iter.next();
            let end = iter.loc();
            return Expr::Const {
                loc: start + end,
                value: Const::Table(vec![]),
            };
        }
        let mut key_vals: Vec<(String, Expr, Loc)> = vec![];
        let mut content = self.consume_expr(iter, 0, Some(Op(RBrc)));
        if self.consume_to_op(iter, RBrc, Some((Op(LBrc), start.clone()))) {
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
                    let (name, name_loc) = match lhs {
                        Expr::Id { loc, name } => (name, loc),
                        _ => {
                            self.add_diagnostic(ErrorCode::InvalidTableKey, lhs.get_loc());
                            return Expr::Error;
                        }
                    };
                    if let Some((name, _, prev_loc)) =
                        key_vals.iter().find(|(prev, _, _)| prev == &name)
                    {
                        self.add_diagnostic(
                            ErrorCode::DuplicateKey(prev_loc.clone(), name.clone()),
                            name_loc,
                        );
                        return Expr::Error;
                    }
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
                            let (name, name_loc) = match lhs {
                                Expr::Id { loc, name } => (name, loc),
                                _ => {
                                    self.add_diagnostic(ErrorCode::InvalidTableKey, lhs.get_loc());
                                    return Expr::Error;
                                }
                            };
                            if let Some((name, _, prev_loc)) =
                                key_vals.iter().find(|(prev, _, _)| prev == &name)
                            {
                                self.add_diagnostic(
                                    ErrorCode::DuplicateKey(prev_loc.clone(), name.clone()),
                                    name_loc,
                                );
                                return Expr::Error;
                            }
                            key_vals.push((name, *rhs, loc));
                        }
                        _ => {
                            self.add_diagnostic(ErrorCode::InvalidTableFormat, loc);
                            return Expr::Error;
                        }
                    }
                    content = *lhs;
                }
                Expr::Error => return content,
                _ => {
                    self.add_diagnostic(ErrorCode::InvalidTableFormat, content.get_loc());
                    return Expr::Error;
                }
            }
        }

        let end = iter.loc();
        key_vals.reverse();
        Expr::Const {
            loc: start + end,
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
            Some(Key(Fn)) => self.consume_fn(iter),
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
                    let lhs = self.consume_expr(iter, 0, Some(Op(RPar)));
                    if self.consume_to_op(iter, RPar, Some((Op(LPar), start.clone()))) {
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
                let rhs = self.consume_expr(iter, precedence_prefix(), not_take_on_error.clone());
                let end = iter.loc();
                Expr::Prefix {
                    loc: start.clone() + end,
                    op,
                    rhs: Box::new(rhs),
                }
            }
            Some(Key(If)) => self.consume_if(iter),
            Some(Key(Begin)) => self.consume_block(iter),
            Some(Op(LBrk)) => {
                iter.next();
                let previous_loc = iter.loc();
                let mut exprs = vec![];
                if let Some(Op(RBrk)) = iter.peek() {
                    iter.next();
                } else {
                    let mut parameters = self.consume_expr(iter, 0, Some(Op(RBrk)));
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
                    if self.consume_to_op(iter, RBrk, Some((Op(LBrk), previous_loc))) {
                        return Expr::Error;
                    };
                }
                Expr::Const {
                    loc: start.clone() + iter.loc(),
                    value: Const::List(exprs),
                }
            }
            Some(Op(LBrc)) => self.consume_table(iter),
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
                    self.add_diagnostic(ErrorCode::MissingExpr(iter.loc()), start);
                } else {
                    let token = token.clone();
                    iter.next();
                    self.add_diagnostic(ErrorCode::UnexpectedToken(Some(token), None, None), start);
                }
                return Expr::Error;
            }
            None => {
                self.add_diagnostic(ErrorCode::UnexpectedEof, start);
                return Expr::Error;
            }
        };

        loop {
            let op = match iter.peek2() {
                (Some(Op(SemiColon)), _) => return lhs,
                // Allow (1..) format
                (Some(Op(Range)), Some(next)) if !matches!(next, expr_start_pattern!()) => {
                    iter.next();
                    lhs = Expr::OpenRange {
                        loc: start.clone() + iter.loc(),
                        lhs: Box::new(lhs),
                    };
                    continue;
                }
                // Allow [1,2,] format
                (Some(Op(Comma)), Some(Op(RPar | RBrk | RBrc))) => {
                    iter.next();
                    break;
                }
                (Some(Op(op @ (LBrk | LPar))), _) => {
                    let op = match op {
                        LPar => OpPostfix::Call,
                        LBrk => OpPostfix::Index,
                        _ => unreachable!(),
                    };
                    let precedence = precedence_postfix();
                    if precedence > min_precedence {
                        match op {
                            OpPostfix::Call => {
                                iter.next();
                                let previous_loc = iter.loc();
                                let mut exprs = vec![];
                                if let Some(Op(RPar)) = iter.peek() {
                                    iter.next();
                                } else {
                                    let mut parameters = self.consume_expr(iter, 0, Some(Op(RPar)));
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
                                        RPar,
                                        Some((Op(LPar), previous_loc)),
                                    ) {
                                        return Expr::Error;
                                    };
                                }
                                lhs = Expr::Call {
                                    loc: start.clone() + iter.loc(),
                                    lhs: Box::new(lhs),
                                    parameters: exprs,
                                };
                                continue;
                            }
                            OpPostfix::Index => {
                                iter.next();
                                let match_loc = iter.loc();
                                let expr = self.consume_expr(iter, 0, Some(Op(RBrk)));
                                if self.consume_to_op(iter, RBrk, Some((Op(LBrk), match_loc))) {
                                    return Expr::Error;
                                };
                                lhs = Expr::Index {
                                    loc: start.clone() + iter.loc(),
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
                    Op(Is) => OpInfix::Is,
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
                    Op(DoubleColon) => OpInfix::DoubleColon,
                    Op(LArrow) => OpInfix::LArrow,
                    _ => return lhs,
                },
                _ => return lhs,
            };

            let precedence = precedence_infix(op);
            if precedence.0 < min_precedence {
                break;
            }

            iter.next();

            let rhs = self.consume_expr(iter, precedence.1, not_take_on_error.clone());

            lhs = Expr::Infix {
                loc: start.clone() + iter.loc(),
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }

        lhs
    }

    pub fn add_diagnostic(&mut self, error: ErrorCode, loc: Loc) {
        let eof = matches!(
            error,
            ErrorCode::UnexpectedEof | ErrorCode::UnexpectedToken(None, _, _)
        );
        let diag = match error {
        ErrorCode::UnexpectedToken(met, expected, to_match) => {
            let mut diagnostic = Diagnostic::error().with_code("E1000");
            if let Some(t) = met {
                diagnostic = diagnostic
                    .with_message(format!("Unexpected token `{t}`"))
                    .with_labels(vec![Label::primary(self.fid, loc)]);
            } else {
                diagnostic = diagnostic
                    .with_message("End of file while parsing")
                    .with_labels(vec![Label::primary(self.fid, loc)]);
            }
            if let Some(t) = expected {
                diagnostic = diagnostic.with_notes(vec![format!("Consider add a `{t}` here")]);
            }
            if let Some((t, loc)) = to_match {
                diagnostic = diagnostic.with_labels(vec![
                    Label::secondary(self.fid, loc).with_message(format!("Due to `{t}` here"))
                ]);
            }
            diagnostic
        }
        ErrorCode::UnexpectedEof => Diagnostic::error()
            .with_code("E1001")
            .with_message("Unexpected end of file here")
            .with_labels(vec![Label::primary(self.fid, loc)]),
        ErrorCode::MissingExpr(loc_pre) => Diagnostic::error()
            .with_code("E1002")
            .with_message("Missing expression here")
            .with_labels(vec![
                Label::primary(self.fid, loc),
                Label::secondary(self.fid, loc_pre).with_message("Previous token here"),
            ]),
        ErrorCode::InvalidTableKey => Diagnostic::error()
            .with_code("E1003")
            .with_message("Table key must be an identifier")
            .with_labels(vec![Label::primary(self.fid, loc)]),
        ErrorCode::ModuleNotFound => Diagnostic::error()
            .with_code("E1004")
            .with_message("Module can not be found")
            .with_labels(vec![Label::primary(self.fid, loc)])
            .with_notes(vec!["Looking for `<module>.dm` or `<module>/mod.dm`".to_string()]),
        ErrorCode::CircularImport(prev_loc) => {
            let mut diag = Diagnostic::error()
                .with_code("E1005")
                .with_message("Circular import detected")
                .with_labels(vec![Label::primary(self.fid, loc)]);
            if let Some(prev_loc) = prev_loc {
                diag = diag.with_labels(vec![Label::secondary(prev_loc.fid, prev_loc).with_message("Previously imported here")]);
            }
            diag
        },
        ErrorCode::InvalidTableFormat => Diagnostic::error()
            .with_code("E1006")
            .with_message("Invalid syntax in table")
            .with_labels(vec![Label::primary(self.fid, loc)])
            .with_notes(vec!["Table should look like `{<identifier>=<expression>, <identifier>=<expression>, ...}`".to_string()]),
        ErrorCode::DuplicateKey(prev_loc, name) => Diagnostic::error()
            .with_code("E1007")
            .with_message(format!("Duplicate table key `{name}`"))
            .with_labels(vec![Label::primary(self.fid, loc), Label::secondary(self.fid, prev_loc).with_message("Also defined here")]),
        ErrorCode::InvalidImport => Diagnostic::error()
            .with_code("E1008")
            .with_message("Not allowed in import statement")
            .with_labels(vec![Label::primary(self.fid, loc)]),
    };

        self.file_manager.add_diagnostic(diag, eof);
    }
}
