mod ast;
mod error;
use crate::diagnostic::{Diagnoser, DisplayableOsString, Loc, SharedFile};

use self::error::{to_diagnostic, ErrorCode};

use super::{
    lexer::{Keyword, Operator, Token},
    util::TokenIterator,
    Lexer,
};
use ahash::AHashMap;
use ast::{Ast, Const, Expr, Expr_, OpInfix, OpPostfix, OpPrefix, Stat, Stat_};
use std::{
    ffi::{OsStr, OsString},
    fs,
    mem::Discriminant,
};

const fn precedence_infix(op: OpInfix) -> (u16, u16) {
    use OpInfix::*;
    match op {
        Assign => (1, 2),
        Comma => (3, 4),
        Range => (5, 6),
        Or => (7, 8),
        And => (9, 10),
        Eq | Ne | Le | Lt | Gt | Ge => (11, 12),
        Plus | Minus => (13, 14),
        Mul | Div | DivFloor | Mod => (15, 16),
        Exp => (17, 18),
        Member => (19, 20),
    }
}

const fn precedence_prefix() -> u16 {
    100
}

const fn precedence_postfix() -> u16 {
    101
}

/// A pattern match all possible start of an expression
macro_rules! expr_start_pattern {
    () => {
        Token::Key(
            Keyword::Def
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
/// // To parse a single file
/// parser.parse(OsStr::new("/path/to/code.dm"));
/// // Check if something went wrong
/// if parser.diagnostic_count() > 0 {
///     parser.print_diagnoses();
///     parser.print_diagnoses_summary();
/// }
///
/// // Retrieve AST tree
/// let _ast = parser.get_incremental();
///
/// // Or parse a &str
/// parser.parse_str(OsStr::new("/path/to/code.dm"), "<code here>");
/// let _ast = parser.get_incremental();
///
/// ```
///
pub struct Parser {
    diagnoser: Diagnoser,
    files: AHashMap<OsString, Lexer>,
    ast: Ast,
    current_file_id: usize,
    consumed: usize,
    has_eof_error: bool,
    has_non_eof_error: bool,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            files: AHashMap::new(),
            ast: Ast::default(),
            diagnoser: Diagnoser::new(),
            current_file_id: 0,
            consumed: 0,
            has_eof_error: false,
            has_non_eof_error: false,
        }
    }

    /// Get statements have not been consumed before
    pub fn get_incremental(&mut self) -> std::iter::Skip<std::slice::Iter<Stat>> {
        let last = self.consumed;
        self.consumed = self.ast.statements.len();
        self.ast.statements.iter().skip(last)
    }

    pub fn input_can_continue(&self) -> bool {
        if self.files.iter().all(|(_, x)| !x.has_non_eof_error())
            && self.files.iter().any(|(_, x)| x.has_eof_error())
        {
            return true;
        }
        self.has_eof_error && !self.has_non_eof_error
    }

    fn add_diagnostic(&mut self, error: ErrorCode, loc: Loc) {
        match error {
            ErrorCode::UnexpectedEof | ErrorCode::UnexpectedToken(None, _, _) => {
                if self.has_eof_error {
                    return;
                } else {
                    self.has_eof_error = true
                }
            }
            _ => self.has_non_eof_error = true,
        }
        let diag = to_diagnostic(error, loc, self.current_file_id);
        self.diagnoser.push(diag);
    }

    pub fn clear_diagnoses(&mut self) {
        self.has_non_eof_error = false;
        self.has_eof_error = false;
        for (_, lexer) in &mut self.files {
            lexer.clear_diagnoses();
        }
        self.diagnoser.clear()
    }

    pub fn print_diagnoses(&self) {
        for (_, lexer) in &self.files {
            lexer.print_diagnoses();
        }
        self.diagnoser.print()
    }

    pub fn print_diagnoses_summary(&self) {
        let mut errors = self.diagnoser.error_count();
        let mut warnings = self.diagnoser.warning_count();
        for (_, lexer) in &self.files {
            errors += lexer.error_count();
            warnings += lexer.warning_count();
        }
        println!(
            "Summary: {} errors and {} warnings are generated while parsing.",
            errors, warnings
        );
    }

    pub fn diagnostic_count(&self) -> usize {
        let mut count = self.diagnoser.count();
        for (_, lexer) in &self.files {
            count += lexer.diagnostic_count();
        }
        count
    }

    /// Parse a file.
    ///
    /// Return true if errors are encountered.
    pub fn parse(&mut self, filepath: &OsStr) -> bool {
        let path = fs::canonicalize(filepath);
        let path = match path {
            Ok(path) => path,
            Err(err) => {
                self.diagnoser.push(to_diagnostic(
                    ErrorCode::NoSuchFile(DisplayableOsString::from(filepath), err),
                    0..0,
                    0,
                ));
                return true;
            }
        };
        let path_str = path.as_os_str();
        let content = SharedFile::new(path_str);
        let content = match content {
            Ok(content) => content,
            Err(err) => {
                self.diagnoser.push(to_diagnostic(
                    ErrorCode::NoSuchFile(DisplayableOsString::from(path_str), err),
                    0..0,
                    0,
                ));
                return true;
            }
        };
        self.parse_file(path_str, content);
        self.diagnostic_count() != 0
    }

    /// Parse a string and append to current parse tree
    pub fn parse_str(&mut self, path: &OsStr, content: &str) {
        self.parse_file(path, SharedFile::from_str(content))
    }

    fn parse_file(&mut self, path: &OsStr, content: SharedFile) {
        let lexer = Lexer::new(path.to_os_string(), content.clone());
        self.current_file_id = self.diagnoser.new_file(path.to_os_string(), content);
        let mut iter = lexer.iter();

        while iter.peek().is_some() {
            let stat = self.consume_stat(&mut iter, None);
            self.ast.statements.push(stat);
        }
        self.files.insert(path.to_os_string(), lexer);
    }

    fn consume_stat(&mut self, iter: &mut TokenIterator, not_take_on_error: Option<Token>) -> Stat {
        use Keyword::*;
        use Token::*;
        let start = iter.next_loc();
        match iter.peek() {
            Some(Key(Break)) => {
                iter.next();
                Stat {
                    loc: start,
                    val: Stat_::Break,
                }
            }
            Some(Key(Continue)) => {
                iter.next();
                Stat {
                    loc: start,
                    val: Stat_::Break,
                }
            }
            Some(Key(Return)) => {
                iter.next();
                match iter.peek() {
                    Some(expr_start_pattern!()) => {
                        let expr = self.consume_expr(iter, 0, not_take_on_error);
                        let end = iter.loc();
                        Stat {
                            loc: start.start..end.end,
                            val: Stat_::Return(Some(expr)),
                        }
                    }
                    _ => Stat {
                        loc: start,
                        val: Stat_::Return(None),
                    },
                }
            }
            Some(expr_start_pattern!()) => {
                let expr = self.consume_expr(iter, 0, not_take_on_error);
                let end = iter.loc();
                Stat {
                    loc: start.start..end.end,
                    val: Stat_::Expr(expr),
                }
            }
            Some(Key(Class)) => self.consume_class(iter),
            Some(Key(Loop | Until)) => self.consume_loop(iter),
            Some(Key(For)) => self.consume_for(iter),
            Some(token) => {
                let token = token.clone();
                iter.next();
                self.add_diagnostic(
                    ErrorCode::UnexpectedToken(Some(token), None, None),
                    start.clone(),
                );
                Stat {
                    loc: start,
                    val: Stat_::Error,
                }
            }
            None => {
                self.add_diagnostic(ErrorCode::UnexpectedEof, start.clone());
                Stat {
                    loc: start,
                    val: Stat_::Error,
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

    fn consume_cond_then(&mut self, iter: &mut TokenIterator) -> Option<Expr> {
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
        let mut exprs: Vec<Expr> = vec![];
        match self.consume_cond_then(iter) {
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
        let mut block: Vec<Stat> = vec![];
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
                    match self.consume_cond_then(iter) {
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
                                let stat = self.consume_stat(iter, Some(Key(End)));
                                block.push(stat);
                            }
                            None => {
                                let end = iter.loc();
                                self.add_diagnostic(ErrorCode::UnexpectedEof, end);
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
                    let stat = self.consume_stat(iter, Some(Key(Else)));
                    block.push(stat);
                }
                None => {
                    let end = iter.loc();
                    self.add_diagnostic(
                        ErrorCode::UnexpectedToken(None, Some(Key(Else)), Some((Key(If), start))),
                        end,
                    );
                    return Expr {
                        loc: 0..0,
                        val: Expr_::Error,
                    };
                }
            }
        }
    }

    fn consume_for(&mut self, iter: &mut TokenIterator) -> Stat {
        use Keyword::*;
        use Token::*;
        iter.next();
        let start = iter.loc();
        let vars = self.consume_expr(iter, 0, Some(Key(In)));
        // Consume "in"
        if self.has_eof_error || self.consume_to_key(iter, In, Some((Key(For), start.clone()))) {
            return Stat {
                loc: start.start..iter.loc().end,
                val: Stat_::Error,
            };
        };
        let iterator = self.consume_expr(iter, 0, Some(Key(Do)));
        if self.has_eof_error || self.consume_to_key(iter, Do, Some((Key(For), start.clone()))) {
            return Stat {
                loc: start.start..iter.loc().end,
                val: Stat_::Error,
            };
        };
        let mut stats = vec![];
        loop {
            match iter.peek() {
                Some(Key(End)) => {
                    iter.next();
                    return Stat {
                        loc: start.start..iter.loc().end,
                        val: Stat_::For(Box::new(vars), Box::new(iterator), stats),
                    };
                }
                Some(_) => stats.push(self.consume_stat(iter, Some(Key(End)))),
                None => {
                    self.add_diagnostic(ErrorCode::UnexpectedEof, iter.loc());
                    return Stat {
                        loc: start.start..iter.loc().end,
                        val: Stat_::Error,
                    };
                }
            }
        }
    }

    fn consume_loop(&mut self, iter: &mut TokenIterator) -> Stat {
        use Keyword::*;
        use Token::*;
        let key = iter.next().cloned();
        let start = iter.loc();
        let cond = match key {
            Some(Key(Loop)) => None,
            Some(Key(Until)) => {
                let stat = self.consume_expr(iter, 0, None);
                // Consume "do" if there are no EOF error
                // This is to prevent multiple EOF error spamming
                if self.has_eof_error
                    || self.consume_to_key(iter, Do, Some((Key(Until), start.clone())))
                {
                    return Stat {
                        loc: start.start..iter.loc().end,
                        val: Stat_::Error,
                    };
                };
                Some(stat)
            }
            _ => unreachable!(),
        };
        let mut stats = vec![];
        loop {
            match iter.peek() {
                Some(Key(End)) => {
                    iter.next();
                    return Stat {
                        loc: start.start..iter.loc().end,
                        val: Stat_::Loop(cond, stats),
                    };
                }
                Some(_) => stats.push(self.consume_stat(iter, Some(Key(End)))),
                None => {
                    self.add_diagnostic(ErrorCode::UnexpectedEof, iter.loc());
                    return Stat {
                        loc: start.start..iter.loc().end,
                        val: Stat_::Error,
                    };
                }
            }
        }
    }

    fn consume_class(&mut self, iter: &mut TokenIterator) -> Stat {
        use Token::*;

        iter.next();
        let start = iter.loc();
        let name = match iter.peek() {
            Some(Id(name)) => {
                let name = name.clone();
                iter.next();
                name
            }
            Some(Key(Keyword::End)) => {
                iter.next();
                let end = iter.loc();
                self.add_diagnostic(ErrorCode::MissingStat(start.clone()), end.clone());
                return Stat {
                    loc: start.start..end.end,
                    val: Stat_::Class("Error".to_string(), vec![], vec![]),
                };
            }
            Some(token) => {
                let token = token.clone();
                iter.next();
                let loc = iter.loc();
                self.add_diagnostic(
                    ErrorCode::UnexpectedToken(
                        Some(token),
                        Some(Id("name of this class".to_string())),
                        Some((Key(Keyword::Class), start.clone())),
                    ),
                    loc,
                );
                "Error".to_string()
            }
            None => {
                self.add_diagnostic(ErrorCode::UnexpectedEof, iter.loc());
                let end = iter.loc();
                return Stat {
                    loc: start.start..end.end,
                    val: Stat_::Class("Error".to_string(), vec![], vec![]),
                };
            }
        };

        let mut members: Vec<(String, Loc)> = vec![];
        let mut methods: Vec<Expr> = vec![];
        loop {
            match iter.peek() {
                Some(Key(Keyword::End)) => {
                    iter.next();
                    let end = iter.loc();
                    return Stat {
                        loc: start.start..end.end,
                        val: Stat_::Class(name, members, methods),
                    };
                }
                Some(Id(name)) => {
                    let loc = iter.next_loc();
                    members.push((name.clone(), loc));
                    iter.next();
                }
                Some(Key(Keyword::Def)) => {
                    let expr = self.consume_expr(iter, 0, Some(Key(Keyword::End)));
                    methods.push(expr);
                }
                Some(token) => {
                    let token = token.clone();
                    iter.next();
                    let loc = iter.loc();
                    self.add_diagnostic(
                        ErrorCode::UnexpectedToken(
                            Some(token),
                            None,
                            Some((Key(Keyword::Class), start.clone())),
                        ),
                        loc,
                    );
                    iter.next();
                }
                None => {
                    self.add_diagnostic(ErrorCode::UnexpectedEof, iter.loc());
                    let end = iter.loc();
                    return Stat {
                        loc: start.start..end.end,
                        val: Stat_::Class(name, members, methods),
                    };
                }
            }
        }
    }

    fn consume_def(&mut self, iter: &mut TokenIterator) -> Expr {
        iter.next();
        let start = iter.loc();
        let name = if let Some(Token::Id(name)) = iter.peek() {
            let name = name.clone();
            iter.next();
            Some(name)
        } else {
            None
        };
        let eof = self.consume_to_op(
            iter,
            Operator::LPar,
            Some((Token::Key(Keyword::Def), start.clone())),
        );
        if eof {
            return Expr {
                loc: start.start..iter.loc().end,
                val: Expr_::Error,
            };
        }
        let decl_start = iter.loc();
        let decl = if let Some(Token::Op(Operator::RPar)) = iter.peek() {
            iter.next();
            None
        } else {
            let decl = self.consume_expr(iter, 0, Some(Token::Op(Operator::RPar)));
            let eof = self.consume_to_op(
                iter,
                Operator::RPar,
                Some((Token::Op(Operator::LPar), decl_start)),
            );
            if eof {
                return Expr {
                    loc: start.start..iter.loc().end,
                    val: Expr_::Error,
                };
            }
            Some(Box::new(decl))
        };
        let body = self.consume_stat(iter, None);
        Expr {
            loc: start.start..iter.loc().end,
            val: Expr_::Def(name, decl, Box::new(body)),
        }
    }

    fn consume_block(&mut self, iter: &mut TokenIterator) -> Expr {
        iter.next();
        let start = iter.loc();
        let mut stats: Vec<Stat> = vec![];
        loop {
            match iter.peek() {
                Some(Token::Key(Keyword::End)) => {
                    iter.next();
                    let end = iter.loc();
                    return Expr {
                        loc: start.start..end.end,
                        val: Expr_::Block(stats),
                    };
                }
                Some(_) => {
                    let stat = self.consume_stat(iter, Some(Token::Key(Keyword::End)));
                    stats.push(stat);
                }
                None => {
                    let end = iter.loc();
                    self.add_diagnostic(ErrorCode::UnexpectedEof, end.clone());
                    return Expr {
                        loc: start.start..end.end,
                        val: Expr_::Block(stats),
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
        min_precedence: u16,
        not_take_on_error: Option<Token>,
    ) -> Expr {
        use Keyword::*;
        use Operator::*;
        use Token::*;
        let mut start = iter.next_loc();
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
            Some(Key(Def)) => self.consume_def(iter),
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
                let lhs = self.consume_expr(iter, 0, Some(Op(RPar)));
                self.consume_to_op(iter, RPar, Some((Op(LPar), start.clone())));
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
                let rhs = self.consume_expr(iter, precedence_prefix(), not_take_on_error.clone());
                let end = iter.loc();
                Expr {
                    loc: start.start..end.end,
                    val: Expr_::Prefix(op, Box::new(rhs)),
                }
            }
            Some(Key(If)) => self.consume_if(iter),
            Some(Key(Begin)) => self.consume_block(iter),
            Some(Op(LBrk)) => {
                iter.next();
                // Test for empty list
                if let Some(Op(Operator::RBrk)) = iter.peek() {
                    iter.next();
                    let end = iter.loc();
                    Expr {
                        loc: start.start..end.end,
                        val: Expr_::Const(Const::List(None)),
                    }
                } else {
                    let expr = self.consume_expr(iter, 0, Some(Op(RBrk)));
                    self.consume_to_op(iter, RBrk, Some((Op(LBrk), start.clone())));
                    let end = iter.loc();
                    Expr {
                        loc: start.start..end.end,
                        val: Expr_::Const(Const::List(Some(Box::new(expr)))),
                    }
                }
            }
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
                    self.add_diagnostic(ErrorCode::MissingExpr(iter.loc()), start.clone())
                } else {
                    let token = token.clone();
                    iter.next();
                    self.add_diagnostic(
                        ErrorCode::UnexpectedToken(Some(token), None, None),
                        start.clone(),
                    );
                }
                return Expr {
                    loc: start,
                    val: Expr_::Error,
                };
            }
            None => {
                self.add_diagnostic(ErrorCode::UnexpectedEof, start.clone());
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
                        Some(token) => {
                            let token = token.clone();
                            iter.next();
                            let loc = iter.loc();
                            iter.next();
                            let loc_token = iter.loc();
                            self.add_diagnostic(
                                ErrorCode::UnexpectedToken(
                                    Some(token),
                                    None,
                                    Some((Op(Colon), loc)),
                                ),
                                loc_token,
                            );
                            return lhs;
                        }
                        None => {
                            iter.next();
                            let loc = iter.loc();
                            self.add_diagnostic(ErrorCode::UnexpectedEof, loc);
                            return lhs;
                        }
                    };
                    let precedence = precedence_postfix();
                    if precedence > min_precedence {
                        match op {
                            OpPostfix::Call => {
                                iter.next();
                                let loc = iter.loc();
                                iter.next();
                                let match_loc = iter.loc();
                                // Test for empty function call
                                if matches!(iter.peek(), Some(Op(RPar))) {
                                    iter.next();
                                    let end = iter.loc();
                                    lhs = Expr {
                                        loc: loc.start..end.end,
                                        val: Expr_::Postfix(OpPostfix::Call, Box::new(lhs), None),
                                    };
                                    start = end;
                                } else {
                                    let expr = self.consume_expr(iter, 0, Some(Op(RPar)));
                                    self.consume_to_op(iter, RPar, Some((Op(LPar), match_loc)));
                                    let end = iter.loc();

                                    lhs = Expr {
                                        loc: loc.start..end.end,
                                        val: Expr_::Postfix(
                                            OpPostfix::Call,
                                            Box::new(lhs),
                                            Some(Box::new(expr)),
                                        ),
                                    };
                                    start = iter.next_loc();
                                }
                                continue;
                            }
                            OpPostfix::Index => {
                                iter.next();
                                let loc = iter.loc();
                                iter.next();
                                let match_loc = iter.loc();
                                let expr = self.consume_expr(iter, 0, Some(Op(RBrk)));
                                self.consume_to_op(iter, RBrk, Some((Op(LBrk), match_loc)));
                                let end = iter.loc();

                                lhs = Expr {
                                    loc: loc.start..end.end,
                                    val: Expr_::Postfix(
                                        OpPostfix::Index,
                                        Box::new(lhs),
                                        Some(Box::new(expr)),
                                    ),
                                };
                                start = end;
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

            let rhs = self.consume_expr(iter, precedence.1, not_take_on_error.clone());
            let end = iter.loc();

            lhs = Expr {
                loc: start.start..end.end,
                val: Expr_::Infix(op, Box::new(lhs), Box::new(rhs)),
            };
            start = end;
        }

        lhs
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    fn test_str(code: &str, should_fail: bool) {
        let mut parser = Parser::new();
        parser.parse_str(OsStr::new("test.dm"), code);
        println!("{:#?}", parser.ast.statements);
        if !should_fail && parser.diagnostic_count() > 0 {
            parser.diagnoser.print();
        }
        if should_fail {
            assert!(parser.diagnostic_count() > 0);
        } else {
            assert!(parser.diagnostic_count() == 0);
        }
    }

    #[test]
    fn test_expr() {
        let code = "a,b, nil = not 32 * 15$()+8.9e13//(12+\"asdf\") or false and -23";
        let code = SharedFile::from_str(code);
        let lexer = Lexer::new(OsString::from_str("test.dm").unwrap(), code);
        let mut parser = Parser::new();
        let expr = parser.consume_expr(&mut lexer.iter(), 0, None);
        println!("{expr:?}");
        parser.diagnoser.print();
        assert_eq!(parser.diagnostic_count(), 0);
    }

    #[test]
    fn test_expr_postfix_ambiguous() {
        let code = "0,a $ (1,2,3) (3,4)$[2-1]+0.333//[0,1,2]$[1][a,v,b]";
        let mut parser = Parser::new();
        parser.parse_str(OsStr::new("test.dm"), code);
        println!("{:#?}", parser.ast.statements);
        if parser.diagnostic_count() > 0 {
            parser.diagnoser.print();
        }
        assert_eq!(parser.ast.statements.len(), 3);
    }

    #[test]
    fn test_valid() {
        test_str("a$()", false);
        test_str("[]", false);
        test_str("", false);
    }

    #[test]
    fn test_invalid() {
        test_str(">> <<", true);
        test_str("a$[]", true);
        test_str("[1,2,]", true);
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
        test_str("def a a+1", true);
        test_str("def a() a+1", false);
        test_str("def a(a) a+1", false);
        test_str("def () 1", false);
        test_str("def (a, b, c) a+b+1", false);
    }

    #[test]
    fn test_statement() {
        test_str("begin return end", false);
        test_str("begin return [1,2,3] return def ()[] end", false);
        test_str("begin break continue end", false);
        test_str("if false then return else end", false);
        test_str(
            "if false then return [1,2,3] return def ()[] else end",
            false,
        );
        test_str("if false then break continue else end", false);
        test_str("return", false);
        test_str("def () return 1", false);
        test_str("def () return ", false);
    }

    #[test]
    fn test_class() {
        test_str("class end", true);
        test_str("class a end", false);
        test_str("class c def ()[] end", false);
        test_str("class c a b c def ()begin return 1 end s end", false);
        test_str("class a a end", false);
        test_str("class ∂ def ()a a end", false);
    }

    #[test]
    fn test_loop() {
        test_str("loop end", false);
        test_str("loop continue false true return end", false);
        test_str("until end", true);
        test_str("until iii end", true);
        test_str("until true do end", false);
        test_str("until true do nil end", false);
        test_str("until [1,2,3] do xxx yyy break end", false);
    }
}
