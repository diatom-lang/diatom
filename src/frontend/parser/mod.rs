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

const fn precedence_infix(op: OpInfix) -> (u32, u32) {
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
    }
}

const fn precedence_prefix() -> u32 {
    100
}

const fn precedence_postfix() -> u32 {
    101
}

/// The parser for Diatom.
///
/// # Errors
/// Error code `E1000` to `E1999` is reserved for `Parser`
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
                self.has_eof_error = true
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
    /// Return Err(()) if file can not be read.
    pub fn parse(&mut self, filepath: &OsStr) -> Result<(), ()> {
        let path = fs::canonicalize(filepath);
        let path = match path {
            Ok(path) => path,
            Err(err) => {
                self.diagnoser.push(to_diagnostic(
                    ErrorCode::NoSuchFile(DisplayableOsString::from(filepath), err),
                    0..0,
                    0,
                ));
                return Err(());
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
                return Err(());
            }
        };
        self.parse_file(path_str, content);
        Ok(())
    }

    /// Parse a string and append to current parse tree
    pub fn parse_str(&mut self, path: &OsStr, content: &str) {
        self.parse_file(path, SharedFile::from_str(content))
    }

    fn parse_file(&mut self, path: &OsStr, content: SharedFile) {
        use Keyword::*;
        use Token::*;

        let lexer = Lexer::new(path.to_os_string(), content.clone());
        self.current_file_id = self.diagnoser.new_file(path.to_os_string(), content);
        let mut iter = lexer.iter();

        loop {
            let start = iter.loc();
            let stat = match iter.peek() {
                Some(Key(Class)) => self.consume_class(&mut iter),
                Some(Key(Def)) => self.consume_def(&mut iter),
                Some(_) => {
                    let expr = self.consume_expr(&mut iter, 0);
                    let end = iter.loc();
                    Stat::new(Stat_::Expr(expr), start.start..end.end)
                }
                None => {
                    break;
                }
            };
            self.ast.statements.push(stat);
        }
        self.files.insert(path.to_os_string(), lexer);
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
            let t = iter.next().map(|x| x.clone());
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
            let t = iter.next().map(|x| x.clone());
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

    fn consume_cond_then(&mut self, iter: &mut TokenIterator) -> Option<Box<Expr>> {
        use Keyword::*;
        use Token::*;
        let start = iter.loc();
        let condition;
        // match `condition`
        if !matches!(iter.peek(), Some(Key(Then))) {
            condition = Some(self.consume_expr(iter, 0));
        } else {
            // empty condition, return an error expression to prevent multiple error reports
            let end = iter.loc();
            iter.next();
            let loc = iter.loc();
            self.add_diagnostic(ErrorCode::MissingExpr(start.clone()), loc);
            return Some(Box::new(Expr {
                loc: start.start..end.end,
                val: Expr_::Error,
            }));
        }
        // match `then`
        if !self.consume_to_key(iter, Then, Some((Key(If), start.clone()))) {
            return condition;
        } else {
            return None;
        }
    }

    fn consume_if(&mut self, iter: &mut TokenIterator) -> Box<Expr> {
        use Keyword::*;
        use Token::*;
        iter.next();
        let start = iter.loc();
        let mut exprs: Vec<Box<Expr>> = vec![];
        match self.consume_cond_then(iter) {
            Some(expr) => exprs.push(expr),
            None => {
                let end = iter.loc();
                return Box::new(Expr {
                    loc: start.start..end.end,
                    val: Expr_::Error,
                });
            }
        }
        // match block
        let mut block: Vec<Box<Expr>> = vec![];
        let mut block_start = iter.next_loc();
        loop {
            match iter.peek() {
                Some(Key(Elsif)) => {
                    exprs.push(Box::new(Expr {
                        loc: block_start.start..iter.loc().end,
                        val: Expr_::Block(block),
                    }));
                    block = vec![];
                    iter.next();
                    match self.consume_cond_then(iter) {
                        Some(expr) => {
                            exprs.push(expr);
                        }
                        None => {
                            let end = iter.loc();
                            return Box::new(Expr {
                                loc: start.start..end.end,
                                val: Expr_::Error,
                            });
                        }
                    }
                    block_start = iter.next_loc();
                }
                Some(Key(Else)) => {
                    exprs.push(Box::new(Expr {
                        loc: block_start.start..iter.loc().end,
                        val: Expr_::Block(block),
                    }));
                    block = vec![];
                    iter.next();
                    loop {
                        match iter.peek() {
                            Some(Key(End)) => {
                                exprs.push(Box::new(Expr {
                                    loc: block_start.start..iter.loc().end,
                                    val: Expr_::Block(block),
                                }));
                                iter.next();
                                let end = iter.loc();
                                return Box::new(Expr {
                                    loc: start.start..end.end,
                                    val: Expr_::If(exprs),
                                });
                            }
                            Some(_) => {
                                let expr = self.consume_expr(iter, 0);
                                block.push(expr);
                            }
                            None => {
                                let end = iter.loc();
                                self.add_diagnostic(ErrorCode::UnexpectedEof, end.clone());
                                return Box::new(Expr {
                                    loc: 0..0,
                                    val: Expr_::Error,
                                });
                            }
                        }
                    }
                }
                Some(_) => {
                    let expr = self.consume_expr(iter, 0);
                    block.push(expr);
                }
                None => {
                    let end = iter.loc();
                    self.add_diagnostic(ErrorCode::UnexpectedEof, end.clone());
                    return Box::new(Expr {
                        loc: 0..0,
                        val: Expr_::Error,
                    });
                }
            }
        }
    }

    fn consume_class(&mut self, _iter: &mut TokenIterator) -> Stat {
        todo!()
    }

    fn consume_def(&mut self, _iter: &mut TokenIterator) -> Stat {
        todo!()
    }

    fn consume_block(&mut self, iter: &mut TokenIterator) -> Box<Expr> {
        iter.next();
        let start = iter.loc();
        let mut exprs: Vec<Box<Expr>> = vec![];
        loop {
            match iter.peek() {
                Some(Token::Key(Keyword::End)) => {
                    iter.next();
                    let end = iter.loc();
                    return Box::new(Expr {
                        loc: start.start..end.end,
                        val: Expr_::Block(exprs),
                    });
                }
                Some(_) => {
                    let expr = self.consume_expr(iter, 0);
                    exprs.push(expr);
                }
                None => {
                    let end = iter.loc();
                    self.add_diagnostic(ErrorCode::UnexpectedEof, end.clone());
                    return Box::new(Expr {
                        loc: start.start..end.end,
                        val: Expr_::Block(exprs),
                    });
                }
            }
        }
    }

    fn consume_expr(&mut self, iter: &mut TokenIterator, min_precedence: u32) -> Box<Expr> {
        use Keyword::*;
        use Operator::*;
        use Token::*;
        let mut start = iter.next_loc();
        let mut lhs = match iter.peek() {
            Some(Id(s)) => {
                let s = s.clone();
                iter.next();
                Box::new(Expr {
                    loc: start.clone(),
                    val: Expr_::Id(s),
                })
            }
            Some(Key(Nil)) => {
                iter.next();
                Box::new(Expr {
                    loc: start.clone(),
                    val: Expr_::Const(Const::Nil),
                })
            }
            Some(Str(s)) => {
                let s = s.clone();
                iter.next();
                Box::new(Expr {
                    loc: start.clone(),
                    val: Expr_::Const(Const::Str(s)),
                })
            }
            Some(Float(f)) => {
                let f = *f;
                iter.next();
                Box::new(Expr {
                    loc: start.clone(),
                    val: Expr_::Const(Const::Float(f)),
                })
            }
            Some(Integer(i)) => {
                let i = *i;
                iter.next();
                Box::new(Expr {
                    loc: start.clone(),
                    val: Expr_::Const(Const::Int(i)),
                })
            }
            Some(Key(val @ (Keyword::True | Keyword::False))) => {
                let val = matches!(val, Keyword::True);
                iter.next();
                Box::new(Expr {
                    loc: start.clone(),
                    val: Expr_::Const(Const::Bool(val)),
                })
            }
            Some(Op(LPar)) => {
                iter.next();
                if matches!(iter.peek(), Some(Op(RPar))) {
                    iter.next();
                    let end = iter.loc();
                    self.add_diagnostic(ErrorCode::MissingExpr(start.clone()), end.clone());
                    Box::new(Expr {
                        loc: start.start..end.end,
                        val: Expr_::Error,
                    })
                } else {
                    let lhs = self.consume_expr(iter, 0);
                    self.consume_to_op(iter, RPar, Some((Op(LPar), start.clone())));
                    lhs
                }
            }
            Some(Op(op @ (Not | Minus))) => {
                let op = match op {
                    Not => OpPrefix::Not,
                    Minus => OpPrefix::Neg,
                    _ => unreachable!(),
                };
                iter.next();
                let rhs = self.consume_expr(iter, precedence_prefix());
                let end = iter.loc();
                Box::new(Expr {
                    loc: start.start..end.end,
                    val: Expr_::Prefix(op, rhs),
                })
            }
            Some(Key(If)) => self.consume_if(iter),
            Some(Key(Begin)) => self.consume_block(iter),
            Some(Op(LBrk)) => {
                iter.next();
                // Test for empty list
                if matches!(iter.peek(), Some(Op(RBrk))) {
                    iter.next();
                    let end = iter.loc();
                    Box::new(Expr {
                        loc: start.start..end.end,
                        val: Expr_::Const(Const::List(None)),
                    })
                } else {
                    let expr = self.consume_expr(iter, 0);
                    self.consume_to_op(iter, RBrk, Some((Op(LBrk), start.clone())));
                    let end = iter.loc();
                    Box::new(Expr {
                        loc: start.start..end.end,
                        val: Expr_::Const(Const::List(Some(expr))),
                    })
                }
            }
            Some(token) => {
                let token = token.clone();
                iter.next();
                self.add_diagnostic(
                    ErrorCode::UnexpectedToken(Some(token), None, None),
                    start.clone(),
                );
                return Box::new(Expr {
                    loc: start,
                    val: Expr_::Error,
                });
            }
            None => {
                self.add_diagnostic(ErrorCode::UnexpectedEof, start.clone());
                return Box::new(Expr {
                    loc: start,
                    val: Expr_::Error,
                });
            }
        };

        loop {
            let op = match iter.peek2() {
                (Some(Op(Colon)), op) => {
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
                                    lhs = Box::new(Expr {
                                        loc: loc.start..end.end,
                                        val: Expr_::Postfix(OpPostfix::Call, lhs, None),
                                    });
                                    start = end;
                                } else {
                                    let expr = self.consume_expr(iter, 0);
                                    self.consume_to_op(iter, RPar, Some((Op(LPar), match_loc)));
                                    let end = iter.loc();

                                    lhs = Box::new(Expr {
                                        loc: loc.start..end.end,
                                        val: Expr_::Postfix(OpPostfix::Call, lhs, Some(expr)),
                                    });
                                    start = iter.next_loc();
                                }
                                continue;
                            }
                            OpPostfix::Index => {
                                iter.next();
                                let loc = iter.loc();
                                iter.next();
                                let match_loc = iter.loc();
                                if matches!(iter.peek(), Some(Op(RBrk))) {
                                    iter.next();
                                    let end = iter.loc();
                                    self.add_diagnostic(
                                        ErrorCode::MissingExpr(match_loc),
                                        end.clone(),
                                    );
                                    lhs = Box::new(Expr {
                                        loc: loc.start..end.end,
                                        val: Expr_::Error,
                                    });
                                    start = iter.next_loc();
                                } else {
                                    let expr = self.consume_expr(iter, 0);
                                    self.consume_to_op(iter, RBrk, Some((Op(LBrk), match_loc)));
                                    let end = iter.loc();

                                    lhs = Box::new(Expr {
                                        loc: loc.start..end.end,
                                        val: Expr_::Postfix(OpPostfix::Index, lhs, Some(expr)),
                                    });
                                    start = end;
                                }
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
                    _ => return lhs,
                },
                _ => return lhs,
            };

            let precedence = precedence_infix(op);
            if precedence.0 < min_precedence {
                break;
            }

            iter.next();

            let rhs = self.consume_expr(iter, precedence.1);
            let end = iter.loc();

            lhs = Box::new(Expr {
                loc: start.start..end.end,
                val: Expr_::Infix(op, lhs, rhs),
            });
            start = end;
        }

        lhs
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn test_expr() {
        let code = "a,b, nil = not 32 * 15:()+8.9e13//(12+\"asdf\") or false and -23";
        let code = SharedFile::from_str(code);
        let lexer = Lexer::new(OsString::from_str("test.dm").unwrap(), code);
        let mut parser = Parser::new();
        let expr = parser.consume_expr(&mut lexer.iter(), 0);
        println!("{expr:?}");
        parser.diagnoser.print();
        // This is manually verified to be correct =)
        let result = "(((\"a\", Comma, \"b\"), Comma, nil), Assign, ((((Not, 32), Mul, (15, Call, None)), Plus, (89000000000000, DivFloor, (12, Plus, asdf))), Or, (false, And, (Neg, 23))))";
        assert_eq!(format!("{:?}", expr), result);
    }

    #[test]
    fn test_expr_postfix_ambiguous() {
        let code = "0,a : (1,2,3) (3,4):[2-1]+0.333//[0,1,2]:[1][a,v,b]";
        let mut parser = Parser::new();
        parser.parse_str(OsStr::new("test.dm"), code);
        println!("{:#?}", parser.ast.statements);
        if parser.diagnostic_count() > 0 {
            parser.diagnoser.print();
        }
        assert_eq!(parser.ast.statements.len(), 3);
    }

    #[test]
    fn test_invalid_op() {
        let code = ">> <<";
        let mut parser = Parser::new();
        parser.parse_str(OsStr::new("test.dm"), code);
        println!("{:#?}", parser.ast.statements);
        assert!(parser.diagnostic_count() > 0);
    }

    #[test]
    fn test_if_1() {
        let code = "if a then b elsif c then 0.92 a = 0 b:[0,1,2] else end";
        let mut parser = Parser::new();
        parser.parse_str(OsStr::new("test.dm"), code);
        println!("{:#?}", parser.ast.statements);
        if parser.diagnostic_count() > 0 {
            parser.diagnoser.print();
        }
        assert!(parser.diagnostic_count() == 0);
    }

    #[test]
    fn test_if_2() {
        let code = "if a then else nil end";
        let mut parser = Parser::new();
        parser.parse_str(OsStr::new("test.dm"), code);
        println!("{:#?}", parser.ast.statements);
        if parser.diagnostic_count() > 0 {
            parser.diagnoser.print();
        }
        assert!(parser.diagnostic_count() == 0);
    }
}
