mod ast;
mod error;
use crate::diagnostic::{Diagnoser, Diagnostic, DisplayableOsString, Loc, SharedFile};

use self::error::{to_diagnostic, ErrorCode};

use super::{
    lexer::{Keyword, Operator, Token},
    util::TokenIterator,
    Lexer,
};
use ahash::AHashMap;
use ast::{Ast, Const, Expr, Expr_, OpInfix, OpPostfix, OpPrefix, Stat, Stat_, Type};
use std::{
    ffi::{OsStr, OsString},
    fs,
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
}

impl Parser {
    pub fn new() -> Self {
        Self {
            files: AHashMap::new(),
            ast: Ast::default(),
            diagnoser: Diagnoser::new(),
            current_file_id: 0,
        }
    }

    fn to_diagnostic(&self, error: ErrorCode, loc: Loc) -> Diagnostic {
        to_diagnostic(error, loc, self.current_file_id)
    }

    pub fn clear_diagnoses(&mut self) {
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
                Some(Key(Class)) => {
                    todo!()
                }
                Some(Key(Def)) => {
                    todo!()
                }
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

    fn consume_until_key(iter: &mut TokenIterator, key: Keyword) -> usize {
        let key_type = std::mem::discriminant(&key);
        let mut count = 0;
        loop {
            match iter.peek() {
                Some(Token::Key(next)) if std::mem::discriminant(next) == key_type => return count,
                None => return count,
                _ => (),
            }
            iter.next();
            count += 1;
        }
    }

    fn consume_until_op(iter: &mut TokenIterator, op: Operator) -> usize {
        let op_type = std::mem::discriminant(&op);
        let mut count = 0;
        loop {
            match iter.peek() {
                Some(Token::Op(next)) if std::mem::discriminant(next) == op_type => return count,
                None => return count,
                _ => (),
            }
            iter.next();
            count += 1;
        }
    }

    fn consume_type(&mut self, _iter: &mut TokenIterator) -> Type {
        todo!()
    }

    fn consume_if(&mut self, _iter: &mut TokenIterator) -> Box<Expr> {
        todo!()
    }

    fn consume_case(&mut self, _iter: &mut TokenIterator) {
        todo!()
    }

    fn consume_loop_if(&mut self, _iter: &mut TokenIterator) {
        todo!()
    }

    fn consume_loop(&mut self, _iter: &mut TokenIterator) {
        todo!()
    }

    fn consume_class(&mut self, _iter: &mut TokenIterator) {
        todo!()
    }

    fn consume_def(&mut self, _iter: &mut TokenIterator) {
        todo!()
    }

    fn consume_pattern(&mut self, _iter: &mut TokenIterator) {
        todo!()
    }

    fn consume_expr(&mut self, iter: &mut TokenIterator, min_precedence: u32) -> Box<Expr> {
        use Keyword::*;
        use Operator::*;
        use Token::*;
        let mut start = iter.loc();
        let mut lhs = match iter.peek() {
            Some(Id(s)) => {
                let s = s.clone();
                iter.next();
                let loc = iter.loc();
                Box::new(Expr {
                    loc,
                    val: Expr_::Id(s),
                })
            }
            Some(Key(Nil)) => {
                iter.next();
                let loc = iter.loc();
                Box::new(Expr {
                    loc,
                    val: Expr_::Const(Const::Nil),
                })
            }
            Some(Str(s)) => {
                let s = s.clone();
                iter.next();
                let loc = iter.loc();
                Box::new(Expr {
                    loc,
                    val: Expr_::Const(Const::Str(s)),
                })
            }
            Some(Float(f)) => {
                let f = *f;
                iter.next();
                let loc = iter.loc();
                Box::new(Expr {
                    loc,
                    val: Expr_::Const(Const::Float(f)),
                })
            }
            Some(Integer(i)) => {
                let i = *i;
                iter.next();
                let loc = iter.loc();
                Box::new(Expr {
                    loc,
                    val: Expr_::Const(Const::Int(i)),
                })
            }
            Some(Key(val @ (Keyword::True | Keyword::False))) => {
                let val = matches!(val, Keyword::True);
                iter.next();
                let loc = iter.loc();
                Box::new(Expr {
                    loc,
                    val: Expr_::Const(Const::Bool(val)),
                })
            }
            Some(Op(LPar)) => {
                iter.next();
                let loc = iter.loc();
                if matches!(iter.peek(), Some(Op(RPar))) {
                    iter.next();
                    let end = iter.loc();
                    self.diagnoser
                        .push(self.to_diagnostic(ErrorCode::MissingExpr(loc.clone()), end.clone()));
                    Box::new(Expr {
                        loc: loc.start..end.end,
                        val: Expr_::Error,
                    })
                } else {
                    let lhs = self.consume_expr(iter, 0);
                    if !matches!(iter.peek(), Some(Op(RPar))) {
                        let t = iter.next().map(|x| x.clone());
                        let loc_now = iter.loc();
                        self.diagnoser.push(self.to_diagnostic(
                            ErrorCode::UnexpectedToken(t, Some(Op(RPar)), Some((Op(LPar), loc))),
                            loc_now,
                        ));
                    }
                    Self::consume_until_op(iter, RPar);
                    iter.next();
                    lhs
                }
            }
            Some(Op(op @ (Not | Minus))) => {
                let op = match op {
                    Not => OpPrefix::Not,
                    Minus => OpPrefix::Neg,
                    _ => unreachable!(),
                };
                let start = iter.loc();
                iter.next();
                let rhs = self.consume_expr(iter, precedence_prefix());
                let end = iter.loc();
                Box::new(Expr {
                    loc: start.start..end.end,
                    val: Expr_::Prefix(op, rhs),
                })
            }
            Some(Key(If)) => self.consume_if(iter),
            Some(Op(LBrk)) => {
                iter.next();
                let start = iter.loc();
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
                    if !matches!(iter.peek(), Some(Op(RBrk))) {
                        let t = iter.next().map(|x| x.clone());
                        let loc_now = iter.loc();
                        self.diagnoser.push(self.to_diagnostic(
                            ErrorCode::UnexpectedToken(
                                t,
                                Some(Op(RBrk)),
                                Some((Op(LBrk), start.clone())),
                            ),
                            loc_now,
                        ));
                    }
                    Self::consume_until_op(iter, RBrk);
                    iter.next();
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
                let loc = iter.loc();
                self.diagnoser.push(self.to_diagnostic(
                    ErrorCode::UnexpectedToken(Some(token), None, None),
                    loc.clone(),
                ));
                return Box::new(Expr {
                    loc,
                    val: Expr_::Error,
                });
            }
            None => {
                let loc = iter.loc();
                self.diagnoser
                    .push(self.to_diagnostic(ErrorCode::UnexpectedEof, loc.clone()));
                return Box::new(Expr {
                    loc,
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
                            self.diagnoser.push(self.to_diagnostic(
                                ErrorCode::UnexpectedToken(
                                    Some(token),
                                    None,
                                    Some((Op(Colon), loc)),
                                ),
                                loc_token,
                            ));
                            return lhs;
                        }
                        None => {
                            iter.next();
                            let loc = iter.loc();
                            self.diagnoser
                                .push(self.to_diagnostic(ErrorCode::UnexpectedEof, loc));
                            return lhs;
                        }
                    };
                    let precedence = precedence_postfix();
                    if precedence > min_precedence {
                        match op {
                            OpPostfix::Call => {
                                iter.next();
                                iter.next();
                                let loc = iter.loc();
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
                                    if !matches!(iter.peek(), Some(Op(RPar))) {
                                        let t = iter.next().map(|x| x.clone());
                                        let loc_now = iter.loc();
                                        self.diagnoser.push(self.to_diagnostic(
                                            ErrorCode::UnexpectedToken(
                                                t,
                                                Some(Op(RPar)),
                                                Some((Op(LPar), loc.clone())),
                                            ),
                                            loc_now,
                                        ));
                                    }
                                    Self::consume_until_op(iter, RPar);
                                    iter.next();
                                    let end = iter.loc();

                                    lhs = Box::new(Expr {
                                        loc: loc.start..end.end,
                                        val: Expr_::Postfix(OpPostfix::Call, lhs, Some(expr)),
                                    });
                                    start = end;
                                }
                                continue;
                            }
                            OpPostfix::Index => {
                                iter.next();
                                iter.next();
                                let loc = iter.loc();
                                if matches!(iter.peek(), Some(Op(RBrk))) {
                                    iter.next();
                                    let end = iter.loc();
                                    self.diagnoser.push(self.to_diagnostic(
                                        ErrorCode::MissingExpr(loc.clone()),
                                        end.clone(),
                                    ));
                                    lhs = Box::new(Expr {
                                        loc: loc.start..end.end,
                                        val: Expr_::Error,
                                    });
                                    start = end;
                                } else {
                                    let expr = self.consume_expr(iter, 0);
                                    if !matches!(iter.peek(), Some(Op(RBrk))) {
                                        let t = iter.next().map(|x| x.clone());
                                        let loc_now = iter.loc();
                                        self.diagnoser.push(self.to_diagnostic(
                                            ErrorCode::UnexpectedToken(
                                                t,
                                                Some(Op(RBrk)),
                                                Some((Op(LBrk), loc.clone())),
                                            ),
                                            loc_now,
                                        ));
                                    }
                                    Self::consume_until_op(iter, RBrk);
                                    iter.next();
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
        parser.diagnoser.print();
        assert_eq!(parser.ast.statements.len(), 3);
    }

    #[test]
    fn test_invalid_op() {
        let code = ">> <<";
        let mut parser = Parser::new();
        parser.parse_str(OsStr::new("test.dm"), code);
        println!("{:#?}", parser.ast.statements);
        parser.diagnoser.print();
        assert!(parser.diagnostic_count() > 0);
    }
}
