use super::{
    ast::{Ast, Const, Expr, Expr_, OpInfix, OpPostfix, OpPrefix, Stat, Stat_, Type},
    error::ErrorType,
    lexer::{Keyword, Operator, Token},
    util::TokenIterator,
    ErrorReporter, Lexer,
};
use ahash::AHashMap;
use std::fs;

const fn precedence_infix(op: OpInfix) -> (u32, u32) {
    use OpInfix::*;
    match op {
        Comma => (1, 2),
        Range => (3, 4),
        Or | And => (5, 6),
        Eq | Ne | Le | Lt | Gt | Ge => (7, 8),
        Plus | Minus => (9, 10),
        Mul | Div | DivFloor | Mod => (11, 12),
        Exp => (13, 14),
    }
}

const fn precedence_prefix() -> u32 {
    100
}

const fn precedence_postfix() -> u32 {
    101
}

/// The parser for Diatom.
pub struct Parser {
    error_reporter: ErrorReporter,
    files: AHashMap<String, Lexer>,
    ast: Ast,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            error_reporter: ErrorReporter::new(),
            files: AHashMap::new(),
            ast: Ast::default(),
        }
    }

    pub fn has_error(&self) -> bool {
        !self.error_reporter.is_empty()
    }

    /// Parse a file.
    ///
    /// Return Err(()) if file can not be read.
    pub fn parse(&mut self, path: &str) -> Result<(), std::io::Error> {
        let abspath = fs::canonicalize(path)?;
        let abspath_string = abspath.to_str().ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::Other, "File path is not valid utf-8")
        })?;
        let content = fs::read_to_string(&abspath)?;
        let lexer = Lexer::new(content, abspath_string.to_string());
        self.parse_iter(&mut lexer.iter());
        self.files.insert(abspath_string.to_string(), lexer);
        Ok(())
    }

    pub fn parse_iter(&mut self, iter: &mut TokenIterator) {
        use Keyword::*;
        use Token::*;
        loop {
            let start = iter.get_location();
            let stat = match iter.peek() {
                Some(Key(Class)) => {
                    todo!()
                }
                Some(Key(Def)) => {
                    todo!()
                }
                Some(_) => {
                    let expr = self.consume_expr(iter, 0);
                    let end = iter.get_location();
                    Stat {
                        location: start + end,
                        val: Stat_::Expr(expr),
                    }
                }
                None => {
                    return;
                }
            };
            self.ast.statements.push(stat);
        }
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

    fn consume_if(&mut self, iter: &mut TokenIterator) -> Box<Expr> {
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
        let mut start = iter.get_location();
        let mut lhs = match iter.peek() {
            Some(Id(s)) => {
                let s = s.clone();
                iter.next();
                let location = iter.get_location();
                Box::new(Expr {
                    location,
                    val: Expr_::Id(s),
                })
            }
            Some(Str(s)) => {
                let s = s.clone();
                iter.next();
                let location = iter.get_location();
                Box::new(Expr {
                    location,
                    val: Expr_::Const(Const::Str(s)),
                })
            }
            Some(Float(f)) => {
                let f = *f;
                iter.next();
                let location = iter.get_location();
                Box::new(Expr {
                    location,
                    val: Expr_::Const(Const::Float(f)),
                })
            }
            Some(Integer(i)) => {
                let i = *i;
                iter.next();
                let location = iter.get_location();
                Box::new(Expr {
                    location,
                    val: Expr_::Const(Const::Int(i)),
                })
            }
            Some(Key(val @ (Keyword::True | Keyword::False))) => {
                let val = !matches!(val, Keyword::True);
                iter.next();
                let location = iter.get_location();
                Box::new(Expr {
                    location,
                    val: Expr_::Const(Const::Bool(val)),
                })
            }
            Some(Op(LPar)) => {
                let location = iter.get_location();
                iter.next();
                let lhs = self.consume_expr(iter, 0);
                if !matches!(iter.peek(), Some(Op(RPar))) {
                    iter.next();
                    let location_now = iter.get_location();
                    self.error_reporter.append(
                        "Parser",
                        location + location_now,
                        ErrorType::UnexpectedToken((
                            "Missing parentheses".to_string(),
                            "Expected \")\" here.".to_string(),
                        )),
                    );
                }
                Self::consume_until_op(iter, RPar);
                iter.next();
                lhs
            }
            Some(Op(op @ (Not | Minus))) => {
                let op = match op {
                    Not => OpPrefix::Not,
                    Minus => OpPrefix::Neg,
                    _ => unreachable!(),
                };
                let start = iter.get_location();
                iter.next();
                let rhs = self.consume_expr(iter, precedence_prefix());
                let end = iter.get_location();
                Box::new(Expr {
                    location: start + end,
                    val: Expr_::Prefix(op, rhs),
                })
            }
            Some(Key(If)) => self.consume_if(iter),
            Some(Op(LBrk)) => {
                let start = iter.get_location();
                iter.next();
                let expr = self.consume_expr(iter, 0);
                let location = iter.get_location();
                if Self::consume_until_op(iter, RBrk) > 0 {
                    self.error_reporter.append(
                        "Parser",
                        location,
                        ErrorType::UnexpectedToken((
                            "Missing parentheses".to_string(),
                            "Expected \"]\" here.".to_string(),
                        )),
                    );
                };
                iter.next();
                let end = iter.get_location();
                Box::new(Expr {
                    location: start + end,
                    val: Expr_::Const(Const::List(expr)),
                })
            }
            Some(token) => {
                let token = format!("{:?}", token);
                let location = iter.get_location();
                iter.next();
                self.error_reporter.append(
                    "Parser",
                    iter.get_location(),
                    ErrorType::UnexpectedToken((
                        format!("{:?}", token),
                        "This token is not allowed at the beginning of an expression.".to_string(),
                    )),
                );
                return Box::new(Expr {
                    location,
                    val: Expr_::Error,
                });
            }
            None => {
                let location = iter.get_location();
                self.error_reporter
                    .append("Parser", iter.get_location(), ErrorType::EofError());
                return Box::new(Expr {
                    location,
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
                        Some(op) => {
                            self.error_reporter.append(
                                "Parser",
                                iter.get_location(),
                                ErrorType::UnexpectedToken((
                                    format!("{:?}", op),
                                    "Expected \")\" here.".to_string(),
                                )),
                            );
                            return lhs;
                        }
                        None => {
                            self.error_reporter.append(
                                "Parser",
                                iter.get_location(),
                                ErrorType::EofError(),
                            );
                            return lhs;
                        }
                    };
                    let precedence = precedence_postfix();
                    if precedence > min_precedence {
                        match op {
                            OpPostfix::Call => {
                                iter.next();
                                iter.next();
                                let expr = self.consume_expr(iter, 0);
                                let location = iter.get_location();
                                if Self::consume_until_op(iter, RPar) > 0 {
                                    self.error_reporter.append(
                                        "Parser",
                                        location,
                                        ErrorType::UnexpectedToken((
                                            "Missing parentheses".to_string(),
                                            "Expected \")\" here.".to_string(),
                                        )),
                                    );
                                };
                                iter.next();
                                let end = iter.get_location();

                                lhs = Box::new(Expr {
                                    location: start.clone() + end.clone(),
                                    val: Expr_::Postfix(OpPostfix::Call, lhs, expr),
                                });
                                start = end;
                                continue;
                            }
                            OpPostfix::Index => {
                                iter.next();
                                iter.next();
                                let expr = self.consume_expr(iter, 0);
                                let location = iter.get_location();
                                if Self::consume_until_op(iter, RBrk) > 0 {
                                    self.error_reporter.append(
                                        "Parser",
                                        location,
                                        ErrorType::UnexpectedToken((
                                            "Missing parentheses".to_string(),
                                            "Expected \"]\" here.".to_string(),
                                        )),
                                    );
                                };
                                iter.next();
                                let end = iter.get_location();

                                lhs = Box::new(Expr {
                                    location: start.clone() + end.clone(),
                                    val: Expr_::Postfix(OpPostfix::Index, lhs, expr),
                                });
                                start = end;
                                continue;
                            }
                        };
                    } else {
                        return lhs;
                    }
                }
                (Some(op), _) => match op {
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
            let end = iter.get_location();

            lhs = Box::new(Expr {
                location: start.clone() + end.clone(),
                val: Expr_::Infix(op, lhs, rhs),
            });
            start = end;
        }

        lhs
    }

    fn consume_assign(&mut self, _iter: &mut TokenIterator) {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expr() {
        let code = "not 32 * 15+8.9e13//(12+\"asdf\") or false and -23";
        let lexer = Lexer::new(code.to_string(), "test.dm".to_string());
        let mut parser = Parser::new();
        let expr = parser.consume_expr(&mut lexer.iter(), 0);
        // This is manually verified to be correct =)
        let result = "Expr { location: 0:40-0:48, val: Infix(And, Expr { location: 0:31-0:40, val: Infix(Or, Expr { location: 0:11-0:31, val: Infix(Plus, Expr { location: 0:0-0:11, val: Infix(Mul, Expr { location: 0:0-0:6, val: Prefix(Not, Expr { location: 0:4-0:6, val: Const(Int(32)) }) }, Expr { location: 0:9-0:11, val: Const(Int(15)) }) }, Expr { location: 0:12-0:31, val: Infix(DivFloor, Expr { location: 0:12-0:18, val: Const(Float(89000000000000.0)) }, Expr { location: 0:21-0:30, val: Infix(Plus, Expr { location: 0:21-0:23, val: Const(Int(12)) }, Expr { location: 0:24-0:30, val: Const(Str(\"asdf\")) }) }) }) }, Expr { location: 0:35-0:40, val: Const(Bool(true)) }) }, Expr { location: 0:44-0:48, val: Prefix(Neg, Expr { location: 0:46-0:48, val: Const(Int(23)) }) }) }";
        assert_eq!(format!("{:?}", expr), result);
    }

    #[test]
    fn test_expr_postfix_ambiguous() {
        let code = "0,a : (1,2,3) (3,4):[2-1]+0.333//[0,1,2]:[1][a,v,b]";
        let lexer = Lexer::new(code.to_string(), "test.dm".to_string());
        let mut parser = Parser::new();
        parser.parse_iter(&mut lexer.iter());
        assert_eq!(parser.ast.statements.len(), 3);
    }
}
