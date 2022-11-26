use super::{
    error::ErrorType,
    lexer::{Keyword, Operator, Token},
    util::TokenIterator,
    ErrorReporter, Lexer,
    ast::{Type, OpInfix, OpPrefix, OpPostfix, Expr, Expr_, Const}
};

const fn precedence_infix(op: OpInfix) -> (u32, u32) {
    use OpInfix::*;
    match op {
        Range => (1, 2),
        Or | And => (3, 4),
        Eq | Ne | Le | Lt | Gt | Ge => (5, 6),
        Plus | Minus => (7, 8),
        Mul | Div | DivFloor | Mod => (9, 10),
        Exp => (11, 12),
    }
}

const fn precedence_prefix() -> u32 {
    100
}

const fn precedence_postfix() -> u32 {
    101
}

/// The parser for Diatom.
pub struct Parser
{
    error_reporter: ErrorReporter,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            error_reporter: ErrorReporter::new(),
        }
    }

    pub fn has_error(&self) -> bool {
        !self.error_reporter.is_empty()
    }

    pub fn parse(&mut self, lexer: &mut Lexer) {
        use Keyword::*;
        use Operator::*;
        use Token::*;
        let mut iter = lexer.iter();
        loop {
            match iter.peek2() {
                (Some(Id(_)), Some(Op(Colon | Comma | Eq))) => {
                    self.consume_assign(&mut iter);
                }
                (Some(Key(LOOP)), Some(Key(IF))) => {
                    self.consume_loop_if(&mut iter);
                }
                _ => todo!(),
            }
        }
    }

    fn consume_type(&mut self, iter: &mut TokenIterator) -> Type {
        todo!()
    }

    fn consume_if(&mut self, iter: &mut TokenIterator) {
        todo!()
    }

    fn consume_case(&mut self, iter: &mut TokenIterator) {
        todo!()
    }

    fn consume_loop_if(&mut self, iter: &mut TokenIterator) {
        todo!()
    }

    fn consume_loop(&mut self, iter: &mut TokenIterator) {
        todo!()
    }

    fn consume_class(&mut self, iter: &mut TokenIterator) {
        todo!()
    }

    fn consume_def(&mut self, iter: &mut TokenIterator) {
        todo!()
    }

    fn consume_pattern(&mut self, iter: &mut TokenIterator) {
        todo!()
    }

    fn consume_expr(&mut self, iter: &mut TokenIterator, min_precedence: u32) -> Box<Expr> {
        use Operator::*;
        use Token::*;
        let mut start = iter.get_location();
        let mut lhs = match iter.peek() {
            Some(Id(s)) => {
                let s = s.clone();
                iter.next();
                let location = iter.get_location();
                Box::new(Expr{location, val: Expr_::Id(s)})
            },
            Some(Str(s)) => {
                let s = s.clone();
                iter.next();
                let location = iter.get_location();
                Box::new(Expr{location, val: Expr_::Const(Const::Str(s))})
            }
            Some(Float(f)) => {
                let f = *f;
                iter.next();
                let location = iter.get_location();
                Box::new(Expr{location, val: Expr_::Const(Const::Float(f))})
            }
            Some(Integer(i)) => {
                let i = *i;
                iter.next();
                let location = iter.get_location();
                Box::new(Expr{location, val: Expr_::Const(Const::Int(i))})
            }
            Some(Key(val@(Keyword::TRUE | Keyword::FALSE))) => {
                let val = !matches!(val, Keyword::TRUE);
                iter.next();
                let location = iter.get_location();
                Box::new(Expr{location, val: Expr_::Const(Const::Bool(val))})
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
                while !matches!(iter.next(), Some(Op(RPar))) {}
                lhs
            }
            Some(Op(op @ (Not | Minus))) => {
                let op = match op {
                    Not => OpPrefix::Not,
                    Minus => OpPrefix::Neg,
                    _ => unreachable!()
                };
                let start = iter.get_location();
                iter.next();
                let rhs = self.consume_expr(iter, precedence_prefix());
                let end = iter.get_location();
                Box::new(Expr{location: start+end, val: Expr_::Prefix(op, rhs)})
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
                return Box::new(Expr{location, val: Expr_::Error});
            }
            None => {
                let location = iter.get_location();
                self.error_reporter
                    .append("Parser", iter.get_location(), ErrorType::EofError());
                return Box::new(Expr{location, val: Expr_::Error});
            }
        };

        loop {
            let op = match iter.peek() {
                Some(Op(Range)) => OpInfix::Range,
                Some(Op(Or)) => OpInfix::Or,
                Some(Op(And)) => OpInfix::And,
                Some(Op(Eq)) => OpInfix::Eq,
                Some(Op(Ne)) => OpInfix::Ne,
                Some(Op(Le)) => OpInfix::Le,
                Some(Op(Lt)) => OpInfix::Lt,
                Some(Op(Ge)) => OpInfix::Ge,
                Some(Op(Gt)) => OpInfix::Gt,
                Some(Op(Plus)) => OpInfix::Plus,
                Some(Op(Minus)) => OpInfix::Minus,
                Some(Op(Mul)) => OpInfix::Mul,
                Some(Op(Div)) => OpInfix::Div,
                Some(Op(DivFloor)) => OpInfix::DivFloor,
                Some(Op(Mod)) => OpInfix::Mod,
                Some(Op(Exp)) => OpInfix::Exp,
                Some(Op(LBrk)) => {
                    // array
                    todo!()
                }
                Some(Op(LPar)) => {
                    // function call
                    todo!()
                }
                Some(Op(Pipeline)) => {
                    // Pipeline
                    todo!()
                }
                _ => return lhs,
            };

            let precedence = precedence_infix(op);
            if precedence.0 < min_precedence {
                break;
            }

            iter.next();

            let rhs = self.consume_expr(iter, precedence.1);
            let end = iter.get_location();

            lhs = Box::new(Expr{location: start.clone() + end.clone(), val: Expr_::Infix(op, lhs, rhs)});
            start = end;
        }

        lhs
    }

    fn consume_assign(&mut self, iter: &mut TokenIterator) {
        todo!()
    }
}

#[cfg(test)]
mod tests{
    use super::*;

    #[test]
    fn test_expr(){
        let code = "not 32 * 15+8.9e13//(12+\"asdf\") or false and -23";
        let lexer = Lexer::new(code, Some("test.dm"));
        let mut parser = Parser::new();
        let expr = parser.consume_expr(&mut lexer.iter(), 0);
        // This is manually verified to be correct =)
        let result = "Expr { location: FileLocation { start: LineLocation { line: 0, offset: 40 }, end: LineLocation { line: 0, offset: 48 } }, val: Infix(And, Expr { location: FileLocation { start: LineLocation { line: 0, offset: 31 }, end: LineLocation { line: 0, offset: 40 } }, val: Infix(Or, Expr { location: FileLocation { start: LineLocation { line: 0, offset: 11 }, end: LineLocation { line: 0, offset: 31 } }, val: Infix(Plus, Expr { location: FileLocation { start: LineLocation { line: 0, offset: 0 }, end: LineLocation { line: 0, offset: 11 } }, val: Infix(Mul, Expr { location: FileLocation { start: LineLocation { line: 0, offset: 0 }, end: LineLocation { line: 0, offset: 6 } }, val: Prefix(Not, Expr { location: FileLocation { start: LineLocation { line: 0, offset: 4 }, end: LineLocation { line: 0, offset: 6 } }, val: Const(Int(32)) }) }, Expr { location: FileLocation { start: LineLocation { line: 0, offset: 9 }, end: LineLocation { line: 0, offset: 11 } }, val: Const(Int(15)) }) }, Expr { location: FileLocation { start: LineLocation { line: 0, offset: 12 }, end: LineLocation { line: 0, offset: 31 } }, val: Infix(DivFloor, Expr { location: FileLocation { start: LineLocation { line: 0, offset: 12 }, end: LineLocation { line: 0, offset: 18 } }, val: Const(Float(89000000000000.0)) }, Expr { location: FileLocation { start: LineLocation { line: 0, offset: 21 }, end: LineLocation { line: 0, offset: 30 } }, val: Infix(Plus, Expr { location: FileLocation { start: LineLocation { line: 0, offset: 21 }, end: LineLocation { line: 0, offset: 23 } }, val: Const(Int(12)) }, Expr { location: FileLocation { start: LineLocation { line: 0, offset: 24 }, end: LineLocation { line: 0, offset: 30 } }, val: Const(Str(\"asdf\")) }) }) }) }, Expr { location: FileLocation { start: LineLocation { line: 0, offset: 35 }, end: LineLocation { line: 0, offset: 40 } }, val: Const(Bool(false)) }) }, Expr { location: FileLocation { start: LineLocation { line: 0, offset: 44 }, end: LineLocation { line: 0, offset: 48 } }, val: Prefix(Neg, Expr { location: FileLocation { start: LineLocation { line: 0, offset: 46 }, end: LineLocation { line: 0, offset: 48 } }, val: Const(Int(23)) }) }) }";
        assert_eq!(format!("{:?}", expr), result);
    }
}
