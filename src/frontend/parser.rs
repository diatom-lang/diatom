use super::{
    error::ErrorType,
    lexer::{Keyword, Operator, Token},
    util::TokenIterator,
    ErrorReporter, Lexer,
};
use crate::backend::{IrBuilder, Type};

#[derive(Clone, Copy)]
enum OpInfix {
    Range,
    Or,
    And,
    Eq,
    Ne,
    Le,
    Lt,
    Ge,
    Gt,
    Plus,
    Minus,
    Mul,
    Div,
    DivFloor,
    Mod,
    Exp,
}

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
pub struct Parser<Builder>
where
    Builder: IrBuilder,
{
    builder: Builder,
    error_reporter: ErrorReporter,
}

impl<'a, Builder: IrBuilder> Parser<Builder> {
    pub fn new() -> Self {
        Self {
            builder: Builder::new(),
            error_reporter: ErrorReporter::new(),
        }
    }

    pub fn has_error(&self) -> bool {
        !self.error_reporter.is_empty()
    }

    pub fn parse(&mut self, lexer: &'a mut Lexer) {
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

    fn consume_type(&mut self, iter: &'a mut TokenIterator) -> Type {
        todo!()
    }

    fn consume_id(&mut self, iter: &'a mut TokenIterator) -> (&'a String, Type) {
        match iter.next() {
            Some(Token::Id(s)) => {
                if let Some(Token::Op(Operator::Colon)) = iter.peek() {
                    iter.next();
                    (s, self.consume_type(iter))
                } else {
                    (s, Type::Any)
                }
            }
            _ => unreachable!(),
        }
    }

    fn consume_if(&mut self, iter: &'a mut TokenIterator) {
        todo!()
    }

    fn consume_case(&mut self, iter: &'a mut TokenIterator) {
        todo!()
    }

    fn consume_loop_if(&mut self, iter: &'a mut TokenIterator) {
        todo!()
    }

    fn consume_loop(&mut self, iter: &'a mut TokenIterator) {
        todo!()
    }

    fn consume_class(&mut self, iter: &'a mut TokenIterator) {
        todo!()
    }

    fn consume_def(&mut self, iter: &'a mut TokenIterator) {
        todo!()
    }

    fn consume_pattern(&mut self, iter: &'a mut TokenIterator) {
        todo!()
    }

    fn consume_expr(&mut self, iter: &'a mut TokenIterator, min_precedence: u32) -> String {
        use Operator::*;
        use Token::*;
        let lhs = match iter.peek() {
            Some(Id(s)) => s.clone(),
            Some(Op(LPar)) => {
                iter.reset_location_counter();
                iter.next();
                let location = iter.get_location();
                let lhs = self.consume_expr(iter, 0);
                if !matches!(iter.peek(), Some(Op(RPar))) {
                    iter.next();
                    let location_now = iter.get_location();
                    self.error_reporter.append(
                        "Parser",
                        location | location_now,
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
                let op = *op;
                iter.reset_location_counter();
                iter.next();
                let location = iter.get_location();
                let rhs = self.consume_expr(iter, precedence_prefix());
                let res = self.builder.get_intermediate();
                if let Err(s) = match op {
                    Not => self.builder.not(&rhs, &res),
                    Minus => self.builder.neg(&rhs, &res),
                    _ => unreachable!(),
                } {
                    self.error_reporter
                        .append("IrBuilder", location, ErrorType::IrError(s));
                }
                res
            }
            Some(token) => {
                let token = format!("{:?}", token);
                iter.reset_location_counter();
                iter.next();
                self.error_reporter.append(
                    "Parser",
                    iter.get_location(),
                    ErrorType::UnexpectedToken((
                        format!("{:?}", token),
                        "This token is not allowed at the beginning of an expression.".to_string(),
                    )),
                );
                return self.builder.get_intermediate();
            }
            None => {
                iter.reset_location_counter();
                self.error_reporter
                    .append("Parser", iter.get_location(), ErrorType::EofError());
                return self.builder.get_intermediate();
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

            iter.reset_location_counter();
            iter.next();
            let location = iter.get_location();

            let rhs = self.consume_expr(iter, precedence.1);

            let res = self.builder.get_intermediate();
            if let Err(s) = match op {
                OpInfix::Range => self.builder.range(&lhs, &rhs, &res),
                OpInfix::Or => self.builder.or(&lhs, &rhs, &res),
                OpInfix::And => self.builder.and(&lhs, &rhs, &res),
                OpInfix::Eq => self.builder.eq(&lhs, &rhs, &res),
                OpInfix::Ne => self.builder.ne(&lhs, &rhs, &res),
                OpInfix::Le => self.builder.le(&lhs, &rhs, &res),
                OpInfix::Lt => self.builder.lt(&lhs, &rhs, &res),
                OpInfix::Ge => self.builder.ge(&lhs, &rhs, &res),
                OpInfix::Gt => self.builder.gt(&lhs, &rhs, &res),
                OpInfix::Plus => self.builder.and(&lhs, &rhs, &res),
                OpInfix::Minus => self.builder.sub(&lhs, &rhs, &res),
                OpInfix::Mul => self.builder.mul(&lhs, &rhs, &res),
                OpInfix::Div => self.builder.div(&lhs, &rhs, &res),
                OpInfix::DivFloor => self.builder.div_floor(&lhs, &rhs, &res),
                OpInfix::Mod => self.builder.modulo(&lhs, &rhs, &res),
                OpInfix::Exp => self.builder.exp(&lhs, &rhs, &res),
            } {
                self.error_reporter
                    .append("IrBuilder", location, ErrorType::IrError(s));
            }
            return res;
        }

        lhs
    }

    fn consume_assign(&mut self, iter: &'a mut TokenIterator) {
        let mut vars = Vec::<String>::new();
        // Read var list
        loop {
            iter.reset_location_counter();
            match iter.peek() {
                Some(Token::Id(_)) => {
                    let var = self.consume_id(iter);
                    vars.push(var.0.clone());
                    match self.builder.declare_var(var.0, var.1) {
                        Ok(_) => (),
                        Err(s) => self.error_reporter.append(
                            "IrBuilder",
                            iter.get_location(),
                            ErrorType::IrError(s),
                        ),
                    };
                }
                Some(Token::Op(Operator::Comma)) => {
                    iter.next();
                    continue;
                }
                Some(Token::Op(Operator::Assign)) => {
                    iter.next();
                    break;
                }
                Some(token) => {
                    self.error_reporter.append(
                        "Parser",
                        iter.get_location(),
                        ErrorType::UnexpectedToken((
                            format!("{:?}", token),
                            "Assign expression does not allow this token here.".to_string(),
                        )),
                    );
                    // omit everything till next "="
                    loop {
                        match iter.next() {
                            Some(Token::Op(Operator::Assign)) => {
                                break;
                            }
                            Some(_) => continue,
                            None => {
                                return;
                            }
                        }
                    }
                }
                None => {
                    self.error_reporter.append(
                        "Parser",
                        iter.get_location(),
                        ErrorType::EofError(),
                    );
                }
            }
        }

        iter.next();

        let value = self.consume_expr(iter, 0);

        for (i, var) in vars.iter().enumerate() {
            match self.builder.load_unpack(&value, i, var) {
                Ok(_) => (),
                Err(s) => self.error_reporter.append(
                    "IrBuilder",
                    iter.get_location(),
                    ErrorType::IrError(s),
                ),
            }
        }
    }
}
