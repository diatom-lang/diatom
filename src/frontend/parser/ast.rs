use std::fmt::Debug;

use crate::diagnostic::Loc;

/// All possible types used by parser.
///
/// `Set`, `List` and `Dict` are three special classes that should be implemented by code generator
/// backend. Specially, `Any` means any type except `Nil` is possible.
pub enum _Type {
    Any,
    Float,
    Int,
    Str,
    Class(String),
    Function,
    Nil,
}

#[derive(Debug)]
pub enum Stat_ {
    Expr(Box<Expr>),
}

pub struct Stat {
    pub loc: Loc,
    pub val: Stat_,
}

impl Debug for Stat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("").field(&self.val).finish()
    }
}

impl Stat {
    pub fn new(val: Stat_, loc: Loc) -> Self {
        Self { loc, val }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum OpInfix {
    Assign,
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
    Comma,
}

#[derive(Clone, Copy, Debug)]
pub enum OpPrefix {
    Not,
    Neg,
}

#[derive(Clone, Copy, Debug)]
pub enum OpPostfix {
    Index,
    Call,
}

#[derive(Debug)]
pub enum Expr_ {
    Block(Vec<Expr>),
    /// An `if..then..elsif..then..else..end`
    /// Expression is in order
    If(Vec<Expr>),
    Prefix(OpPrefix, Box<Expr>),
    /// `call func arg` or `index array num`
    /// If func call does not have any arguments, the third parameter is None.
    Postfix(OpPostfix, Box<Expr>, Option<Box<Expr>>),
    Infix(OpInfix, Box<Expr>, Box<Expr>),
    Id(String),
    Const(Const),
    Error,
}

pub struct Expr {
    pub loc: Loc,
    pub val: Expr_,
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.val {
            Expr_::Block(b) => f.debug_list().entries(b.iter()).finish(),
            Expr_::Prefix(op, expr) => f.debug_tuple("").field(&op).field(&expr).finish(),
            Expr_::Postfix(op, e1, e2) => {
                f.debug_tuple("").field(&e1).field(&op).field(&e2).finish()
            }
            Expr_::Infix(op, e1, e2) => f.debug_tuple("").field(&e1).field(&op).field(&e2).finish(),
            Expr_::Id(id) => write!(f, "{:?}", id),
            Expr_::Const(c) => write!(f, "{:?}", c),
            Expr_::Error => write!(f, "Error"),
            Expr_::If(v) => f.debug_tuple("").field(&"if").field(&v).finish(),
        }
    }
}

pub enum Const {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    List(Option<Box<Expr>>),
    Nil,
}

impl Debug for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Const::Int(i) => write!(f, "{}", i),
            Const::Float(fp) => write!(f, "{}", fp),
            Const::Str(s) => write!(f, "{}", s),
            Const::Bool(b) => write!(f, "{}", b),
            Const::List(l) => f.debug_list().entry(&l).finish(),
            Const::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Default)]
pub struct Ast {
    pub statements: Vec<Stat>,
}
