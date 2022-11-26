use super::util::FileLocation;

/// All possible types used by parser.
///
/// `Set`, `List` and `Dict` are three special classes that should be implemented by code generator
/// backend. Specially, `Any` means any type except `Nil` is possible.
pub enum Type {
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
    Assign,
}

#[derive(Debug)]
pub struct Stat {
    pub location: FileLocation,
    pub val: Stat_
}

#[derive(Clone, Copy, Debug)]
pub enum OpInfix {
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

#[derive(Clone, Copy, Debug)]
pub enum OpPrefix {
    Not,
    Neg
}

#[derive(Clone, Copy, Debug)]
pub enum OpPostfix{
    Index,
    Call,
    Pipeline
}

#[derive(Debug)]
pub enum Expr_ {
    Prefix(OpPrefix, Box<Expr>),
    Postfix(OpPostfix, Box<Expr>),
    Infix(OpInfix, Box<Expr>, Box<Expr>),
    Id(String),
    Const(Const),
    Error
}

#[derive(Debug)]
pub struct Expr{
    pub location: FileLocation,
    pub val: Expr_
}

#[derive(Debug)]
pub enum Const {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
}

pub struct Ast{
    
}
