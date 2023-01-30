use crate::diagnostic::{Diagnoser, Diagnostic, DisplayableOsString, Loc, SharedFile};
use std::{ffi::OsString, fmt::Debug};

/// All possible types used by parser.
///
/// `Set`, `List` and `Dict` are three special classes that should be implemented by code generator
/// backend.
pub enum _Type {
    Any,
    Float,
    Int,
    Str,
    Class(String),
    Function,
}

pub enum Stmt_ {
    Expr(Expr),
    Continue,
    Break,
    /// Return, may not have a return value
    Return(Option<Expr>),
    /// A Data Type
    ///
    /// Name + Variant:(Name, members) + Functions(Always be `Def` variant)
    Data(String, Vec<(String, Vec<String>)>, Vec<Stmt>),
    /// An optional break condition & a body
    Loop(Option<Expr>, Vec<Stmt>),
    /// variables, iterator, statements
    For(Box<Expr>, Box<Expr>, Vec<Stmt>),
    /// Define a function
    ///
    /// Name + Parameters + Body + Bindings:(Name + value)
    Def(String, Vec<String>, Vec<Stmt>, Vec<(String, Expr)>),
    Error,
}

pub struct Stmt {
    pub loc: Loc,
    pub val: Stmt_,
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Stmt_::*;
        match &self.val {
            Expr(expr) => f.debug_tuple("").field(&expr).finish(),
            Continue => f.debug_tuple("continue").finish(),
            Break => f.debug_tuple("break").finish(),
            Return(expr) => {
                if let Some(expr) = expr {
                    f.debug_tuple("return").field(&expr).finish()
                } else {
                    f.debug_tuple("return").finish()
                }
            }
            Error => f.debug_tuple("error").finish(),
            Data(name, subtypes, methods) => f
                .debug_tuple("data")
                .field(name)
                .field(subtypes)
                .field(methods)
                .finish(),
            Loop(cond, body) => f.debug_tuple("loop").field(cond).field(body).finish(),
            For(vars, iter, expr) => f
                .debug_tuple("for")
                .field(vars)
                .field(&"in")
                .field(iter)
                .field(&"do")
                .field(expr)
                .finish(),
            Def(name, decl, body, binds) => f
                .debug_tuple("def")
                .field(name)
                .field(decl)
                .field(body)
                .field(binds)
                .finish(),
        }
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
    Rem,
    Exp,
    Comma,
    Member,
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
    Construct,
}

pub enum ConstPattern {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
}

impl Debug for ConstPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstPattern::Int(i) => write!(f, "{}", i),
            ConstPattern::Float(fl) => write!(f, "{}", fl),
            ConstPattern::Str(s) => write!(f, "\"{}\"", s),
            ConstPattern::Bool(b) => write!(f, "{}", b),
        }
    }
}
pub enum Pattern_ {
    /// Maybe a type or a variable
    ///
    /// For example `a`, `module_a.A_TYPE.B_SUBTYPE`
    Id(Vec<String>),
    /// a@A
    Bind(String, Box<Pattern>),
    /// A | B | C
    Or(Box<Pattern>, Box<Pattern>),
    /// A, B, C
    And(Box<Pattern>, Box<Pattern>),
    /// A${Pattern1 Pattern2 Pattern3 ...}
    Inner(Vec<String>, Vec<Pattern>),
    /// Constant value
    Const(ConstPattern),
    /// (Pattern)
    Parentheses(Box<Pattern>),
    Error,
}

pub struct Pattern {
    pub loc: Loc,
    pub val: Pattern_,
}

impl Pattern {
    pub fn new(loc: Loc, val: Pattern_) -> Self {
        Self { loc, val }
    }
}

impl Debug for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.val {
            Pattern_::Id(id) => write!(f, "{:?}", id),
            Pattern_::Bind(id, p) => f.debug_tuple("").field(id).field(&"@").field(p).finish(),
            Pattern_::Or(a, b) => f.debug_tuple("").field(a).field(&"|").field(b).finish(),
            Pattern_::And(a, b) => f.debug_tuple("").field(a).field(&",").field(b).finish(),
            Pattern_::Inner(name, p) => f.debug_tuple("").field(name).field(&"$").field(p).finish(),
            Pattern_::Const(c) => write!(f, "{:?}", c),
            Pattern_::Parentheses(p) => f.debug_tuple("").field(p).finish(),
            Pattern_::Error => write!(f, "Error"),
        }
    }
}
#[derive(Debug)]
pub enum Expr_ {
    /// A block of statements
    Block(Vec<Stmt>),
    /// An `if..then..elsif..then..else..end`
    /// Expression is in order
    /// Body is wrapped by a `block`, do not give warning on this
    If(Vec<Expr>),
    Prefix(OpPrefix, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Construct(Box<Expr>, Box<Expr>),
    Infix(OpInfix, Box<Expr>, Box<Expr>),
    Fn(Vec<String>, Box<Expr>),
    Id(String),
    Parentheses(Box<Expr>),
    Const(Const),
    /// Pattern + Guard + body
    Case(Box<Expr>, Vec<(Pattern, Option<Expr>, Stmt)>),
    Module(DisplayableOsString),
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
            Expr_::Infix(op, e1, e2) => f.debug_tuple("").field(&e1).field(&op).field(&e2).finish(),
            Expr_::Id(id) => write!(f, "{:?}", id),
            Expr_::Const(c) => write!(f, "{:?}", c),
            Expr_::Error => write!(f, "Error"),
            Expr_::If(v) => f.debug_tuple("").field(&"if").field(&v).finish(),
            Expr_::Parentheses(expr) => f
                .debug_tuple("")
                .field(&"(")
                .field(expr)
                .field(&")")
                .finish(),
            Expr_::Call(expr, call) => f.debug_tuple("Call").field(expr).field(call).finish(),
            Expr_::Index(expr, index) => f.debug_tuple("Call").field(expr).field(index).finish(),
            Expr_::Fn(parameters, expr) => f
                .debug_tuple("fn")
                .field(parameters)
                .field(&"=")
                .field(expr)
                .finish(),
            Expr_::Construct(id, init) => {
                f.debug_tuple("").field(id).field(&"$").field(init).finish()
            }
            Expr_::Case(expr, v) => f
                .debug_tuple("case")
                .field(expr)
                .field(&"of")
                .field(v)
                .finish(),
            Expr_::Module(path) => f.debug_tuple("require").field(path).finish(),
        }
    }
}

pub enum Const {
    Unit,
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    List(Vec<Expr>),
    Set(Vec<Expr>),
    // keys-values
    Dict(Vec<Expr>, Vec<Expr>),
}

impl Debug for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Const::Unit => write!(f, "()"),
            Const::Int(i) => write!(f, "{}", i),
            Const::Float(fp) => write!(f, "{}", fp),
            Const::Str(s) => write!(f, "{}", s),
            Const::Bool(b) => write!(f, "{}", b),
            Const::List(l) => f.debug_list().entries(l.iter()).finish(),
            Const::Set(val) => f.debug_set().entries(val).finish(),
            Const::Dict(keys, vals) => f.debug_map().entries(keys.iter().zip(vals.iter())).finish(),
        }
    }
}

/// Abstract syntax tree on a given file
pub struct Ast {
    pub statements: Vec<Stmt>,
    pub file_content: SharedFile,
    pub diagnoser: Diagnoser,
    has_eof_error: bool,
    has_non_eof_error: bool,
}

impl Ast {
    pub fn new(path: OsString, content: SharedFile) -> Self {
        Self {
            statements: vec![],
            file_content: content.clone(),
            diagnoser: Diagnoser::new(path, content),
            has_eof_error: false,
            has_non_eof_error: false,
        }
    }

    pub fn input_can_continue(&self) -> bool {
        self.has_eof_error && !self.has_non_eof_error
    }

    pub fn add_diagnostic(&mut self, diag: (Diagnostic, bool)) {
        if diag.1 {
            self.has_eof_error = true;
        } else if diag.0.severity >= codespan_reporting::diagnostic::Severity::Error
            && !self.has_eof_error
        // prevent other error triggered by eof being recorded
        {
            self.has_non_eof_error = true;
        }
        self.diagnoser.push(diag.0);
    }
}
