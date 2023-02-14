use crate::diagnostic::{Diagnoser, Diagnostic, DisplayableOsString, Loc, SharedFile};
use std::ffi::OsString;

pub enum Stmt {
    Expr {
        loc: Loc,
        expr: Expr,
    },
    Continue {
        loc: Loc,
    },
    Break {
        loc: Loc,
    },
    /// Return, may not have a return value
    Return {
        loc: Loc,
        value: Option<Expr>,
    },
    /// loop
    Loop {
        loc: Loc,
        condition: Option<Expr>,
        body: Vec<Stmt>,
    },
    /// for each loop
    For {
        loc: Loc,
        loop_variable: Box<Expr>,
        iterator: Box<Expr>,
        body: Vec<Stmt>,
    },
    /// Define a function
    Def {
        loc: Loc,
        name: String,
        parameters: Vec<(String, Loc)>,
        body: Vec<Stmt>,
    },
    Error,
}

#[derive(Clone, Copy)]
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
    DoubleColon,
    LArrow,
}

#[derive(Clone, Copy)]
pub enum OpPrefix {
    Not,
    Neg,
}

#[derive(Clone, Copy)]
pub enum OpPostfix {
    Index,
    Call,
}

pub enum Expr {
    Block {
        loc: Loc,
        body: Vec<Stmt>,
    },
    If {
        loc: Loc,
        conditional: Vec<(Expr, Vec<Stmt>)>,
        default: Option<Vec<Stmt>>,
    },
    Prefix {
        loc: Loc,
        op: OpPrefix,
        rhs: Box<Expr>,
    },
    Call {
        loc: Loc,
        lhs: Box<Expr>,
        parameters: Vec<Expr>,
    },
    Index {
        loc: Loc,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Infix {
        loc: Loc,
        op: OpInfix,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Fn {
        loc: Loc,
        parameters: Vec<(String, Loc)>,
        body: Box<Expr>,
    },
    Id {
        loc: Loc,
        name: String,
    },
    Parentheses {
        loc: Loc,
        content: Box<Expr>,
    },
    Const {
        loc: Loc,
        value: Const,
    },
    Module {
        loc: Loc,
        path: DisplayableOsString,
    },
    Error,
}

impl Expr {
    pub fn get_loc(&self) -> Loc {
        match self {
            Expr::Block { loc, body: _ } => loc,
            Expr::If {
                loc,
                conditional: _,
                default: _,
            } => loc,
            Expr::Prefix { loc, op: _, rhs: _ } => loc,
            Expr::Call {
                loc,
                lhs: _,
                parameters: _,
            } => loc,
            Expr::Index {
                loc,
                lhs: _,
                rhs: _,
            } => loc,
            Expr::Infix {
                loc,
                op: _,
                lhs: _,
                rhs: _,
            } => loc,
            Expr::Fn {
                loc,
                parameters: _,
                body: _,
            } => loc,
            Expr::Id { loc, name: _ } => loc,
            Expr::Parentheses { loc, content: _ } => loc,
            Expr::Const { loc, value: _ } => loc,
            Expr::Module { loc, path: _ } => loc,
            Expr::Error => unreachable!(),
        }
        .clone()
    }
}

pub enum Const {
    Unit,
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    List(Vec<Expr>),
    Table(Vec<(String, Expr, Loc)>),
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
