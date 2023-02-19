use crate::file_manager::{DisplayableOsString, Loc};

#[derive(Clone)]
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
        variable: Box<Expr>,
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
    Is,
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

#[derive(Clone)]
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
    OpenRange {
        loc: Loc,
        lhs: Box<Expr>,
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
    _Module {
        loc: Loc,
        path: DisplayableOsString,
    },
    Error,
}

impl Expr {
    pub fn get_loc(&self) -> Loc {
        match self {
            Expr::Block { loc, .. } => loc,
            Expr::If { loc, .. } => loc,
            Expr::Prefix { loc, .. } => loc,
            Expr::Call { loc, .. } => loc,
            Expr::Index { loc, .. } => loc,
            Expr::Infix { loc, .. } => loc,
            Expr::Fn { loc, .. } => loc,
            Expr::Id { loc, .. } => loc,
            Expr::Parentheses { loc, .. } => loc,
            Expr::Const { loc, .. } => loc,
            Expr::_Module { loc, .. } => loc,
            Expr::Error => unreachable!(),
            Expr::OpenRange { loc, .. } => loc,
        }
        .clone()
    }
}

#[derive(Clone)]
pub enum Const {
    Unit,
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    List(Vec<Expr>),
    Table(Vec<(String, Expr, Loc)>),
}
