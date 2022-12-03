use std::fmt::{Debug, Display};

#[derive(Clone)]
pub enum Token {
    Str(String),
    Integer(i64),
    Float(f64),
    Id(String),
    Key(Keyword),
    Op(Operator),
}

#[derive(Clone, Copy)]
pub enum Keyword {
    /// true
    True,
    /// false
    False,
    /// do
    Do,
    /// end
    End,
    /// if
    If,
    /// then
    Then,
    /// else
    Else,
    /// elsif
    Elsif,
    /// case
    Case,
    /// in
    In,
    /// for
    For,
    /// nil
    Nil,
    /// assert
    Assert,
    /// return
    Return,
    /// break
    Break,
    /// continue
    Continue,
    /// loop
    Loop,
    /// class
    Class,
    /// def
    Def,
    /// begin
    Begin,
}

/// A enum of all operators
#[derive(Clone, Copy)]
pub enum Operator {
    /// "+"
    Plus,
    /// "-"
    Minus,
    /// "**"
    Exp,
    /// "*"
    Mul,
    /// "//"
    DivFloor,
    /// "/"
    Div,
    /// "%"
    Mod,
    /// ".."
    Range,
    /// "and"
    And,
    /// "or"
    Or,
    /// "not"
    Not,
    /// ">"
    Gt,
    /// ">="
    Ge,
    /// "=="
    Eq,
    /// "<>"
    Ne,
    /// "<"
    Lt,
    /// "<"
    Le,
    /// "="
    Assign,
    /// ","
    Comma,
    /// "."
    Member,
    /// "|>"
    Pipeline,
    /// "("
    LPar,
    /// ")"
    RPar,
    /// "["
    LBrk,
    /// "]"
    RBrk,
    /// "{"
    LBrc,
    /// "}"
    RBrc,
    /// ":"
    Colon,
    /// $
    Call,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Str(s) => write!(f, "str({})", s),
            Token::Integer(i) => write!(f, "int({})", i),
            Token::Float(fp) => write!(f, "float({})", fp),
            Token::Id(id) => write!(f, "id({})", id),
            Token::Key(key) => write!(f, "`{}`", key),
            Token::Op(op) => write!(f, "\"{}\"", op),
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::Do => "do",
            Keyword::End => "end",
            Keyword::If => "if",
            Keyword::Then => "then",
            Keyword::Else => "else",
            Keyword::Elsif => "elsif",
            Keyword::Case => "case",
            Keyword::In => "in",
            Keyword::For => "for",
            Keyword::Nil => "nil",
            Keyword::Assert => "assert",
            Keyword::Return => "return",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::Loop => "loop",
            Keyword::Class => "class",
            Keyword::Def => "def",
            Keyword::Begin => "begin",
        };
        write!(f, "{}", name)
    }
}

impl Debug for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Exp => "**",
            Operator::Mul => "*",
            Operator::DivFloor => "//",
            Operator::Div => "/",
            Operator::Mod => "%",
            Operator::Range => "..",
            Operator::And => "and",
            Operator::Or => "or",
            Operator::Not => "not",
            Operator::Gt => ">",
            Operator::Ge => ">=",
            Operator::Eq => "==",
            Operator::Ne => "<>",
            Operator::Lt => "<",
            Operator::Le => "<=",
            Operator::Assign => "=",
            Operator::Comma => ",",
            Operator::Member => ".",
            Operator::Pipeline => "|>",
            Operator::LPar => "(",
            Operator::RPar => ")",
            Operator::LBrk => "[",
            Operator::RBrk => "]",
            Operator::LBrc => "{",
            Operator::RBrc => "}",
            Operator::Colon => ":",
            Operator::Call => "$",
        };
        write!(f, "{}", name)
    }
}

impl Debug for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}
