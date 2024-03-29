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
    /// until
    Until,
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
    /// in
    In,
    /// for
    For,
    /// return
    Return,
    /// break
    Break,
    /// continue
    Continue,
    /// loop
    Loop,
    /// def
    Def,
    /// fn
    Fn,
    /// begin
    Begin,
    /// import
    Import,
    /// from
    From,
    /// as
    As,
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
    Rem,
    /// ".."
    Range,
    /// is
    Is,
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
    /// "<="
    Le,
    /// "="
    Assign,
    /// ","
    Comma,
    /// "."
    Member,
    /// "|"
    BitOr,
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
    /// "::"
    DoubleColon,
    /// ;
    SemiColon,
    /// @
    At,
    /// <-
    LArrow,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Str(s) => write!(f, "str({s})"),
            Token::Integer(i) => write!(f, "int({i})"),
            Token::Float(fp) => write!(f, "float({fp})"),
            Token::Id(id) => write!(f, "id({id})"),
            Token::Key(key) => write!(f, "`{key}`"),
            Token::Op(op) => write!(f, "\"{op}\""),
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
            Keyword::Until => "until",
            Keyword::End => "end",
            Keyword::If => "if",
            Keyword::Then => "then",
            Keyword::Else => "else",
            Keyword::Elsif => "elsif",
            Keyword::In => "in",
            Keyword::For => "for",
            Keyword::Return => "return",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::Loop => "loop",
            Keyword::Def => "def",
            Keyword::Begin => "begin",
            Keyword::Fn => "fn",
            Keyword::Import => "import",
            Keyword::From => "from",
            Keyword::As => "as",
        };
        write!(f, "{name}")
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
            Operator::Rem => "%",
            Operator::Range => "..",
            Operator::Is => "is",
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
            Operator::BitOr => "|",
            Operator::LPar => "(",
            Operator::RPar => ")",
            Operator::LBrk => "[",
            Operator::RBrk => "]",
            Operator::LBrc => "{",
            Operator::RBrc => "}",
            Operator::DoubleColon => "::",
            Operator::SemiColon => ";",
            Operator::At => "@",
            Operator::LArrow => "<-",
        };
        write!(f, "{name}")
    }
}

impl Debug for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}
