use crate::{
    ast::{BinOpKind, BinOpToken, Token, TokenKind},
    kw::Keyword,
};

#[derive(PartialEq, Debug)]
pub enum Fixity {
    /// The operator is left-associative
    Left,
    /// The operator is right-associative
    Right,
}

/// Associative operator with precedence.
///
/// This is the enum which specifies operator precedence and fixity to the parser.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum AssocOp {
    /// `+`
    Add,
    /// `-`
    Subtract,
    /// `*`
    Multiply,
    /// `/`
    Divide,
    /// `%`
    Modulus,
    /// `&&`
    LAnd,
    /// `||`
    LOr,
    /// `^`
    BitXor,
    /// `&`
    BitAnd,
    /// `|`
    BitOr,
    /// `<<`
    ShiftLeft,
    /// `>>`
    ShiftRight,
    /// `==`
    Equal,
    /// `<`
    Less,
    /// `<=`
    LessEqual,
    /// `!=`
    NotEqual,
    /// `>`
    Greater,
    /// `>=`
    GreaterEqual,
    /// `=`
    Assign,
    /// `?=` where ? is one of the BinOpToken
    AssignOp(BinOpToken),
    /// `as`
    As,
}

impl AssocOp {
    /// Creates a new AssocOP from a token
    pub fn from_token(t: &Token) -> Option<AssocOp> {
        use AssocOp::*;
        // Special case for `as`
        if t.is_keyword(Keyword::As) {
            return Some(As);
        }

        match t.kind {
            TokenKind::BinOpEq(k) => Some(AssignOp(k)),
            TokenKind::Eq => Some(Assign),
            TokenKind::BinOp(BinOpToken::Star) => Some(Multiply),
            TokenKind::BinOp(BinOpToken::Slash) => Some(Divide),
            TokenKind::BinOp(BinOpToken::Percent) => Some(Modulus),
            TokenKind::BinOp(BinOpToken::Plus) => Some(Add),
            TokenKind::BinOp(BinOpToken::Minus) => Some(Subtract),
            TokenKind::BinOp(BinOpToken::Shl) => Some(ShiftLeft),
            TokenKind::BinOp(BinOpToken::Shr) => Some(ShiftRight),
            TokenKind::BinOp(BinOpToken::And) => Some(BitAnd),
            TokenKind::BinOp(BinOpToken::Caret) => Some(BitXor),
            TokenKind::BinOp(BinOpToken::Or) => Some(BitOr),
            TokenKind::Lt => Some(Less),
            TokenKind::Le => Some(LessEqual),
            TokenKind::Ge => Some(GreaterEqual),
            TokenKind::Gt => Some(Greater),
            TokenKind::EqEq => Some(Equal),
            TokenKind::Ne => Some(NotEqual),
            TokenKind::AndAnd => Some(LAnd),
            TokenKind::OrOr => Some(LOr),
            _ => None,
        }
    }

    /// Gets the precedence of this operator
    pub fn precedence(&self) -> usize {
        use AssocOp::*;
        match *self {
            As => 11,
            Multiply | Divide | Modulus => 10,
            Add | Subtract => 9,
            ShiftLeft | ShiftRight => 8,
            BitAnd => 7,
            BitXor => 6,
            BitOr => 5,
            Less | Greater | LessEqual | GreaterEqual | Equal | NotEqual => 4,
            LAnd => 3,
            LOr => 2,
            Assign | AssignOp(_) => 1,
        }
    }

    /// Gets the fixity of this operator
    pub fn fixity(&self) -> Fixity {
        use AssocOp::*;
        // NOTE: it is a bug to have an operators that has same precedence but different fixities!
        match *self {
            Assign | AssignOp(_) => Fixity::Right,
            Multiply | Divide | Modulus | Add | Subtract | ShiftLeft | ShiftRight | BitAnd
            | BitXor | BitOr | Less | Greater | LessEqual | GreaterEqual | Equal | NotEqual
            | LAnd | LOr | As => Fixity::Left,
        }
    }

    pub fn to_ast_binop(&self) -> Option<BinOpKind> {
        use AssocOp::*;
        match *self {
            Less => Some(BinOpKind::Lt),
            Greater => Some(BinOpKind::Gt),
            LessEqual => Some(BinOpKind::Le),
            GreaterEqual => Some(BinOpKind::Ge),
            Equal => Some(BinOpKind::Eq),
            NotEqual => Some(BinOpKind::Ne),
            Multiply => Some(BinOpKind::Mul),
            Divide => Some(BinOpKind::Div),
            Modulus => Some(BinOpKind::Mod),
            Add => Some(BinOpKind::Add),
            Subtract => Some(BinOpKind::Sub),
            ShiftLeft => Some(BinOpKind::Shl),
            ShiftRight => Some(BinOpKind::Shr),
            BitAnd => Some(BinOpKind::BitAnd),
            BitXor => Some(BinOpKind::BitXor),
            BitOr => Some(BinOpKind::BitOr),
            LAnd => Some(BinOpKind::And),
            LOr => Some(BinOpKind::Or),
            Assign | AssignOp(_) | As => None,
        }
    }
}
