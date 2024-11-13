mod ident;
pub mod pretty_print;
mod token;
mod tokenstream;

use core::fmt;
use std::fmt::{Display, Formatter};

pub use tokenstream::{DelimSpan, Spacing, TokenStream, TokenTree, TokenTreeCursor};

pub use token::{
    BinOpToken, CommentKind, Delimiter, DocStyle, IdentIsRaw, Lit, LitKind, Token, TokenKind,
};

use crate::span_encoding::{Span, Spanned};

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind {
    /// A binary operation (e.g. `a + b`, `a * b`).
    Binary(BinOp, Box<Expr>, Box<Expr>),
    /// An unary operation (e.g. `!a`, `-a`).
    Unary(UnOp, Box<Expr>),
    /// A literal (e.g. `124`, `"foo"`).
    Literal(token::Lit),
    /// An assignment (`a = foo()`).
    /// The `Span` argument is the span of the `=` token.
    Assign(Box<Expr>, Box<Expr>, Span),
    /// An assignment with an operator.
    ///
    /// E.g., `a += 1`.
    AssignOp(BinOp, Box<Expr>, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.kind {
            ExprKind::Binary(op, ..) => {
                write!(f, "Binary operation: {}", op)
            }
            ExprKind::Unary(op, _) => {
                write!(f, "{:?}", op)
            }
            ExprKind::Literal(lit) => {
                write!(f, "{}", lit)
            }
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub enum UnOp {
    /// The `!` operator for logical inversion.
    Not,
    /// The `-` operator for negation.
    Ne,
}

pub type BinOp = Spanned<BinOpKind>;

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self.node)
    }
}

#[derive(Debug)]
pub enum BinOpKind {
    /// The `+` operator (addition).
    Add,
    /// The `-` operator (subtraction).
    Sub,
    /// The `*` operator (multiplication).
    Mul,
    /// The `/` operator (division).
    Div,
    /// The `%` operator (modulus).
    Mod,
    /// The `==` operator (equality).
    Eq,
    /// The `!=` operator (not equal to).
    Ne,
    /// The `<` operator (less than).
    Lt,
    /// The `<=` operator (less than or equal to).
    Le,
    /// The `>` operator (greater than).
    Gt,
    /// The `>=` operator (greater than or equal to).
    Ge,
    /// The `&&` operator (and).
    And,
    /// The `||` operator (or).
    Or,
    /// The `&` operator (bitwise and).
    BitAnd,
    /// The `|` operator (bitwise or).
    BitOr,
    /// The `^` operator (bitwise xor).
    BitXor,
    /// The `<<` operator (shift left).
    Shl,
    /// The `>>` operator (shift right).
    Shr,
}
