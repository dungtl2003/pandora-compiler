mod token;
mod tokenstream;

pub use tokenstream::{pretty_print, DelimSpan, Spacing, TokenStream, TokenTree};

pub use token::{
    BinOpToken, CommentKind, Delimiter, DocStyle, IdentIsRaw, Lit, LitKind, Token, TokenKind,
};

pub struct Expr {
    pub kind: ExprKind,
}

pub enum ExprKind {
    /// A binary operation (e.g. `a + b`, `a * b`).
    Binary(BinOpKind, Box<Expr>, Box<Expr>),
    /// An unary operation (e.g. `!a`, `-a`).
    Unary(UnaryKind, Box<Expr>),
    /// A literal (e.g. `124`, `"foo"`).
    Literal(token::Lit),
}

pub enum UnaryKind {
    /// The `!` operator for logical inversion.
    Not,
    /// The `-` operator for negation.
    Ne,
}

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
