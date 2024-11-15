mod ident;
pub mod pretty_print;
mod token;
mod tokenstream;

use core::fmt;
use std::fmt::{Display, Formatter};

pub use ident::Ident;
use strum_macros::{AsRefStr, EnumString};
pub use tokenstream::{pprint, DelimSpan, Spacing, TokenStream, TokenTree, TokenTreeCursor};

pub use token::{
    BinOpToken, CommentKind, Delimiter, DocStyle, IdentIsRaw, Lit, LitKind, Token, TokenKind,
};

use crate::span_encoding::{Span, Spanned};

pub struct BindingMode(pub Mutability);

impl Display for BindingMode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub enum Mutability {
    Immutable,
    Mutable,
}

impl Display for Mutability {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Mutability::Immutable => write!(f, "immutable"),
            Mutability::Mutable => write!(f, "mutable"),
        }
    }
}

impl BindingMode {
    pub fn prefix_str(self) -> &'static str {
        match self.0 {
            Mutability::Immutable => "",
            Mutability::Mutable => "mut ",
        }
    }
}

pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

pub enum StmtKind {
    /// An expression statement: `expr;`.
    Expr(Box<Expr>),
    /// A block statement: `{ stmt* }`.
    Block(Vec<Stmt>),
    /// An `if` statement: 'if' expr block ('elif' expr block)* ('else' block)?
    If(Vec<(Expr, Vec<Stmt>)>, Option<Vec<Stmt>>),
    /// A 'break' statement.
    Break,
    /// A 'continue' statement.
    Continue,
    /// A 'return' statement: 'return' expr? ';'
    Return(Option<Box<Expr>>),
    /// A variable declaration: 'var' 'mut'? ident: type ('=' expr)? ';'
    Var(Box<Local>),
}

/// A "Path" is essentially Pandora's notion of a name.
///
/// It's represented as a sequence of identifiers,
/// along with a bunch of supporting information.
///
/// E.g., `std::cmp::PartialEq`.
#[derive(Debug)]
pub struct Path {
    pub span: Span,
    /// The segments in the path: the things separated by `::`.
    /// Global paths begin with `kw::PathRoot`.
    pub segments: Vec<PathSegment>,
}

/// A segment of a path: an identifier and a set of types.
///
/// E.g., `std`, `String` or `Box<T>`.
#[derive(Debug)]
pub struct PathSegment {
    /// The identifier portion of this path segment.
    pub ident: Ident,
    pub args: Option<Box<GenericArgs>>,
}

/// The generic arguments and associated item constraints of a path segment.
///
/// E.g., `<A, B>` as in `Foo<A, B>`.
#[derive(Debug)]
pub enum GenericArgs {
    /// The `<A, B, C>` in `foo::bar::baz::<A, B, C>`.
    AngleBracketed(AngleBracketedArgs),
}

/// A path like `Foo<T, E>`.
#[derive(Debug)]
pub struct AngleBracketedArgs {
    /// The overall span.
    pub span: Span,
    /// The comma separated parts in the `<...>`.
    pub args: Vec<AngleBracketedArg>,
}

/// Either an argument for a generic parameter or a constraint on an associated item.
#[derive(Debug)]
pub enum AngleBracketedArg {
    /// A generic argument for a generic parameter.
    Arg(GenericArg),
}

/// Concrete argument in the sequence of generic args.
#[derive(Debug)]
pub enum GenericArg {
    /// `Bar` in `Foo<Bar>`.
    Type(Box<Ty>),
}

#[derive(Debug)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum TyKind {
    /// A path (`module::module::...::Type`).
    ///
    /// Type parameters are stored in the `Path` itself.
    Path(Path),
}

/// Local represents a `var` statement. e.g. `var mut <ident>:<ty> = <expr>;`.
pub struct Local {
    pub binding_mode: BindingMode,
    pub ident: Ident,
    pub ty: Box<Ty>,
    pub kind: LocalKind,
    pub span: Span,
}

pub enum LocalKind {
    /// Local declaration.
    /// Example: `let x: int;`
    Decl,
    /// Local declaration with an initializer.
    /// Example: `let x: int = y;`
    Init(Box<Expr>),
}

#[derive(Debug, EnumString, AsRefStr, PartialEq)]
#[strum(serialize_all = "lowercase")] // This ensures matching with lowercase strings.
pub enum PrimitiveTy {
    Int,
    Float,
    Bool,
    Char,
}

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
