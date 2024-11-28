mod ident;
pub mod pretty_print;
mod token;
mod tokenstream;

use core::fmt;
use std::fmt::{Display, Formatter};

pub use ident::Ident;
pub use tokenstream::{pprint, DelimSpan, Spacing, TokenStream, TokenTree, TokenTreeCursor};

pub use token::{
    BinOpToken, CommentKind, Delimiter, DocStyle, IdentIsRaw, Lit, LitKind, Token, TokenKind,
};

use crate::{
    span_encoding::{Span, Spanned},
    symbol::Symbol,
};

#[derive(Debug)]
pub struct Ast {
    pub stmts: Vec<Box<Stmt>>,
}

impl Ast {
    pub fn new(stmts: Vec<Box<Stmt>>) -> Self {
        Ast { stmts }
    }
}

#[derive(Debug, Clone)]
pub struct BindingMode(pub Mutability);

impl Display for BindingMode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// A function declaration: `fn foo() { ... }`.
    FuncDecl(Box<Fun>),
    /// An expression statement: `expr;`.
    Expr(Box<Expr>),
    /// A block statement: `{ stmt* }`.
    Block(Vec<Box<Stmt>>),
    /// An `if` statement: `if expr block_stmt (else (block_stmt | if_stmt))?`.
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    /// A 'break' statement.
    Break,
    /// A 'continue' statement.
    Continue,
    /// A 'return' statement: 'return' expr? ';'
    Return(Option<Box<Expr>>),
    /// A variable declaration: 'var' 'mut'? ident: type ('=' expr)? ';'
    Var(Box<Local>),
    /// A while loop: 'while' expr block_stmt
    While(Box<Expr>, Box<Stmt>),
    /// A for loop: 'for' ident 'in' expr block_stmt
    For(Ident, Box<Expr>, Box<Stmt>),
    /// An import statement: 'import' path ';'
    Import(Box<Path>),
    /// An empty statement: ';'.
    Empty,
}

/// A "Path" is essentially Pandora's notion of a name.
///
/// It's represented as a sequence of identifiers,
/// along with a bunch of supporting information.
///
/// E.g., `std::cmp::PartialEq`.
#[derive(Debug, Clone)]
pub struct Path {
    pub span: Span,
    /// The segments in the path: the things separated by `::`.
    /// Global paths begin with `kw::PathRoot`.
    pub segments: Vec<PathSegment>,
}

/// A segment of a path: an identifier and a set of types.
///
/// E.g., `std`
#[derive(Debug, Clone)]
pub struct PathSegment {
    /// The identifier portion of this path segment.
    pub ident: Ident,
}

#[derive(Debug, Clone)]
pub struct Ty {
    pub name: Symbol,
    pub span: Span,
}

/// Local represents a `var` statement. e.g. `var mut <ident>:<ty> = <expr>;`.
#[derive(Debug, Clone)]
pub struct Local {
    pub binding_mode: BindingMode,
    pub ident: Ident,
    pub ty: Ty,
    pub kind: LocalKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum LocalKind {
    /// Local declaration.
    /// Example: `let x: int;`
    Decl,
    /// Local declaration with an initializer.
    /// Example: `let x: int = y;`
    Init(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
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
    Identifier(Ident),
    /// A cast (e.g., `foo as float`).
    Cast(Box<Expr>, Ty),
    /// A function call.
    ///
    /// The first field resolves to the function itself,
    /// and the second field is the list of arguments.
    FunCall(Box<Expr>, Vec<Box<Expr>>),
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

#[derive(Debug, Clone)]
pub enum UnOp {
    /// The `!` operator for logical inversion.
    Not,
    /// The `-` operator for negation.
    Ne,
}

impl UnOp {
    pub fn to_rust_op_str(&self) -> String {
        (match self {
            UnOp::Not => "!",
            UnOp::Ne => "-",
        })
        .to_string()
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type BinOp = Spanned<BinOpKind>;

impl BinOp {
    pub fn to_rust_op_str(&self) -> String {
        (match self.node {
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Mul => "*",
            BinOpKind::Div => "/",
            BinOpKind::Mod => "%",
            BinOpKind::Eq => "==",
            BinOpKind::Ne => "!=",
            BinOpKind::Lt => "<",
            BinOpKind::Le => "<=",
            BinOpKind::Gt => ">",
            BinOpKind::Ge => ">=",
            BinOpKind::And => "&&",
            BinOpKind::Or => "||",
            BinOpKind::BitAnd => "&",
            BinOpKind::BitOr => "|",
            BinOpKind::BitXor => "^",
            BinOpKind::Shl => "<<",
            BinOpKind::Shr => ">>",
        })
        .to_string()
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self.node)
    }
}

#[derive(Debug, Clone)]
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

/// A function definition.
#[derive(Debug, Clone)]
pub struct Fun {
    pub sig: FunSig,
    pub body: Box<Stmt>,
}

/// The signature of a function.
#[derive(Debug, Clone)]
pub struct FunSig {
    pub name: Ident,
    pub inputs: Vec<FunParam>,
    pub output: Option<Ty>,

    pub span: Span,
}

/// A parameter in a function header.
/// E.g., `bar: usize` as in `fn foo(bar: usize)`.
#[derive(Debug, Clone)]
pub struct FunParam {
    pub ty: Ty,
    pub ident: Ident,
    pub span: Span,
}
