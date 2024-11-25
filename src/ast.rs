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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
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
/// E.g., `std`, `String` or `Box::<T>`.
#[derive(Debug, Clone)]
pub struct PathSegment {
    /// The identifier portion of this path segment.
    pub ident: Ident,
    pub args: Option<Box<GenericArgs>>,
}

/// The generic arguments and associated item constraints of a path segment.
///
/// E.g., `<A, B>` as in `Foo<A, B>`.
#[derive(Debug, Clone)]
pub enum GenericArgs {
    /// The `<A, B, C>` in `foo::bar::baz::<A, B, C>`.
    AngleBracketed(AngleBracketedArgs),
}

/// A path like `Foo<T, E>`.
#[derive(Debug, Clone)]
pub struct AngleBracketedArgs {
    /// The overall span.
    pub span: Span,
    /// The comma separated parts in the `<...>`.
    pub args: Vec<AngleBracketedArg>,
}

/// Either an argument for a generic parameter or a constraint on an associated item.
#[derive(Debug, Clone)]
pub enum AngleBracketedArg {
    /// A generic argument for a generic parameter.
    Arg(GenericArg),
}

/// Concrete argument in the sequence of generic args.
#[derive(Debug, Clone)]
pub enum GenericArg {
    /// `Bar` in `Foo<Bar>`.
    Type(Box<Ty>),
}

#[derive(Debug, Clone)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TyKind {
    /// A path (`module::module::...::Type`).
    ///
    /// Type parameters are stored in the `Path` itself.
    Path(Path),

    Never,
}

impl Display for TyKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TyKind::Never => write!(f, "void"),
            TyKind::Path(path) => write!(f, "Path:{:?}", path.segments),
        }
    }
}

/// Local represents a `var` statement. e.g. `var mut <ident>:<ty> = <expr>;`.
#[derive(Debug)]
pub struct Local {
    pub binding_mode: BindingMode,
    pub ident: Ident,
    pub ty: Box<Ty>,
    pub kind: LocalKind,
    pub span: Span,
}

#[derive(Debug)]
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
    /// Variable reference, possibly containing `::` and/or type
    /// parameters (e.g., `foo::bar::<baz>`).
    Path(Box<Path>),
    /// A cast (e.g., `foo as float`).
    Cast(Box<Expr>, Box<Ty>),
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

#[derive(Debug)]
pub struct Visibility {
    pub kind: VisibilityKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum VisibilityKind {
    Public,
}

#[derive(Debug)]
pub struct Item {
    pub span: Span,
    pub kind: ItemKind,
    pub vis: Option<Visibility>,
    /// The name of the item.
    pub ident: Ident,
}

#[derive(Debug)]
pub enum ItemKind {
    /// E.g. `import foo;`, `import foo::bar` or `import foo::bar as baz`.
    Import(ImportTree),

    /// E.g. `class Foo { ... }`.
    Class(Box<Class>),

    /// E.g. `fn foo() { ... }`.
    Fun(Box<Fun>),

    /// E.g. `interface Foo { ... }`.
    Interface(Box<Interface>),
}

#[derive(Debug)]
pub struct Interface {
    pub generics: Vec<GenericParam>,
    pub ext_clause: Option<ExtClause>,
    pub body: InterfaceBody,
}

#[derive(Debug)]
pub struct InterfaceBody {
    pub methods: Vec<Item>,
}

/// A function definition.
#[derive(Debug)]
pub struct Fun {
    pub generics: Vec<GenericParam>,
    pub sig: FunSig,
    pub body: Option<Stmt>,
}

/// The signature of a function.
#[derive(Debug)]
pub struct FunSig {
    pub inputs: (Option<SelfParam>, Vec<FunParam>),
    pub output: FunRetTy,

    pub span: Span,
}

#[derive(Debug)]
pub struct SelfParam {
    pub kind: SelfKind,
    pub span: Span,
}

/// A parameter in a function header.
/// E.g., `bar: usize` as in `fn foo(bar: usize)`.
#[derive(Debug)]
pub struct FunParam {
    pub ty: Box<Ty>,
    pub ident: Ident,
    pub span: Span,
}

#[derive(Debug)]
pub enum FunRetTy {
    /// Returns type is not specified.
    ///
    /// Functions default to `void`.
    /// Span points to where return type would be inserted.
    Default(Span),
    /// Everything else.
    Ty(Box<Ty>),
}

#[derive(Debug)]
pub struct Class {
    pub generics: Vec<GenericParam>,
    pub ext_clause: Option<ExtClause>,
    pub impl_clause: Option<ImplClause>,
    pub body: ClassBody,
}

#[derive(Debug)]
pub struct ClassBody {
    pub fields: Vec<ClassField>,
    pub methods: Vec<Item>,
}

#[derive(Debug)]
pub struct ClassField {
    pub vis: Option<Visibility>,
    pub kind: ClassFieldKind,
}

#[derive(Debug)]
pub enum ClassFieldKind {
    Const(Ident, Box<Ty>, Box<Expr>),
    Var(Ident, Box<Ty>),
}

#[derive(Debug)]
pub struct ExtClause {
    pub span: Span,
    pub ty: Box<Ty>,
}

#[derive(Debug)]
pub struct ImplClause {
    pub span: Span,
    pub tys: Vec<Box<Ty>>,
}

/// Represents type and const parameters attached to a declaration of
/// a function, enum, etc.
#[derive(Clone, Debug)]
pub struct Generics {
    pub params: Vec<GenericParam>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct GenericParam {
    pub ident: Ident,
    pub bounds: Vec<Ty>,
}

#[derive(Debug)]
pub struct ImportTree {
    pub prefix: Path,
    pub kind: ImportTreeKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ImportTreeKind {
    /// `import foo::bar` or `import foo::bar as baz`
    Simple(Option<Ident>),

    /// `import foo::*`
    Glob,
}

/// Alternative representation for `Arg`s describing `self` parameter of methods.
///
/// E.g., `&mut self` as in `fn foo(&mut self)`.
#[derive(Clone, Debug)]
pub enum SelfKind {
    /// `self`, `mut self`
    Value(Mutability),
    /// `self: TYPE`, `mut self: TYPE`
    Explicit(Box<Ty>, Mutability),
}
