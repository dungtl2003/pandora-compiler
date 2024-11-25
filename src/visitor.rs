use crate::ast::{
    AngleBracketedArg, AngleBracketedArgs, Expr, ExprKind, GenericArg, GenericArgs, Ident, Local,
    LocalKind, Path, PathSegment, Stmt, StmtKind, Ty, TyKind,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ControlFlow<B, C = ()> {
    /// Move on to the next phase of the operation as normal.
    Continue(C),
    /// Exit the operation without running subsequent phases.
    Break(B),
    // Yes, the order of the variants doesn't match the type parameters.
    // They're in this order so that `ControlFlow<A, B>` <-> `Result<B, A>`
    // is a no-op conversion in the `Try` implementation.
}

pub trait VisitorResult {
    type Residual;
    fn output() -> Self;
    fn from_residual(residual: Self::Residual) -> Self;
    fn from_branch(b: ControlFlow<Self::Residual>) -> Self;
    fn branch(self) -> ControlFlow<Self::Residual>;
}

impl VisitorResult for () {
    type Residual = ();

    fn output() -> Self {}
    fn from_residual(_: Self::Residual) -> Self {}
    fn from_branch(_: ControlFlow<Self::Residual>) -> Self {}
    fn branch(self) -> ControlFlow<Self::Residual> {
        ControlFlow::Continue(())
    }
}
impl<T> VisitorResult for ControlFlow<T> {
    type Residual = T;

    fn output() -> Self {
        ControlFlow::Continue(())
    }
    fn from_residual(residual: Self::Residual) -> Self {
        ControlFlow::Break(residual)
    }
    fn from_branch(b: Self) -> Self {
        b
    }
    fn branch(self) -> Self {
        self
    }
}

pub trait Visitor<'ast>: Sized {
    /// The result type of the `visit_*` methods. Can be either `()`,
    /// or `ControlFlow<T>`.
    type Result: VisitorResult;

    fn visit_expr(&mut self, expr: &'ast Expr) -> Self::Result {
        walk_expr(self, expr)
    }

    fn visit_path(&mut self, path: &'ast Path) -> Self::Result {
        walk_path(self, path)
    }

    fn visit_path_segment(&mut self, path_segment: &'ast PathSegment) -> Self::Result {
        walk_path_segment(self, path_segment)
    }

    fn visit_generic_args(&mut self, generic_args: &'ast GenericArgs) -> Self::Result {
        walk_generic_args(self, generic_args)
    }

    fn visit_angle_bracketed_args(
        &mut self,
        angle_bracketed_args: &'ast AngleBracketedArgs,
    ) -> Self::Result {
        walk_angle_bracketed_args(self, angle_bracketed_args)
    }

    fn visit_angle_bracketed_arg(
        &mut self,
        angle_bracketed_arg: &'ast AngleBracketedArg,
    ) -> Self::Result {
        walk_angle_bracketed_arg(self, angle_bracketed_arg)
    }

    fn visit_generic_arg(&mut self, generic_arg: &'ast GenericArg) -> Self::Result {
        walk_generic_arg(self, generic_arg)
    }

    fn visit_ty(&mut self, ty: &'ast Ty) -> Self::Result {
        walk_ty(self, ty)
    }

    fn visit_stmt_block(&mut self, stmts: &'ast Vec<Box<Stmt>>) -> Self::Result {
        walk_stmt_block(self, stmts)
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) -> Self::Result {
        walk_stmt(self, stmt)
    }

    fn visit_stmt_if(
        &mut self,
        condition: &'ast Expr,
        block: &'ast Stmt,
        optional_else: Option<&'ast Stmt>,
    ) -> Self::Result {
        walk_stmt_if(self, condition, block, optional_else)
    }

    fn visit_stmt_var(&mut self, local: &'ast Local) -> Self::Result {
        walk_stmt_var(self, local)
    }

    fn visit_stmt_expr(&mut self, expr: &'ast Expr) -> Self::Result {
        walk_stmt_expr(self, expr)
    }

    fn visit_stmt_while(&mut self, condition: &'ast Expr, block: &'ast Stmt) -> Self::Result {
        walk_stmt_while(self, condition, block)
    }

    fn visit_stmt_for(
        &mut self,
        ident: &'ast Ident,
        expr: &'ast Expr,
        block: &'ast Stmt,
    ) -> Self::Result {
        walk_stmt_for(self, ident, expr, block)
    }
}

pub fn walk_stmt<'ast, V: Visitor<'ast>>(visitor: &mut V, stmt: &'ast Stmt) -> V::Result {
    let Stmt { kind, span: _ } = stmt;
    match kind {
        StmtKind::Expr(expr) => {
            visitor.visit_stmt_expr(expr);
        }
        StmtKind::Var(local) => {
            visitor.visit_stmt_var(local);
        }
        StmtKind::If(condition, block, optional_else) => {
            visitor.visit_stmt_if(condition, block, optional_else.as_deref());
        }
        StmtKind::Block(block) => {
            visitor.visit_stmt_block(block);
        }
        StmtKind::While(condition, block) => {
            visitor.visit_stmt_while(condition, block);
        }
        StmtKind::For(ident, expr, block) => {
            visitor.visit_stmt_for(ident, expr, block);
        }
        _ => {}
    }

    V::Result::output()
}

pub fn walk_stmt_while<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    condition: &'ast Expr,
    block: &'ast Stmt,
) -> V::Result {
    visitor.visit_expr(condition);
    visitor.visit_stmt(block);
    V::Result::output()
}

pub fn walk_stmt_for<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    _ident: &'ast Ident,
    expr: &'ast Expr,
    block: &'ast Stmt,
) -> V::Result {
    visitor.visit_expr(expr);
    visitor.visit_stmt(block);
    V::Result::output()
}

pub fn walk_stmt_block<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    stmts: &'ast Vec<Box<Stmt>>,
) -> V::Result {
    for stmt in stmts {
        visitor.visit_stmt(stmt);
    }
    V::Result::output()
}

pub fn walk_stmt_if<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    condition: &'ast Expr,
    block: &'ast Stmt,
    optional_else: Option<&'ast Stmt>,
) -> V::Result {
    visitor.visit_expr(condition);
    visitor.visit_stmt(block);

    if let Some(else_block) = optional_else {
        visitor.visit_stmt(else_block);
    }

    V::Result::output()
}

pub fn walk_stmt_expr<'ast, V: Visitor<'ast>>(visitor: &mut V, expr: &'ast Expr) -> V::Result {
    visitor.visit_expr(expr)
}

pub fn walk_stmt_var<'ast, V: Visitor<'ast>>(visitor: &mut V, local: &'ast Local) -> V::Result {
    let Local {
        binding_mode: _,
        ident: _,
        ty,
        kind,
        span: _,
    } = local;

    visitor.visit_ty(ty);

    match kind {
        LocalKind::Init(expr) => {
            visitor.visit_expr(expr);
        }
        LocalKind::Decl => {}
    }

    V::Result::output()
}

pub fn walk_path<'ast, V: Visitor<'ast>>(visitor: &mut V, path: &'ast Path) -> V::Result {
    for segment in &path.segments {
        visitor.visit_path_segment(segment);
    }

    V::Result::output()
}

pub fn walk_path_segment<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    segment: &'ast PathSegment,
) -> V::Result {
    let PathSegment { ident: _, args } = segment;

    if let Some(args) = args {
        visitor.visit_generic_args(args);
    }

    V::Result::output()
}

pub fn walk_generic_args<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    args: &'ast GenericArgs,
) -> V::Result {
    match args {
        GenericArgs::AngleBracketed(args) => visitor.visit_angle_bracketed_args(args),
    };

    V::Result::output()
}

pub fn walk_angle_bracketed_args<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    args: &'ast AngleBracketedArgs,
) -> V::Result {
    for arg in &args.args {
        visitor.visit_angle_bracketed_arg(arg);
    }

    V::Result::output()
}

pub fn walk_angle_bracketed_arg<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    arg: &'ast AngleBracketedArg,
) -> V::Result {
    match arg {
        AngleBracketedArg::Arg(arg) => visitor.visit_generic_arg(arg),
    };

    V::Result::output()
}

pub fn walk_generic_arg<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    arg: &'ast GenericArg,
) -> V::Result {
    match arg {
        GenericArg::Type(ty) => visitor.visit_ty(ty),
    };

    V::Result::output()
}

pub fn walk_ty<'ast, V: Visitor<'ast>>(visitor: &mut V, ty: &'ast Ty) -> V::Result {
    match &ty.kind {
        TyKind::Path(path) => {
            visitor.visit_path(path);
        }
        TyKind::Never => {}
    }

    V::Result::output()
}

pub fn walk_expr<'ast, V: Visitor<'ast>>(visitor: &mut V, expression: &'ast Expr) -> V::Result {
    let Expr { kind, span: _ } = expression;

    match kind {
        ExprKind::Binary(_op, left_expression, right_expression) => {
            visitor.visit_expr(left_expression);
            visitor.visit_expr(right_expression);
        }

        ExprKind::Unary(_op, subexpression) => {
            visitor.visit_expr(subexpression);
        }

        ExprKind::Assign(_lhs, _rhs, _span) => {
            visitor.visit_expr(_lhs);
            visitor.visit_expr(_rhs);
        }
        ExprKind::AssignOp(_op, _lhs, _rhs) => {
            visitor.visit_expr(_lhs);
            visitor.visit_expr(_rhs);
        }
        ExprKind::Literal(_token) => {}
        ExprKind::Path(path) => {
            visitor.visit_path(path);
        }
        ExprKind::Cast(expr, ty) => {
            visitor.visit_expr(expr);
            visitor.visit_ty(ty);
        }
    }

    V::Result::output()
}
