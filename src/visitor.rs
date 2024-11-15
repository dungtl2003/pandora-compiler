use crate::ast::{
    AngleBracketedArg, AngleBracketedArgs, Expr, ExprKind, GenericArg, GenericArgs, Path,
    PathSegment, Ty, TyKind, Stmt
};
use crate::span_encoding::Span;

pub trait Visitor<'ast>: Sized {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        walk_expr(self, expr);
    }


    fn visit_path(&mut self, path: &'ast Path) {
        walk_path(self, path);
    }

    fn visit_path_segment(&mut self, path_segment: &'ast PathSegment) {
        walk_path_segment(self, path_segment);
    }

    fn visit_generic_args(&mut self, generic_args: &'ast GenericArgs) {
        walk_generic_args(self, generic_args);
    }

    fn visit_angle_bracketed_args(&mut self, angle_bracketed_args: &'ast AngleBracketedArgs) {
        walk_angle_bracketed_args(self, angle_bracketed_args);
    }

    fn visit_angle_bracketed_arg(&mut self, angle_bracketed_arg: &'ast AngleBracketedArg) {
        walk_angle_bracketed_arg(self, angle_bracketed_arg);
    }

    fn visit_generic_arg(&mut self, generic_arg: &'ast GenericArg) {
        walk_generic_arg(self, generic_arg);
    }

    fn visit_ty(&mut self, ty: &'ast Ty) {
        walk_ty(self, ty);
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt);
    fn visit_stmt_if(&mut self, if_branch: &Vec<(Box<Expr>, Vec<Box<Stmt>>)>, else_branch: &Option<Vec<Box<Stmt>>>, span: &Span);
    fn visit_stmt_block(&mut self, stmt: &Vec<Box<Stmt>>);
}

pub fn walk_path<'ast, V: Visitor<'ast>>(visitor: &mut V, path: &'ast Path) {
    for segment in &path.segments {
        visitor.visit_path_segment(segment);
    }
}

pub fn walk_path_segment<'ast, V: Visitor<'ast>>(visitor: &mut V, segment: &'ast PathSegment) {
    let PathSegment { ident: _, args } = segment;

    if let Some(args) = args {
        visitor.visit_generic_args(args);
    }
}

pub fn walk_generic_args<'ast, V: Visitor<'ast>>(visitor: &mut V, args: &'ast GenericArgs) {
    match args {
        GenericArgs::AngleBracketed(args) => visitor.visit_angle_bracketed_args(args),
    }
}

pub fn walk_angle_bracketed_args<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    args: &'ast AngleBracketedArgs,
) {
    for arg in &args.args {
        visitor.visit_angle_bracketed_arg(arg);
    }
}

pub fn walk_angle_bracketed_arg<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    arg: &'ast AngleBracketedArg,
) {
    match arg {
        AngleBracketedArg::Arg(arg) => visitor.visit_generic_arg(arg),
    }
}

pub fn walk_generic_arg<'ast, V: Visitor<'ast>>(visitor: &mut V, arg: &'ast GenericArg) {
    match arg {
        GenericArg::Type(ty) => visitor.visit_ty(ty),
    }
}

pub fn walk_ty<'ast, V: Visitor<'ast>>(visitor: &mut V, ty: &'ast Ty) {
    match &ty.kind {
        TyKind::Path(path) => visitor.visit_path(path),
    }

}

pub fn walk_expr<'ast, V: Visitor<'ast>>(visitor: &mut V, expression: &'ast Expr) {
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
    }
}
