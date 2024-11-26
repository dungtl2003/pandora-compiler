use crate::ast::{
    AngleBracketedArg, AngleBracketedArgs, Class, Expr, ExprKind, Fun, FunRetTy, FunSig,
    GenericArg, GenericArgs, GenericParam, Ident, Interface, Item, ItemKind, Local, LocalKind,
    Path, PathSegment, Stmt, StmtKind, Ty, TyKind,
};

pub trait Visitor<'ast>: Sized {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        walk_expr(self, expr)
    }

    fn visit_path(&mut self, path: &'ast Path) {
        walk_path(self, path)
    }

    fn visit_path_segment(&mut self, path_segment: &'ast PathSegment) {
        walk_path_segment(self, path_segment)
    }

    fn visit_generic_args(&mut self, generic_args: &'ast GenericArgs) {
        walk_generic_args(self, generic_args)
    }

    fn visit_angle_bracketed_args(&mut self, angle_bracketed_args: &'ast AngleBracketedArgs) {
        walk_angle_bracketed_args(self, angle_bracketed_args)
    }

    fn visit_angle_bracketed_arg(&mut self, angle_bracketed_arg: &'ast AngleBracketedArg) {
        walk_angle_bracketed_arg(self, angle_bracketed_arg)
    }

    fn visit_generic_arg(&mut self, generic_arg: &'ast GenericArg) {
        walk_generic_arg(self, generic_arg)
    }

    fn visit_ty(&mut self, ty: &'ast Ty) {
        walk_ty(self, ty)
    }

    fn visit_stmt_block(&mut self, stmts: &'ast Vec<Box<Stmt>>) {
        walk_stmt_block(self, stmts)
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        walk_stmt(self, stmt)
    }

    fn visit_stmt_if(
        &mut self,
        condition: &'ast Expr,
        block: &'ast Stmt,
        optional_else: Option<&'ast Stmt>,
    ) {
        walk_stmt_if(self, condition, block, optional_else)
    }

    fn visit_stmt_var(&mut self, local: &'ast Local) {
        walk_stmt_var(self, local)
    }

    fn visit_stmt_expr(&mut self, expr: &'ast Expr) {
        walk_stmt_expr(self, expr)
    }

    fn visit_stmt_while(&mut self, condition: &'ast Expr, block: &'ast Stmt) {
        walk_stmt_while(self, condition, block)
    }

    fn visit_stmt_for(&mut self, ident: &'ast Ident, expr: &'ast Expr, block: &'ast Stmt) {
        walk_stmt_for(self, ident, expr, block)
    }

    fn visit_stmt_return(&mut self, expr: Option<&'ast Expr>) {
        walk_stmt_return(self, expr);
    }

    fn visit_stmt_empty(&mut self) {
        walk_stmt_empty(self);
    }

    fn visit_stmt_item(&mut self, item: &'ast Item) {
        walk_stmt_item(self, item);
    }

    fn visit_item(&mut self, item: &'ast Item) {
        walk_item(self, item);
    }
    fn visit_item_fun(&mut self, fun: &'ast Fun) {
        walk_item_fun(self, fun);
    }

    fn visit_item_class(&mut self, class: &'ast Class) {
        walk_item_class(self, class);
    }

    fn visit_item_interface(&mut self, interface: &'ast Interface) {
        walk_item_interface(self, interface);
    }
}

pub fn walk_stmt_item<'ast, V: Visitor<'ast>>(visitor: &mut V, item: &'ast Item) {
    visitor.visit_item(item);
}

pub fn walk_item<'ast, V: Visitor<'ast>>(visitor: &mut V, item: &'ast Item) {
    let Item {
        kind,
        span: _,
        vis: _,
        ident: _,
    } = item;
    match kind {
        ItemKind::Fun(fun) => visitor.visit_item_fun(fun),
        ItemKind::Class(class) => visitor.visit_item_class(class),
        ItemKind::Interface(interface) => visitor.visit_item_interface(interface),
        _ => {}
    }
}

pub fn walk_item_fun<'ast, V: Visitor<'ast>>(visitor: &mut V, fun: &'ast Fun) {
    let Fun {
        generics,
        sig,
        body,
    } = fun;

    for generic in generics {
        let GenericParam { bounds, .. } = generic;
        for bound in bounds {
            visitor.visit_ty(bound)
        }
    }

    let FunSig {
        inputs: (_self_param, fun_param),
        output,
        span: _,
    } = sig;

    for fun_param in fun_param {
        visitor.visit_ty(&fun_param.ty)
    }
    if let FunRetTy::Ty(ty) = output {
        visitor.visit_ty(ty)
    }

    match body {
        Some(body) => match &body.kind {
            StmtKind::Block(block) => visitor.visit_stmt_block(block),
            _ => {}
        },
        None => {}
    }
}

pub fn walk_item_class<'ast, V: Visitor<'ast>>(visitor: &mut V, class: &'ast Class) {
    todo!()
}

pub fn walk_item_interface<'ast, V: Visitor<'ast>>(visitor: &mut V, interface: &'ast Interface) {
    todo!()
}

pub fn walk_stmt<'ast, V: Visitor<'ast>>(visitor: &mut V, stmt: &'ast Stmt) {
    let Stmt { kind, span: _ } = stmt;
    match kind {
        StmtKind::Item(item) => {
            visitor.visit_stmt_item(item);
        }
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
        StmtKind::Return(expr) => visitor.visit_stmt_return(expr.as_deref()),
        StmtKind::Empty => {
            visitor.visit_stmt_empty();
        }
        _ => {}
    }
}

pub fn walk_stmt_return<'ast, V: Visitor<'ast>>(visitor: &mut V, expr: Option<&'ast Expr>) {
    if expr.is_some() {
        visitor.visit_expr(expr.unwrap());
    };
}
pub fn walk_stmt_while<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    condition: &'ast Expr,
    block: &'ast Stmt,
) {
    visitor.visit_expr(condition);
    visitor.visit_stmt(block);
}

pub fn walk_stmt_for<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    _ident: &'ast Ident,
    expr: &'ast Expr,
    block: &'ast Stmt,
) {
    visitor.visit_expr(expr);
    visitor.visit_stmt(block);
}

pub fn walk_stmt_block<'ast, V: Visitor<'ast>>(visitor: &mut V, stmts: &'ast Vec<Box<Stmt>>) {
    for stmt in stmts {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_stmt_if<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    condition: &'ast Expr,
    block: &'ast Stmt,
    optional_else: Option<&'ast Stmt>,
) {
    visitor.visit_expr(condition);
    visitor.visit_stmt(block);

    if let Some(else_block) = optional_else {
        visitor.visit_stmt(else_block);
    }
}

pub fn walk_stmt_expr<'ast, V: Visitor<'ast>>(visitor: &mut V, expr: &'ast Expr) {
    visitor.visit_expr(expr)
}

pub fn walk_stmt_empty<'ast, V: Visitor<'ast>>(_visitor: &mut V) {}

pub fn walk_stmt_var<'ast, V: Visitor<'ast>>(visitor: &mut V, local: &'ast Local) {
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
    };
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
    };
}

pub fn walk_generic_arg<'ast, V: Visitor<'ast>>(visitor: &mut V, arg: &'ast GenericArg) {
    match arg {
        GenericArg::Type(ty) => visitor.visit_ty(ty),
    };
}

pub fn walk_ty<'ast, V: Visitor<'ast>>(visitor: &mut V, ty: &'ast Ty) {
    match &ty.kind {
        TyKind::Path(path) => {
            visitor.visit_path(path);
        }
        TyKind::Never => {}
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
        ExprKind::Path(path) => {
            visitor.visit_path(path);
        }
        ExprKind::Cast(expr, ty) => {
            visitor.visit_expr(expr);
            visitor.visit_ty(ty);
        }
    }
}
