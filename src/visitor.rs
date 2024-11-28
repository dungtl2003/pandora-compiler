use crate::ast::{
    Expr, ExprKind, Fun, FunSig, Ident, Local, LocalKind, Path, PathSegment, Stmt, StmtKind, Ty,
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

    fn visit_ty(&mut self, ty: &'ast Ty) {
        walk_ty(self, ty)
    }

    fn visit_stmt_block(&mut self, stmts: &'ast Vec<Box<Stmt>>) {
        walk_stmt_block(self, stmts)
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        walk_stmt(self, stmt)
    }

    fn visit_stmt_func_decl(&mut self, fun: &'ast Fun) {
        walk_stmt_func_decl(self, fun)
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

    fn visit_stmt_import(&mut self, path: &'ast Path) {
        walk_stmt_import(self, path);
    }
}

pub fn walk_stmt<'ast, V: Visitor<'ast>>(visitor: &mut V, stmt: &'ast Stmt) {
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
        StmtKind::Return(expr) => visitor.visit_stmt_return(expr.as_deref()),
        StmtKind::Empty => {
            visitor.visit_stmt_empty();
        }
        StmtKind::FuncDecl(fun) => {
            visitor.visit_stmt_func_decl(fun);
        }
        StmtKind::Break => {}
        StmtKind::Continue => {}
        StmtKind::Import(path) => {
            visitor.visit_stmt_import(path);
        }
    }
}

pub fn walk_stmt_import<'ast, V: Visitor<'ast>>(visitor: &mut V, path: &'ast Path) {
    visitor.visit_path(path);
}

pub fn walk_stmt_func_decl<'ast, V: Visitor<'ast>>(visitor: &mut V, fun: &'ast Fun) {
    let Fun { sig, body } = fun;
    let FunSig {
        name: _,
        inputs,
        output,
        span: _,
    } = sig;

    for input in inputs {
        visitor.visit_ty(&input.ty);
    }

    if let Some(output) = output {
        visitor.visit_ty(output);
    }

    visitor.visit_stmt(body);
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

pub fn walk_path_segment<'ast, V: Visitor<'ast>>(_visitor: &mut V, _segment: &'ast PathSegment) {}

pub fn walk_ty<'ast, V: Visitor<'ast>>(_visitor: &mut V, _ty: &'ast Ty) {}

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
        ExprKind::Identifier(_ident) => {}
        ExprKind::Cast(expr, ty) => {
            visitor.visit_expr(expr);
            visitor.visit_ty(ty);
        }
        ExprKind::FunCall(fun, args) => {
            visitor.visit_expr(fun);
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
    }
}
