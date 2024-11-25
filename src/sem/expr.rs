use super::SematicResolver;
use crate::ast::{self, Expr, ExprKind};

impl SematicResolver {
    pub fn resolve_expr(&mut self, expr: &Box<Expr>) {
        let Expr { kind, .. } = expr.as_ref();
        match kind {
            ExprKind::Literal(lit) => {}
            _ => todo!(),
        }
    }
}
