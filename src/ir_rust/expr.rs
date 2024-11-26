use crate::ast::{BinOp, Expr, ExprKind, Lit, Path, Ty, UnOp};

use super::IrRustGenerator;

impl<'ctx> IrRustGenerator<'ctx> {
    pub fn generate_expr(&self, expr: &Box<Expr>) -> String {
        let Expr { kind, .. } = expr.as_ref();
        println!("{:?}", kind);
        match kind {
            ExprKind::Binary(op, left, right) => self.generate_expr_binary(op, left, right),
            ExprKind::Unary(op, expr) => self.generate_expr_unary(op, expr),
            ExprKind::Assign(lhs, rhs, _) => self.generate_expr_assign(lhs, rhs),
            ExprKind::AssignOp(op, lhs, rhs) => self.generate_expr_assign_op(op, lhs, rhs),
            ExprKind::Literal(lit) => self.generate_expr_literal(lit),
            ExprKind::Path(path) => self.generate_expr_path(path),
            ExprKind::Cast(expr, ty) => self.generate_expr_cast(expr, ty),
        }
    }

    fn generate_expr_cast(&self, expr: &Box<Expr>, ty: &Box<Ty>) -> String {
        let expr = self.generate_expr(expr);
        let ty = self.generate_ty(ty);
        format!("{} as {}", expr, ty)
    }

    fn generate_expr_path(&self, path: &Box<Path>) -> String {
        self.generate_path(path)
    }

    fn generate_expr_literal(&self, lit: &Lit) -> String {
        lit.to_rust_lit_str()
    }

    fn generate_expr_assign_op(&self, op: &BinOp, lhs: &Box<Expr>, rhs: &Box<Expr>) -> String {
        let lhs = self.generate_expr(lhs);
        let rhs = self.generate_expr(rhs);
        format!("{} {}= {}", lhs, op, rhs)
    }

    fn generate_expr_assign(&self, lhs: &Box<Expr>, rhs: &Box<Expr>) -> String {
        let lhs = self.generate_expr(lhs);
        let rhs = self.generate_expr(rhs);
        format!("{} = {}", lhs, rhs)
    }

    fn generate_expr_binary(&self, op: &BinOp, left: &Box<Expr>, right: &Box<Expr>) -> String {
        let left = self.generate_expr(left);
        let right = self.generate_expr(right);
        format!("{} {} {}", left, op, right)
    }

    fn generate_expr_unary(&self, op: &UnOp, expr: &Box<Expr>) -> String {
        let op = op.to_rust_op_str();
        let expr = self.generate_expr(expr);
        format!("{}{}", op, expr)
    }
}
