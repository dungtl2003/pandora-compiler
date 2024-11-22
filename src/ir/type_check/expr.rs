use crate::ast::{Expr, ExprKind, Lit, LitKind, UnOp};

use super::{
    super::{
        ty::{PrimTy, TyKind},
        SResult, SematicResolver,
    },
    path::PathStyle,
};

impl<'ast> SematicResolver<'ast> {
    // TODO: Support computation for more types, not just primitive types
    pub fn get_expr_ty(&mut self, expr: &Box<Expr>) -> SResult<TyKind> {
        let Expr { kind, .. } = expr.as_ref();
        match kind {
            ExprKind::Binary(_, lhs, rhs) => self.get_binary_ty(lhs, rhs),
            ExprKind::Unary(op, expr) => self.get_unary_ty(op, expr),
            ExprKind::Literal(lit) => self.get_literal_ty(lit),
            ExprKind::Assign(..) | ExprKind::AssignOp(..) => Ok(TyKind::Void),
            ExprKind::Path(path) => self.get_path_ty(path.as_ref(), PathStyle::Variable),
            _ => todo!(),
        }
    }

    fn get_unary_ty(&mut self, op: &UnOp, expr: &Box<Expr>) -> SResult<TyKind> {
        let expr_ty = self.get_expr_ty(expr);
        match op {
            UnOp::Ne => match expr_ty {
                Ok(TyKind::Prim(PrimTy::Int(_))) => Ok(TyKind::Prim(PrimTy::Int(None))),
                Ok(TyKind::Prim(PrimTy::Float(_))) => Ok(TyKind::Prim(PrimTy::Float(None))),
                _ => Err(format!(
                    "Unary operation - only supports int and float, not {:?}",
                    expr_ty
                )),
            },
            UnOp::Not => match expr_ty {
                Ok(TyKind::Prim(PrimTy::Bool(_))) => Ok(TyKind::Prim(PrimTy::Bool(None))),
                _ => Err(format!(
                    "Unary operation ! only supports bool, not {:?}",
                    expr_ty
                )),
            },
        }
    }

    fn get_binary_ty(&mut self, lhs: &Box<Expr>, rhs: &Box<Expr>) -> SResult<TyKind> {
        let lhs_ty = self.get_expr_ty(lhs)?;
        let rhs_ty = self.get_expr_ty(rhs)?;

        if lhs_ty != rhs_ty {
            return Err(format!(
                "Binary operation between different types: {:?} and {:?}",
                lhs_ty, rhs_ty
            ));
        }

        // TODO: Implement more complex type checking
        if !matches!(lhs_ty, TyKind::Prim(_)) {
            return Err(format!(
                "Binary operation currently only supports primitive types, not {:?}",
                lhs_ty
            ));
        }

        let prim_ty = match lhs_ty.clone() {
            TyKind::Prim(prim_ty) => prim_ty,
            _ => unreachable!(),
        };

        if !matches!(prim_ty, PrimTy::Int(_) | PrimTy::Float(_)) {
            return Err(format!(
                "Binary operation currently only supports int and float, not {:?}",
                prim_ty
            ));
        }

        Ok(lhs_ty)
    }

    fn get_literal_ty(&mut self, lit: &Lit) -> SResult<TyKind> {
        let Lit { kind, symbol: _ } = lit;
        let ty_kind = match kind {
            LitKind::Int => TyKind::Prim(PrimTy::Int(None)),
            LitKind::Float => TyKind::Prim(PrimTy::Float(None)),
            LitKind::Bool => TyKind::Prim(PrimTy::Bool(None)),
            _ => unimplemented!(),
        };
        Ok(ty_kind)
    }
}
