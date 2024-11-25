use crate::{
    ast::{BinOpKind, Expr, ExprKind, Lit, LitKind, UnOp},
    sem::{PrimTy, TyKind},
    span_encoding::Spanned,
};

use super::{
    super::{SResult, SematicResolver},
    path::PathStyle,
};

impl SematicResolver {
    // TODO: Support computation for more types, not just primitive types
    /// Check and get the type of an expression
    pub fn check_and_get_expr_ty(&mut self, expr: &Box<Expr>) -> SResult<TyKind> {
        let Expr { kind, .. } = expr.as_ref();
        match kind {
            ExprKind::Binary(op, lhs, rhs) => self.check_and_get_binary_ty(&op.node, lhs, rhs),
            ExprKind::Unary(op, expr) => self.check_and_get_unary_ty(op, expr),
            ExprKind::Literal(lit) => self.check_and_get_literal_ty(lit),
            ExprKind::Assign(lhs, rhs, _) | ExprKind::AssignOp(_, lhs, rhs) => {
                self.check_and_get_assign_ty(lhs, rhs)
            }
            ExprKind::Path(path) => self.check_and_get_path_ty(path.as_ref(), PathStyle::Variable),
            _ => todo!(),
        }
    }

    fn check_and_get_unary_ty(&mut self, op: &UnOp, expr: &Box<Expr>) -> SResult<TyKind> {
        let expr_ty = self.check_and_get_expr_ty(expr);
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

    fn check_and_get_assign_ty(&mut self, lhs: &Box<Expr>, rhs: &Box<Expr>) -> SResult<TyKind> {
        let lhs_ty = self.check_and_get_expr_ty(lhs)?;
        let rhs_ty = self.check_and_get_expr_ty(rhs)?;

        if lhs_ty != rhs_ty {
            return Err(format!(
                "Assignment between different types: {:?} and {:?}",
                lhs_ty, rhs_ty
            ));
        }

        Ok(TyKind::Void)
    }

    fn check_and_get_binary_ty(
        &mut self,
        op: &BinOpKind,
        lhs: &Box<Expr>,
        rhs: &Box<Expr>,
    ) -> SResult<TyKind> {
        let lhs_ty = self.check_and_get_expr_ty(lhs)?;
        let rhs_ty = self.check_and_get_expr_ty(rhs)?;

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

        match op {
            BinOpKind::Eq
            | BinOpKind::Ne
            | BinOpKind::Lt
            | BinOpKind::Le
            | BinOpKind::Gt
            | BinOpKind::Ge
            | BinOpKind::And
            | BinOpKind::Or => match prim_ty {
                PrimTy::Int(_) | PrimTy::Float(_) => Ok(TyKind::Prim(PrimTy::Bool(None))),
                _ => {
                    return Err(format!(
                        "Comparation operation currently only supports int and float, not {:?}",
                        prim_ty
                    ));
                }
            },
            BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod => {
                match prim_ty {
                    PrimTy::Int(_) | PrimTy::Float(_) => Ok(TyKind::Prim(prim_ty)),
                    _ => {
                        return Err(format!(
                            "Binary operation currently only supports int and float, not {:?}",
                            prim_ty
                        ));
                    }
                }
            }

            BinOpKind::BitAnd
            | BinOpKind::BitOr
            | BinOpKind::BitXor
            | BinOpKind::Shl
            | BinOpKind::Shr => match prim_ty {
                PrimTy::Int(_) => Ok(TyKind::Prim(prim_ty)),
                _ => {
                    return Err(format!(
                        "Bitwise operation currently only supports int, not {:?}",
                        prim_ty
                    ));
                }
            },
        }
    }

    fn check_and_get_literal_ty(&mut self, lit: &Lit) -> SResult<TyKind> {
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
