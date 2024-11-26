use crate::{
    ast::{BinOpKind, Expr, ExprKind, Lit, LitKind, UnOp},
    sem::{PrimTy, TyKind},
};

use super::{
    super::{SResult, SematicResolver},
    path::PathStyle,
};

impl SematicResolver {
    // TODO: Support computation for more types, not just primitive types
    /// Check and get the type of an expression
    pub fn resolve_and_get_expr_ty(&mut self, expr: &Box<Expr>) -> SResult<TyKind> {
        let Expr { kind, .. } = expr.as_ref();
        match kind {
            ExprKind::Binary(op, lhs, rhs) => self.resolve_and_get_binary_ty(&op.node, lhs, rhs),
            ExprKind::Unary(op, expr) => self.resolve_and_get_unary_ty(op, expr),
            ExprKind::Literal(lit) => self.resolve_and_get_literal_ty(lit),
            ExprKind::Assign(lhs, rhs, _) | ExprKind::AssignOp(_, lhs, rhs) => {
                self.resolve_and_get_assign_ty(lhs, rhs)
            }
            ExprKind::Path(path) => self.check_and_get_path_ty(path.as_ref(), PathStyle::Variable),
            _ => todo!(),
        }
    }

    fn resolve_and_get_unary_ty(&mut self, op: &UnOp, expr: &Box<Expr>) -> SResult<TyKind> {
        let expr_ty = self.resolve_and_get_expr_ty(expr);
        match op {
            UnOp::Ne => match expr_ty {
                Ok(TyKind::Prim(PrimTy::Int)) => Ok(TyKind::Prim(PrimTy::Int)),
                Ok(TyKind::Prim(PrimTy::Float)) => Ok(TyKind::Prim(PrimTy::Float)),
                _ => Err(format!(
                    "Unary operation - only supports int and float, not {:?}",
                    expr_ty
                )),
            },
            UnOp::Not => match expr_ty {
                Ok(TyKind::Prim(PrimTy::Bool)) => Ok(TyKind::Prim(PrimTy::Bool)),
                _ => Err(format!(
                    "Unary operation ! only supports bool, not {:?}",
                    expr_ty
                )),
            },
        }
    }

    fn resolve_and_get_assign_ty(&mut self, lhs: &Box<Expr>, rhs: &Box<Expr>) -> SResult<TyKind> {
        let lhs_ty = self.resolve_and_get_expr_ty(lhs)?;
        let rhs_ty = self.resolve_and_get_expr_ty(rhs)?;

        if lhs_ty != rhs_ty {
            return Err(format!(
                "Assignment between different types: {:?} and {:?}",
                lhs_ty, rhs_ty
            ));
        }

        // After checking the types, we can resolve the left-hand side of the assignment (There
        // shouldn't be a case when the variable is not found)
        let left_var = match &lhs.as_ref().kind {
            ExprKind::Path(path) => self.resolve_variable_from_path(&path),
            _ => return Err("Left-hand side of assignment must be a variable".to_string()),
        }
        .unwrap();

        if !left_var.borrow().can_be_assigned() {
            return Err("Cannot assign to immutable variable".to_string());
        }

        left_var.borrow_mut().is_initialized = true;
        Ok(TyKind::Void)
    }

    fn resolve_and_get_binary_ty(
        &mut self,
        op: &BinOpKind,
        lhs: &Box<Expr>,
        rhs: &Box<Expr>,
    ) -> SResult<TyKind> {
        let lhs_ty = self.resolve_and_get_expr_ty(lhs)?;
        let rhs_ty = self.resolve_and_get_expr_ty(rhs)?;

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
            BinOpKind::And | BinOpKind::Or => match prim_ty {
                PrimTy::Bool => Ok(TyKind::Prim(prim_ty)),
                _ => {
                    return Err(format!(
                        "Logical operation currently only supports bool, not {:?}",
                        prim_ty
                    ));
                }
            },
            BinOpKind::Eq
            | BinOpKind::Ne
            | BinOpKind::Lt
            | BinOpKind::Le
            | BinOpKind::Gt
            | BinOpKind::Ge => match prim_ty {
                PrimTy::Int | PrimTy::Float => Ok(TyKind::Prim(PrimTy::Bool)),
                _ => {
                    return Err(format!(
                        "Comparation operation currently only supports int and float, not {:?}",
                        prim_ty
                    ));
                }
            },
            BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod => {
                match prim_ty {
                    PrimTy::Int | PrimTy::Float => Ok(TyKind::Prim(prim_ty)),
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
                PrimTy::Int => Ok(TyKind::Prim(prim_ty)),
                _ => {
                    return Err(format!(
                        "Bitwise operation currently only supports int, not {:?}",
                        prim_ty
                    ));
                }
            },
        }
    }

    fn resolve_and_get_literal_ty(&mut self, lit: &Lit) -> SResult<TyKind> {
        let Lit { kind, symbol: _ } = lit;
        let ty_kind = match kind {
            LitKind::Int => TyKind::Prim(PrimTy::Int),
            LitKind::Float => TyKind::Prim(PrimTy::Float),
            LitKind::Bool => TyKind::Prim(PrimTy::Bool),
            _ => unimplemented!(),
        };
        Ok(ty_kind)
    }
}
