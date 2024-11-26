use crate::ast::{self, Expr};
use crate::ast::{Local, Stmt, StmtKind};

use super::{
    type_check::path::PathStyle,
    variable::{BindingMode, Mutability, Variable},
    SematicResolver, Ty, TyKind,
};
use super::{PrimTy, SResult};

impl SematicResolver {
    pub fn resolve_stmt(&mut self, stmt: &mut Box<Stmt>) -> SResult<()> {
        let Stmt { kind, .. } = stmt.as_mut();
        match kind {
            StmtKind::Block(block) => self.resolve_block(block),
            StmtKind::If(condition, then_block, else_block) => {
                self.resolve_stmt_if(condition, then_block, else_block)
            }
            StmtKind::Var(local) => self.resolve_local(local),
            StmtKind::Expr(expr) => self.resolve_stmt_expr(expr),
            StmtKind::Empty => Ok(()),
            _ => todo!(),
        }
    }

    fn resolve_stmt_expr(&mut self, expr: &mut Box<Expr>) -> SResult<()> {
        self.resolve_and_get_expr_ty(expr).map(|_| ())
    }

    fn resolve_stmt_if(
        &mut self,
        condition: &mut Box<Expr>,
        then_block: &mut Box<Stmt>,
        else_block: &mut Option<Box<Stmt>>,
    ) -> SResult<()> {
        let condition_ty = self.resolve_and_get_expr_ty(condition).unwrap();

        match condition_ty {
            TyKind::Prim(PrimTy::Bool) => {}
            _ => {
                return Err(format!(
                    "Type mismatch: expected bool, found {:?}",
                    condition_ty
                ))
            }
        }

        self.resolve_stmt(then_block)?;

        if let Some(else_block) = else_block {
            self.resolve_stmt(else_block)?;
        }

        Ok(())
    }

    fn resolve_local(&mut self, local: &mut Box<Local>) -> SResult<()> {
        let Local {
            binding_mode,
            ident,
            ty,
            kind: _,
            span,
        } = local.as_ref();

        let binding_mode = BindingMode(match binding_mode.0 {
            ast::Mutability::Mutable => Mutability::Mutable,
            ast::Mutability::Immutable => Mutability::Immutable,
        });

        // FIX: This is not the correct way to get the type of the variable.
        let ty_kind = match ty.kind {
            ast::TyKind::Path(ref path) => {
                self.check_and_get_path_ty(path, PathStyle::Type).unwrap()
            }
            ast::TyKind::Never => TyKind::Void,
        };

        let ty = Ty {
            kind: ty_kind.clone(),
        };

        let mut is_init = false;
        if let ast::LocalKind::Init(expr) = &local.kind {
            let expr_ty = self.resolve_and_get_expr_ty(expr).unwrap();
            if ty_kind != expr_ty {
                return Err(format!(
                    "Type mismatch: expected {:?}, found {:?}",
                    ty, expr_ty
                ));
            }
            is_init = true;
        }

        let variable = Variable {
            name: ident.name,
            ty,
            span: span.clone(),
            binding_mode,
            is_initialized: is_init,
        };

        let scope_id = self.insert_variable(variable);
        local.ident.scope_id = Some(scope_id);
        Ok(())
    }

    fn resolve_block(&mut self, stmts: &mut Vec<Box<ast::Stmt>>) -> SResult<()> {
        self.resolve_in_new_scope(|resolver| resolver.resolve(stmts))
    }
}
