use crate::ast::{Expr, Local, LocalKind, Stmt, StmtKind};

use super::IrRustGenerator;

impl<'ctx> IrRustGenerator<'ctx> {
    pub fn generate_stmt(&self, stmt: &Box<Stmt>) -> String {
        let Stmt { kind, .. } = stmt.as_ref();

        match kind {
            StmtKind::Var(local) => self.generate_stmt_var(&local),
            StmtKind::Expr(expr) => self.generate_stmt_expr(expr),
            _ => todo!(),
        }
    }

    fn generate_stmt_expr(&self, expr: &Box<Expr>) -> String {
        format!("{};\n", self.generate_expr(expr))
    }

    fn generate_stmt_var(&self, local: &Box<Local>) -> String {
        let Local { ident, kind, .. } = local.as_ref();

        let variable = self.resolve_ident(ident);
        let var_bind = variable.borrow();
        let binding_mode = var_bind.binding_mode.to_rust_bind_str();
        let var_name = var_bind.name.as_str();
        let var_type = var_bind.ty.to_rust_ty_str();

        match kind {
            LocalKind::Decl => {
                format!("let {} {}: {};\n", binding_mode, var_name, var_type)
            }
            LocalKind::Init(expr) => {
                let expr = self.generate_expr(expr);
                format!(
                    "let {} {}: {} = {};\n",
                    binding_mode, var_name, var_type, expr
                )
            }
        }
    }
}
