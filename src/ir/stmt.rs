use crate::ast::{Stmt, StmtKind};

use super::IrGenerator;

impl<'ctx> IrGenerator<'ctx> {
    pub fn generate_ir_stmts(&self, stmts: &Vec<Box<Stmt>>) {
        for stmt in stmts {
            self.generate_ir_stmt(stmt);
        }
    }

    pub fn generate_ir_stmt(&self, stmt: &Stmt) {
        let Stmt { span: _, kind } = stmt;
        match kind {
            //Stmt::Item(item) => self.generate_code_item(item),
            StmtKind::Expr(_expr) => {}
            StmtKind::Empty => {}
            _ => unimplemented!(),
        }
    }
}
