use crate::ast::{Path, Ty, TyKind};

use super::IrRustGenerator;

impl<'ctx> IrRustGenerator<'ctx> {
    pub fn generate_ty(&self, ty: &Box<Ty>) -> String {
        let Ty { kind, .. } = ty.as_ref();

        match kind {
            TyKind::Path(path) => self.generate_ty_path(path),
            _ => todo!(),
        }
    }

    fn generate_ty_path(&self, path: &Box<Path>) -> String {
        self.generate_path(path)
    }
}
