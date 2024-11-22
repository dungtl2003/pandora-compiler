use crate::{
    ast::{Ident, Path},
    ir::{ty::TyKind, SResult, SematicResolver},
};

pub enum PathStyle {
    Type,
    Variable,
}

impl<'ast> SematicResolver<'ast> {
    // FIX: Currently, this function does not care about the path, only the last segment.
    pub fn get_path_ty(&mut self, path: &Path, style: PathStyle) -> SResult<TyKind> {
        let Path { segments, .. } = path;
        let last_segment = segments.last().unwrap();

        match style {
            PathStyle::Type => {
                let Ident { span, name } = last_segment.ident;
                self.lookup_type(name)
                    .map(|ty| ty.kind)
                    .ok_or_else(|| format!("Type '{}' not found in scope at {}", name, span))
            }
            PathStyle::Variable => {
                let Ident { span, name } = last_segment.ident;
                self.lookup_variable(name, span)
                    .map(|var| var.ty.kind.clone())
                    .ok_or_else(|| format!("Variable '{}' not found in scope at {}", name, span))
            }
        }
    }
}
