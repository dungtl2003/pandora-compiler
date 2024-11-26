use crate::{
    ast::{Ident, Path},
    sem::{SResult, SematicResolver, TyKind},
};

pub enum PathStyle {
    Type,
    Variable,
}

impl SematicResolver {
    // FIX: Currently, this function does not care about the path, only the last segment.
    pub fn check_and_get_path_ty(&self, path: &Path, style: PathStyle) -> SResult<TyKind> {
        let Path { segments, .. } = path;
        let last_segment = segments.last().unwrap();

        match style {
            PathStyle::Type => {
                let Ident {
                    span,
                    name,
                    scope_id,
                } = last_segment.ident.clone();
                self.lookup_type(name, scope_id)
                    .map(|ty| ty.kind)
                    .ok_or_else(|| format!("Type '{}' not found in scope at {}", name, span))
            }
            PathStyle::Variable => {
                let Ident {
                    span,
                    name,
                    scope_id: _,
                } = last_segment.ident.clone();
                self.resolve_variable(name, span)
                    .map(|var| var.borrow().ty.kind.clone())
                    .ok_or_else(|| format!("Variable '{}' not found in scope at {}", name, span))
            }
        }
    }
}
