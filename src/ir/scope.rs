use crate::{span_encoding::Span, symbol::Symbol};

use super::{
    item::Item,
    table::Table,
    ty::{PrimTy, Ty, TyKind},
    variable::Variable,
};

#[derive(Debug)]
pub struct SematicScope {
    pub context: SematicContext,
    pub depth: usize,
}

#[derive(Debug)]
pub struct SematicContext {
    pub items: Table<Item>,
    pub variables: Table<Variable>,
}

impl SematicScope {
    pub fn new(depth: usize) -> Self {
        Self {
            context: SematicContext {
                items: Table::new(),
                variables: Table::new(),
            },
            depth,
        }
    }

    pub fn insert_variable(&mut self, variable: Variable) {
        self.context.variables.push(variable);
    }

    pub fn lookup_variable(&self, name: Symbol, span: Span) -> Option<Variable> {
        let variables = self
            .context
            .variables
            .iter()
            .filter(|var| var.name == name)
            .rev(); // Reverse to get the most recent variable (shadowing)

        for var in variables {
            if span.is_after(var.span) {
                return Some(var.clone());
            }
        }

        None
    }

    // FIX: Currently, this function does not care about the path. It also handles only primitive
    // types.
    pub fn lookup_type(&self, name: Symbol) -> Option<Ty> {
        if let Some(kind) = PrimTy::from_str_ty(name.as_str()) {
            Some(Ty {
                kind: TyKind::Prim(kind),
            })
        } else {
            None
        }
    }
}
