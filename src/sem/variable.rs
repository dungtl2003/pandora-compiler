use core::fmt;
use std::fmt::{Display, Formatter};

use crate::{span_encoding::Span, symbol::Symbol};

use super::Ty;

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Symbol,
    pub ty: Ty,
    pub span: Span,
    pub binding_mode: BindingMode,
}

#[derive(Debug, Clone)]
pub struct BindingMode(pub Mutability);

impl Display for BindingMode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum Mutability {
    Immutable,
    Mutable,
}

impl Display for Mutability {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Mutability::Immutable => write!(f, "immutable"),
            Mutability::Mutable => write!(f, "mutable"),
        }
    }
}

impl BindingMode {
    pub fn prefix_str(self) -> &'static str {
        match self.0 {
            Mutability::Immutable => "",
            Mutability::Mutable => "mut ",
        }
    }
}
