use core::fmt;
use std::fmt::{Display, Formatter};

use crate::span_encoding::Span;

use super::ty::Ty;

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub ty: Ty,
    pub span: Span,
    pub binding_mode: BindingMode,
    pub is_initialized: bool,
}

impl Variable {
    pub fn can_be_assigned(&self) -> bool {
        self.binding_mode.0 == Mutability::Mutable || !self.is_initialized
    }
}

#[derive(Debug, Clone)]
pub struct BindingMode(pub Mutability);

impl BindingMode {
    pub fn to_rust_bind_str(&self) -> String {
        match self.0 {
            Mutability::Immutable => "".to_string(),
            Mutability::Mutable => "mut".to_string(),
        }
    }
}

impl Display for BindingMode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
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
