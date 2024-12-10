use crate::{
    interpreter::{ident::Ident, Ty, Value},
    span_encoding::Span,
};

#[derive(Debug, Clone)]
pub struct Variable {
    pub ident: Ident,
    pub is_mut: bool,
    pub val: Option<Value>,
    pub ty: Ty,
    pub first_assigned_span: Option<Span>,
}

impl Variable {
    pub fn can_be_assigned(&self) -> bool {
        self.is_mut || self.val.is_none()
    }
}
