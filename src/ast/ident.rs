use crate::{span_encoding::Span, symbol::Symbol};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    pub fn is_keyword(&self) -> bool {
        self.name.is_keyword()
    }
}
