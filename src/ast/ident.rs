use crate::{kw, span_encoding::Span};
use symbol::Symbol;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    pub fn new(name: Symbol, span: Span) -> Self {
        Ident { name, span }
    }

    pub fn is_keyword(self) -> bool {
        kw::is_keyword(&self.name)
    }
}
