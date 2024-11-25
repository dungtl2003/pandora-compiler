use crate::{span_encoding::Span, symbol::Symbol};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ident {
    pub scope_id: Option<String>,
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    /// A keyword or reserved identifier that can be used as a path segment.
    pub fn is_path_segment_keyword(&self) -> bool {
        self.name.is_path_segment_keyword()
    }
}
