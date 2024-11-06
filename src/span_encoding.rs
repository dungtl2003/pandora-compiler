use crate::session_global::BytePos;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    /// The start of the span.
    pub offset: BytePos,
    /// The total length of the span
    pub length: usize,
}
