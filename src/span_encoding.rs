use crate::session_global::BytePos;

pub const DUMMY_SP: Span = Span {
    offset: 0,
    length: 0,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    /// The start of the span.
    pub offset: BytePos,
    /// The total length of the span
    pub length: usize,
}
