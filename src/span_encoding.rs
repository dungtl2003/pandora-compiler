use core::fmt;
use std::fmt::{Display, Formatter};

use miette::SourceSpan;

use crate::session::BytePos;

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

impl Span {
    /// Creates a new span from a start (inclusive) and end (exclusive) position.
    pub fn new(start: BytePos, end: BytePos) -> Self {
        Span {
            offset: start,
            length: (end - start) as usize,
        }
    }

    pub fn after(span: Span) -> Self {
        Span {
            offset: span.end(),
            length: 1,
        }
    }

    pub fn to_source_span(&self) -> SourceSpan {
        (self.offset as usize, self.length).into()
    }

    /// Returns the end of the span (exclusive).
    pub fn end(&self) -> BytePos {
        self.offset + self.length as u32
    }

    pub fn to(&self, other: Span) -> Span {
        Span {
            offset: self.offset,
            length: (other.end() - self.offset) as usize,
        }
    }
}

impl Display for Span {
    /// Formats the span as `[offset (inclusive), end (exclusive)]`.
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "[{}, {}]", self.offset, self.end())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

pub fn respan<T>(node: T, span: Span) -> Spanned<T> {
    Spanned { node, span }
}
