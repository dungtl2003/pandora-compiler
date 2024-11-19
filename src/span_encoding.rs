use core::fmt;
use std::fmt::{Display, Formatter};

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

impl Span {
    /// Creates a new span from a start (inclusive) and end (exclusive) position.
    pub fn new(start: BytePos, end: BytePos) -> Self {
        Span {
            offset: start,
            length: (end - start) as usize,
        }
    }
    pub fn dummy() -> Self {
        DUMMY_SP
    }

    pub fn with_offset(self, offset: BytePos) -> Self {
        Span {
            offset,
            length: self.length,
        }
    }

    pub fn from_offset(offset: BytePos, length: usize) -> Self {
        Span { offset, length }
    }

    /// Returns the end of the span (exclusive).
    pub fn end(&self) -> BytePos {
        self.offset + self.length as u32
    }

    pub fn is_dummy(&self) -> bool {
        *self == DUMMY_SP
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
