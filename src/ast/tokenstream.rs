use std::fmt::Display;
use std::rc::Rc;

use crate::span_encoding::Span;

use super::{Delimiter, Token};

pub type TokenStream = Rc<Vec<TokenTree>>;

/// Part of a `TokenStream`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenTree {
    /// A single token. Should never be `OpenDelim` or `CloseDelim`, because
    /// delimiters are implicitly represented by `Delimited`.
    Token(Token, Spacing),
    // A delimited sequence of token trees.
    Delimited(DelimSpan, Delimiter, TokenStream),
}

impl Display for TokenTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Token(..) => println!("{self}"),
            Self::Delimited(delim_span, delimiter, stream) => {
                println!("Delim span: {delim_span:?}");
                println!("Delimiter: {delimiter:?}");
                println!("Stream: {stream:?}");
            }
        }
        todo!()
    }
}

/// Indicates whether a token can join with the following token to form a
/// compound token.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Spacing {
    /// The token cannot join with the following token to form a compound
    /// token.
    ///
    /// In token streams parsed from source code, the compiler will use `Alone`
    /// for any token immediately followed by whitespace, a non-doc comment, an identifier,
    /// literal, delimiter, doc comment or EOF.
    Alone,

    /// The token can join with the following token to form a compound token.
    ///
    /// In token streams parsed from source code, the compiler will use `Joint`
    /// for any token immediately followed by punctuation (as determined by
    /// `Token::is_punct`).
    Joint,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct DelimSpan {
    pub open: Span,
    pub close: Span,
}

impl DelimSpan {
    pub fn from_pair(open: Span, close: Span) -> Self {
        DelimSpan { open, close }
    }
}
