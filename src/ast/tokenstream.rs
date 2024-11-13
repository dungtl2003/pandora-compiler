use std::rc::Rc;

use crate::span_encoding::Span;

use super::{Delimiter, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenStream(Rc<Vec<TokenTree>>);

impl TokenStream {
    pub fn new(buf: Vec<TokenTree>) -> Self {
        TokenStream(Rc::new(buf))
    }
    pub fn into_trees(self) -> TokenTreeCursor {
        TokenTreeCursor::new(self)
    }
}

pub fn pprint(stream: &TokenStream) {
    print_recursive(stream, 0);
}

fn print_recursive(stream: &TokenStream, depth: u32) {
    let spaces = " ".repeat((depth * 4) as usize);
    for tt in stream.0.iter() {
        match tt {
            TokenTree::Token(tok, _) => {
                println!(
                    "{spaces}{:?} [{} - {}]",
                    tok.kind,
                    tok.span.offset,
                    tok.span.offset + tok.span.length as u32 - 1
                );
            }
            TokenTree::Delimited(delim_span, delimiter, s) => {
                println!(
                    "{spaces}Delimited, type: {delimiter:?}, [{} - {}]",
                    delim_span.open.offset, delim_span.close.offset
                );
                print_recursive(s, depth + 1);
            }
        }
    }
}

/// Part of a `TokenStream`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenTree {
    /// A single token. Should never be `OpenDelim` or `CloseDelim`, because
    /// delimiters are implicitly represented by `Delimited`.
    Token(Token, Spacing),
    // A delimited sequence of token trees.
    Delimited(DelimSpan, Delimiter, TokenStream),
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

/// Owning by-value iterator over a [`TokenStream`], that produces `&TokenTree`
/// items.
#[derive(Clone, Debug)]
pub struct TokenTreeCursor {
    pub stream: TokenStream,
    index: usize,
}

impl TokenTreeCursor {
    fn new(stream: TokenStream) -> Self {
        TokenTreeCursor { stream, index: 0 }
    }

    #[inline]
    pub fn next_ref(&mut self) -> Option<&TokenTree> {
        self.stream.0.get(self.index).map(|tree| {
            self.index += 1;
            tree
        })
    }

    pub fn look_ahead(&self, n: usize) -> Option<&TokenTree> {
        self.stream.0.get(self.index + n)
    }
}
