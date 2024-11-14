use crate::{
    ast::{DelimSpan, Delimiter, Spacing, Token, TokenKind, TokenStream, TokenTree},
    span_encoding::Span,
};

use super::StringReader;

pub struct TokenTreesReader<'src> {
    string_reader: StringReader<'src>,
    /// The "next" token, which has been obtained from the `StringReader` but
    /// not yet handled by the `TokenTreesReader`.
    token: Token,

    /// Stack of open delimiters and their spans. Used for error message and recovery.
    open_delims: Vec<(Delimiter, Span)>,
}

impl<'src> TokenTreesReader<'src> {
    pub fn lex_all_token_trees(string_reader: StringReader<'src>) -> TokenStream {
        let mut tt_reader = TokenTreesReader {
            string_reader,
            token: Token::dummy(),
            open_delims: Vec::new(),
        };

        tt_reader.lex_token_trees(false).0
    }

    // Lex into a token stream. The `Spacing` in the result is that of the
    // opening delimiter. Return `true` if not encounter any errors, return `false` otherwise.
    fn lex_token_trees(&mut self, is_delimited: bool) -> (TokenStream, bool) {
        // This can be the first token or open delim, can not glue.
        self.eat(false);

        let mut buf = Vec::new();
        loop {
            match self.token.kind {
                TokenKind::OpenDelim(delim) => match self.lex_token_tree_open_delim(delim) {
                    Some(val) => buf.push(val),
                    None => return (TokenStream::new(buf), false),
                },
                TokenKind::CloseDelim(_delim) => {
                    let mut no_err = true;
                    if !is_delimited {
                        no_err = false;
                        self.report_close_delim_error();
                    }
                    return (TokenStream::new(buf), no_err);
                }
                TokenKind::Eof => {
                    let mut no_err = true;
                    if is_delimited {
                        no_err = false;
                        self.report_eof_error();
                    }
                    return (TokenStream::new(buf), no_err);
                }
                _ => {
                    // Get the next normal token.
                    // We will have the previous token, so we can try to glue.
                    let (this_tok, this_spacing) = self.eat(true);
                    buf.push(TokenTree::Token(this_tok, this_spacing));
                }
            }
        }
    }

    fn lex_token_tree_open_delim(&mut self, open_delim: Delimiter) -> Option<TokenTree> {
        // The span for beginning of the delimited section.
        let pre_span = self.token.span;

        self.open_delims.push((open_delim, self.token.span));

        // Lex the token trees within the delimiters.
        // We stop at any delimiter so we can try to recover if the user
        // uses an incorrect delimiter.
        let (tts, no_err) = self.lex_token_trees(/* is_delimited */ true);
        if !no_err {
            return None;
        }

        // Expand to cover the entire delimited token tree.
        let delim_span = DelimSpan::from_pair(pre_span, self.token.span);

        match self.token.kind {
            // Correct delimiter.
            TokenKind::CloseDelim(close_delim) if close_delim == open_delim => {
                self.open_delims.pop();

                // Move past the closing delimiter (ofcourse no glue here).
                self.eat(false);
            }

            // Incorrect delimiter.
            TokenKind::CloseDelim(close_delim) => {
                // The top delim will not have any matched close delim for it, so throw it away.
                self.open_delims.pop();
                // If the incorrect delimiter matches an earlier opening
                // delimiter, then don't consume it (it can be used to
                // close the earlier one). Otherwise, consume it.
                // E.g., we try to recover from:
                // fn foo() {
                //     bar(baz(
                // }  // Incorrect delimiter but matches the earlier `{`
                if !self.open_delims.iter().any(|&(b, _)| b == close_delim) {
                    self.eat(false);
                }
            }
            TokenKind::Eof => {
                // Silently recover, the EOF token will be seen again
                // and an error emitted then. Thus we don't pop from
                // self.open_delims here.
            }
            _ => unreachable!(),
        }

        Some(TokenTree::Delimited(delim_span, open_delim, tts))
    }

    // Move on to the next token, returning the current token and its spacing.
    // Will glue adjacent single-char tokens together if `glue` is set.
    fn eat(&mut self, glue: bool) -> (Token, Spacing) {
        let (this_spacing, next_tok) = loop {
            let next_tok = self.string_reader.next_token();

            let maybe_glued = self.token.glue(&next_tok);
            if glue && maybe_glued.is_some() {
                self.token = maybe_glued.unwrap();
            } else {
                let this_spacing = if next_tok.is_punct() {
                    Spacing::Joint
                } else {
                    Spacing::Alone
                };
                break (this_spacing, next_tok);
            }
        };
        let this_tok = std::mem::replace(&mut self.token, next_tok);
        (this_tok, this_spacing)
    }

    fn report_close_delim_error(&self) {
        unimplemented!();
    }

    fn report_eof_error(&self) {
        unimplemented!();
    }
}
