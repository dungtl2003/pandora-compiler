use crate::{
    ast::{DelimSpan, Delimiter, Spacing, Token, TokenKind, TokenStream, TokenTree},
    parse::PResult,
    span_encoding::Span,
};

use super::StringReader;

pub struct TokenTreesReader<'sess, 'src> {
    string_reader: StringReader<'sess, 'src>,
    /// The "next" token, which has been obtained from the `StringReader` but
    /// not yet handled by the `TokenTreesReader`.
    token: Token,

    /// Stack of open delimiters and their spans. Used for error message and recovery.
    open_delims: Vec<(Delimiter, Span)>,
}

impl<'sess, 'src> TokenTreesReader<'sess, 'src> {
    pub fn lex_all_token_trees(
        string_reader: StringReader<'sess, 'src>,
    ) -> (TokenStream, PResult<()>) {
        let mut tt_reader = TokenTreesReader {
            string_reader,
            token: Token::dummy(),
            open_delims: Vec::new(),
        };

        tt_reader.lex_token_trees(false)
    }

    // Lex into a token stream. The `Spacing` in the result is that of the
    // opening delimiter.
    fn lex_token_trees(&mut self, is_delimited: bool) -> (TokenStream, PResult<()>) {
        // This can be the first token or open delim, can not glue.
        self.eat(false);

        let mut buf = Vec::new();
        loop {
            match self.token.kind {
                TokenKind::OpenDelim(delim) => match self.lex_token_tree_open_delim(delim) {
                    Ok(val) => buf.push(val),
                    Err(err) => return (TokenStream::new(buf), Err(err)),
                },
                TokenKind::CloseDelim(delim) => {
                    if !is_delimited {
                        let err = self
                            .string_reader
                            .session
                            .error_handler
                            .build_unexpected_closing_delimiter(delim, self.token.span);

                        return (TokenStream::new(buf), Err(err));
                    }
                    return (TokenStream::new(buf), Ok(()));
                }
                TokenKind::Eof => {
                    if is_delimited {
                        // This is weird but for display.
                        let span = Span::new(self.token.span.offset - 1, self.token.span.offset);
                        let err = self
                            .string_reader
                            .session
                            .error_handler
                            .build_unclosed_delimiter(
                                self.open_delims.iter().map(|&(_, span)| span).collect(),
                                Some(span),
                            );
                        return (TokenStream::new(buf), Err(err));
                    }
                    return (TokenStream::new(buf), Ok(()));
                }
                _ => {
                    // Get the next normal token.
                    // We will have the previous token, so we can try to glue.
                    let (this_tok, this_spacing) = self.eat(true);
                    match this_tok.kind {
                        TokenKind::DocComment(..) => {
                            // We will ignore doc comments for now.
                        }
                        _ => {
                            buf.push(TokenTree::Token(this_tok, this_spacing));
                        }
                    }
                }
            }
        }
    }

    fn lex_token_tree_open_delim(&mut self, open_delim: Delimiter) -> PResult<TokenTree> {
        // The span for beginning of the delimited section.
        let pre_span = self.token.span;

        self.open_delims.push((open_delim, self.token.span));

        // Lex the token trees within the delimiters.
        // We stop at any delimiter so we can try to recover if the user
        // uses an incorrect delimiter.
        let (tts, res) = self.lex_token_trees(/* is_delimited */ true);
        if res.is_err() {
            return Err(res.unwrap_err());
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
                let err = self
                    .string_reader
                    .session
                    .error_handler
                    .build_mismatched_closing_delimiter(close_delim, self.token.span, pre_span);

                return Err(err);
            }
            TokenKind::Eof => {
                // Silently recover, the EOF token will be seen again
                // and an error emitted then. Thus we don't pop from
                // self.open_delims here.
            }
            _ => unreachable!(),
        }

        Ok(TokenTree::Delimited(delim_span, open_delim, tts))
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
}
