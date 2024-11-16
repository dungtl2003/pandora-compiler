mod expr;
mod path;
mod stmt;
mod ty;

use std::mem;
use symbol::Symbol;

use crate::{
    ast::{
        DelimSpan, Delimiter, Spacing, Stmt, Token, TokenKind, TokenStream, TokenTree,
        TokenTreeCursor,
    },
    kw::{self, Keyword},
    session_global::SessionGlobal,
    span_encoding::DUMMY_SP,
};

pub fn parse(tokens: TokenStream, session: &SessionGlobal) -> PResult<Vec<Box<Stmt>>> {
    let mut stmts: Vec<Box<Stmt>> = Vec::new();
    let mut parser = Parser::new(tokens, session);

    while parser.token.kind != TokenKind::Eof {
        let stmt = parser.parse_stmt()?;
        stmts.push(stmt);
    }

    Ok(stmts)
}

pub struct Parser<'sess> {
    pub session: &'sess SessionGlobal,
    /// The current token.
    pub token: Token,
    /// The spacing for the current token.
    token_spacing: Spacing,
    /// The previous token.
    pub prev_token: Token,
    expected_tokens: Vec<TokenType>,
    token_cursor: TokenCursor,
}

impl<'sess> Parser<'sess> {
    pub fn new(stream: TokenStream, session: &'sess SessionGlobal) -> Self {
        let mut parser = Parser {
            session,
            token: Token::dummy(),
            token_spacing: Spacing::Alone,
            prev_token: Token::dummy(),
            expected_tokens: Vec::new(),
            token_cursor: TokenCursor {
                tree_cursor: stream.into_trees(),
                stack: Vec::new(),
            },
        };

        // Make parser point to the first token.
        parser.advance();

        parser
    }

    /// Advance the parser by one token.
    pub fn advance(&mut self) {
        let (next_token, next_spacing) = self.token_cursor.next();
        // Update the current and previous tokens.
        self.prev_token = mem::replace(&mut self.token, next_token);
        self.token_spacing = next_spacing;

        // Diagnostics.
        self.expected_tokens.clear();
    }

    /// Look-ahead `dist` tokens of `self.token` and get access to that token there.
    /// When `dist == 0` then the current token is looked at. `Eof` will be
    /// returned if the look-ahead is any distance past the end of the tokens.
    pub fn look_ahead<R>(&self, dist: usize, looker: impl FnOnce(&Token) -> R) -> R {
        if dist == 0 {
            return looker(&self.token);
        }

        let mut cursor = self.token_cursor.clone();
        let mut i = 0;
        let mut token = Token::dummy();
        while i < dist {
            token = cursor.next().0;
            i += 1;
        }
        looker(&token)
    }

    pub fn is_keyword_ahead(&mut self, kws: &[Keyword]) -> bool {
        self.look_ahead(1, |tok| {
            if let TokenKind::Ident(ident, _) = tok.kind {
                kws.iter().any(|kw| {
                    let res = kw::from_str(ident.as_str());
                    if res.is_err() {
                        return false;
                    }

                    *kw == res.unwrap()
                })
            } else {
                false
            }
        })
    }

    pub fn is_ident_ahead(&mut self) -> bool {
        self.look_ahead(1, |tok| matches!(tok.kind, TokenKind::Ident(..)))
    }

    fn expect(&mut self, expected: TokenKind) -> PResult<()> {
        if self.token.kind == expected {
            Ok(())
        } else {
            Err(format!(
                "expected {:?}, found {:?}",
                expected, self.token.kind
            ))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    Token(TokenKind),
    Keyword(Symbol),
    Operator,
    Ident,
    Type,
    Const,
}

/// Iterator over a `TokenStream` that produces `Token`s. It's a bit odd that
/// we (a) lex tokens into a nice tree structure (`TokenStream`), and then (b)
/// use this type to emit them as a linear sequence. But a linear sequence is
/// what the parser expects, for the most part.
#[derive(Clone, Debug)]
struct TokenCursor {
    // Cursor for the current (innermost) token stream. The delimiters for this
    // token stream are found in `self.stack.last()`; when that is `None` then
    // we are in the outermost token stream which never has delimiters.
    tree_cursor: TokenTreeCursor,

    // Token streams surrounding the current one. The delimiters for stack[n]'s
    // tokens are in `stack[n-1]`. `stack[0]` (when present) has no delimiters
    // because it's the outermost token stream which never has delimiters.
    stack: Vec<(TokenTreeCursor, DelimSpan, Delimiter)>,
}

impl TokenCursor {
    fn next(&mut self) -> (Token, Spacing) {
        loop {
            if let Some(tree) = self.tree_cursor.next_ref() {
                match tree {
                    &TokenTree::Token(ref token, spacing) => {
                        debug_assert!(!matches!(
                            token.kind,
                            TokenKind::OpenDelim(_) | TokenKind::CloseDelim(_)
                        ));
                        return (token.clone(), spacing);
                    }
                    &TokenTree::Delimited(span, delim, ref tts) => {
                        let trees = tts.clone().into_trees();
                        self.stack
                            .push((mem::replace(&mut self.tree_cursor, trees), span, delim));
                        return (
                            Token::new(TokenKind::OpenDelim(delim), span.open),
                            Spacing::Alone,
                        );
                    }
                }
            } else if let Some((tree_cursor, span, delim)) = self.stack.pop() {
                // We have exhausted this token stream. Move back to its parent token stream.
                self.tree_cursor = tree_cursor;
                return (
                    Token::new(TokenKind::CloseDelim(delim), span.close),
                    Spacing::Alone,
                );
                // No close delimiter to return; continue on to the next iteration.
            } else {
                // We have exhausted the outermost token stream. The use of
                // `Spacing::Alone` is arbitrary and immaterial, because the
                // `Eof` token's spacing is never used.
                return (Token::new(TokenKind::Eof, DUMMY_SP), Spacing::Alone);
            }
        }
    }
}

// TODO: Update later.
pub type PError = String;
pub type PResult<T> = Result<T, PError>;
