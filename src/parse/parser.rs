mod expr;
mod stmt;
mod ty;

use std::fmt::Display;
use std::mem;

use crate::span_encoding::Span;
use crate::symbol::Symbol;
use crate::{
    ast::{
        Ast, DelimSpan, Delimiter, Ident, IdentIsRaw, Spacing, Stmt, Token, TokenKind, TokenStream,
        TokenTree, TokenTreeCursor,
    },
    kw::{self, Keyword},
    session::Session,
    span_encoding::DUMMY_SP,
};

use super::{lexer, PResult};

pub fn parse(contents: &str, session: &Session) -> Option<Ast> {
    let tokens = lexer::lex_token_tree(contents, session);
    if tokens.is_err() {
        session.error_handler.emit_err(tokens.err().unwrap());
        return None;
    }

    let mut parser = Parser::new(tokens.unwrap(), session);

    let mut errors = Vec::new();
    let mut stmts: Vec<Box<Stmt>> = Vec::new();
    while parser.token.kind != TokenKind::Eof {
        let result = parser.parse_stmt();
        match result {
            Ok(stmt) => stmts.push(stmt),
            Err(err) => {
                errors.push(err);
                parser.recover();
            }
        }
    }

    if !errors.is_empty() {
        for err in errors {
            session.error_handler.emit_err(err);
        }
        return None;
    }

    let ast = Ast::new(stmts);
    Some(ast)
}

pub struct Parser<'sess> {
    pub session: &'sess Session,
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
    pub fn new(stream: TokenStream, session: &'sess Session) -> Self {
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

    pub fn recover(&mut self) {
        loop {
            if self.token.kind == TokenKind::Eof {
                break;
            }

            if self.is_synchronized() {
                break;
            }

            self.advance();
        }
    }

    fn is_synchronized(&self) -> bool {
        if self.token.is_keyword(Keyword::Set)
            || self.token.is_keyword(Keyword::When)
            || self.token.is_keyword(Keyword::During)
            || self.token.is_keyword(Keyword::For)
            || self.token.is_keyword(Keyword::Yeet)
            || self.token.is_keyword(Keyword::Fun)
            || self.token.is_keyword(Keyword::Add)
            || self.token.is_keyword(Keyword::Br)
            || self.token.is_keyword(Keyword::Skip)
        {
            return true;
        }

        match self.token.kind {
            TokenKind::Semicolon => true,
            _ => false,
        }
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

    /// Advance the parser by one token using provided token as the next one.
    fn advance_with(&mut self, next: (Token, Spacing)) {
        let (next_token, next_spacing) = next;
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
            return Ok(());
        }

        Err(self.session.error_handler.build_expected_token_error(
            [TokenType::Token(expected)].to_vec(),
            TokenType::Token(self.token.kind),
            self.token.span,
        ))
    }

    /// Eats the expected token if it's present possibly breaking
    /// compound tokens like multi-character operators in process.
    /// Returns `true` if the token was eaten.
    fn break_and_eat(&mut self, expected: TokenKind) -> bool {
        if self.token.kind == expected {
            self.advance();
            return true;
        }
        match self.token.kind.break_two_token_op(1) {
            Some((first, second)) if first == expected => {
                let first_span = Span {
                    offset: self.token.span.offset,
                    length: 1,
                };
                let second_span = first_span.with_offset(self.token.span.end());
                self.token = Token::new(first, first_span);
                // Use the spacing of the glued token as the spacing of the
                // unglued second token.
                self.advance_with((Token::new(second, second_span), self.token_spacing));

                true
            }
            _ => {
                self.expected_tokens.push(TokenType::Token(expected));
                false
            }
        }
    }

    /// Eats `<` possibly breaking tokens like `<<` in process.
    fn eat_lt(&mut self) -> bool {
        self.break_and_eat(TokenKind::Lt)
    }

    /// Eats `<` possibly breaking tokens like `<<` in process.
    /// Signals an error if `<` was not eaten.
    fn expect_lt(&mut self) -> PResult<()> {
        if self.eat_lt() {
            return Ok(());
        }

        Err(self.session.error_handler.build_expected_token_error(
            self.expected_tokens.clone(),
            TokenType::Token(TokenKind::Lt),
            self.token.span,
        ))
    }

    /// Eats `>` possibly breaking tokens like `>>` in process.
    /// Signals an error if `>` was not eaten.
    fn expect_gt(&mut self) -> PResult<()> {
        if self.break_and_eat(TokenKind::Gt) {
            return Ok(());
        }

        Err(self.session.error_handler.build_expected_token_error(
            self.expected_tokens.clone(),
            TokenType::Token(TokenKind::Gt),
            self.token.span,
        ))
    }

    fn parse_ident(&mut self) -> PResult<Ident> {
        let (ident, is_raw) = self.ident_or_err()?;

        if matches!(is_raw, IdentIsRaw::No) && kw::is_keyword(ident.name) {
            return Err(self.session.error_handler.build_expected_identifier_error(
                &TokenType::Keyword(ident.name),
                self.token.span,
            ));
        }

        self.advance();
        Ok(ident)
    }

    fn ident_or_err(&mut self) -> PResult<(Ident, IdentIsRaw)> {
        match self.token.ident() {
            Some(ident) => Ok(ident),
            None => {
                let tok_type = if self.token.can_begin_operator() {
                    TokenType::Operator
                } else {
                    TokenType::Token(self.token.kind)
                };
                Err(self
                    .session
                    .error_handler
                    .build_expected_identifier_error(&tok_type, self.token.span))
            }
        }
    }

    /// Checks if the next token kind is `tok`, and returns `true` if so.
    ///
    /// This method will automatically add `tok` to `expected_tokens` if `tok` is not
    /// encountered.
    fn check(&mut self, tok: TokenKind) -> bool {
        let is_present = self.token.kind == tok;
        if !is_present {
            self.expected_tokens.push(TokenType::Token(tok));
        }
        is_present
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Token(TokenKind),
    Keyword(Symbol),
    Operator,
    Ident,
    Type,
    Const,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Token(kind) => write!(f, "`{}`", kind),
            TokenType::Keyword(kw) => write!(f, "`{}`", kw),
            TokenType::Operator => write!(f, "operator"),
            TokenType::Ident => write!(f, "identifier"),
            TokenType::Type => write!(f, "type"),
            TokenType::Const => write!(f, "constant"),
        }
    }
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
