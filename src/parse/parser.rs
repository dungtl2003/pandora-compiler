mod expr;
mod stmt;
mod ty;

use std::fmt::Display;
use std::mem;

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

use super::errors::PError;
use super::{lexer, PResult};

pub fn parse(contents: &str, session: &mut Session) -> Option<Ast> {
    let tokens = lexer::lex_token_tree(contents, session);

    if !session.can_recover() {
        return None;
    }

    if tokens.is_err() {
        tokens.err().unwrap().iter().for_each(|err| {
            let report = err.clone().to_report(&session.error_handler);
            session.error_handler.report_err(report)
        });
        return None;
    }

    let mut parser = Parser::new(tokens.unwrap());

    let mut errors: Vec<PError> = Vec::new();
    let mut stmts: Vec<Box<Stmt>> = Vec::new();
    while parser.token.kind != TokenKind::Eof {
        let result = parser.parse_stmt();
        match result {
            Ok(stmt) => stmts.push(stmt),
            Err(mut err) => {
                errors.append(&mut err);
                parser.recover();
            }
        }
    }

    if !errors.is_empty() {
        errors.iter().for_each(|err| {
            let report = err.clone().to_report(&session.error_handler);
            session.error_handler.report_err(report)
        });
        return None;
    }

    if session.has_error() {
        return None;
    }

    let ast = Ast::new(stmts);
    Some(ast)
}

pub struct Parser {
    /// The current token.
    pub token: Token,
    /// The spacing for the current token.
    token_spacing: Spacing,
    /// The previous token.
    pub prev_token: Token,
    expected_tokens: Vec<TokenType>,
    token_cursor: TokenCursor,
}

impl Parser {
    pub fn new(stream: TokenStream) -> Self {
        let mut parser = Parser {
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

    fn expect(&mut self, expected: TokenKind) -> PResult<()> {
        if self.token.kind == expected {
            return Ok(());
        }

        let err = PError::ExpectedToken {
            expected: [TokenType::Token(expected)].to_vec(),
            found: TokenType::Token(self.token.kind),
            span: self.token.span,
            prev_span: self.prev_token.span,
        };

        Err(vec![err])
    }

    fn parse_ident(&mut self) -> PResult<Ident> {
        let (ident, is_raw) = self.ident_or_err()?;

        if matches!(is_raw, IdentIsRaw::No) && kw::is_keyword(ident.name) {
            let err = PError::ExpectedIdentifier {
                found: TokenType::Keyword(ident.name),
                span: self.token.span,
            };
            return Err(vec![err]);
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

                let err = PError::ExpectedIdentifier {
                    found: tok_type,
                    span: self.token.span,
                };

                Err(vec![err])
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Token(TokenKind),
    Keyword(Symbol),
    Operator,
    Ident,
    Const,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Token(kind) => write!(f, "`{}`", kind),
            TokenType::Keyword(kw) => write!(f, "`{}`", kw),
            TokenType::Operator => write!(f, "operator"),
            TokenType::Ident => write!(f, "identifier"),
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
