mod tokentrees;

use std::ops::Range;

use crate::ast::{
    BinOpToken, CommentKind, Delimiter, DocStyle, IdentIsRaw, Lit, LitKind, Token, TokenKind,
    TokenStream,
};
use crate::lexer::{self, Base, Cursor, EscapeError, Mode, RawStrError};
use crate::session::{BytePos, Session};
use crate::span_encoding::Span;
use crate::symbol::Symbol;
use crate::ErrorType;

use super::PResult;

pub fn lex_token_tree<'sess, 'src>(
    src: &'src str,
    session: &'sess mut Session,
) -> PResult<TokenStream> {
    let string_reader = StringReader::new(src, session);

    let (tokenstream, res) = tokentrees::TokenTreesReader::lex_all_token_trees(string_reader);

    if res.is_err() {
        return Err(res.unwrap_err());
    }

    Ok(tokenstream)
}

struct StringReader<'sess, 'src> {
    src: &'src str,
    pos: BytePos,
    cursor: Cursor<'src>,
    session: &'sess mut Session,
}

impl<'sess, 'src> StringReader<'sess, 'src> {
    fn new(src: &'src str, session: &'sess mut Session) -> StringReader<'sess, 'src> {
        StringReader {
            src,
            pos: 0,
            cursor: Cursor::new(src),
            session,
        }
    }

    fn next_token(&mut self) -> Token {
        // Skip trivial (whitespaces and comments) tokens.
        loop {
            if !self.session.can_recover() {
                return Token::new(TokenKind::Eof, self.mk_sp(self.pos, self.pos));
            }
            let token = self.cursor.advance_token();
            let start_pos = self.pos;
            self.pos = self.pos + token.len as BytePos;

            // Now "cook" the token, converting the simple `lexer::TokenKind` to rich `ast::TokenKind`.
            // This also turn strings into interned symbols.
            let kind = match token.kind {
                lexer::TokenKind::LineComment { doc_style } => {
                    // Skip normal comment
                    let Some(doc_style) = doc_style else {
                        continue;
                    };

                    let content_start = start_pos + 3; // skip "quotes" (//! or //@)
                    let content_end = self.pos;
                    let content = self.str_from_to(content_start, content_end);
                    self.cook_doc_comment(content, CommentKind::Line, doc_style)
                }
                lexer::TokenKind::BlockComment {
                    doc_style,
                    terminated,
                } => {
                    if !terminated {
                        self.report_unterminated_block_comment(start_pos, doc_style);
                    }

                    // Skip normal comment
                    let Some(doc_style) = doc_style else {
                        continue;
                    };

                    let content_start = start_pos + 3; // skip "quotes" (/*! or /*@)
                    let content_end = self.pos - if terminated { 2 } else { 0 };
                    let content = self.str_from_to(content_start, content_end);
                    self.cook_doc_comment(content, CommentKind::Block, doc_style)
                }
                lexer::TokenKind::Whitespace => {
                    continue;
                }
                lexer::TokenKind::Ident => {
                    let content = self.str_from_to(start_pos, self.pos);
                    self.cook_ident(content)
                }
                lexer::TokenKind::RawIdent => {
                    let content = self.str_from_to(start_pos + 2, self.pos); // skip r#
                    self.cook_raw_ident(content)
                }
                lexer::TokenKind::Literal(kind) => self.cook_literal(start_pos, self.pos, kind),

                lexer::TokenKind::Eq => TokenKind::Eq,
                lexer::TokenKind::Lt => TokenKind::Lt,
                lexer::TokenKind::Gt => TokenKind::Gt,
                lexer::TokenKind::Bang => TokenKind::Not,
                lexer::TokenKind::Tilde => TokenKind::Tilde,
                lexer::TokenKind::Plus => TokenKind::BinOp(BinOpToken::Plus),
                lexer::TokenKind::Minus => TokenKind::BinOp(BinOpToken::Minus),
                lexer::TokenKind::Star => TokenKind::BinOp(BinOpToken::Star),
                lexer::TokenKind::Slash => TokenKind::BinOp(BinOpToken::Slash),
                lexer::TokenKind::Percent => TokenKind::BinOp(BinOpToken::Percent),
                lexer::TokenKind::Caret => TokenKind::BinOp(BinOpToken::Caret),
                lexer::TokenKind::And => TokenKind::BinOp(BinOpToken::And),
                lexer::TokenKind::Or => TokenKind::BinOp(BinOpToken::Or),
                lexer::TokenKind::Dot => TokenKind::Dot,
                lexer::TokenKind::Comma => TokenKind::Comma,
                lexer::TokenKind::Semicolon => TokenKind::Semicolon,
                lexer::TokenKind::Colon => TokenKind::Colon,
                lexer::TokenKind::Question => TokenKind::Question,
                lexer::TokenKind::OpenParen => TokenKind::OpenDelim(Delimiter::Parenthesis),
                lexer::TokenKind::CloseParen => TokenKind::CloseDelim(Delimiter::Parenthesis),
                lexer::TokenKind::OpenBrace => TokenKind::OpenDelim(Delimiter::Brace),
                lexer::TokenKind::CloseBrace => TokenKind::CloseDelim(Delimiter::Brace),
                lexer::TokenKind::OpenBracket => TokenKind::OpenDelim(Delimiter::Bracket),
                lexer::TokenKind::CloseBracket => TokenKind::CloseDelim(Delimiter::Bracket),

                lexer::TokenKind::Unknown => {
                    self.report_unknown_symbol(start_pos, self.pos);
                    continue;
                }

                lexer::TokenKind::Eof => TokenKind::Eof,
            };

            let span = self.mk_sp(start_pos, self.pos);
            return Token::new(kind, span);
        }
    }

    fn mk_sp(&self, start: BytePos, end: BytePos) -> Span {
        Span {
            offset: start,
            length: (end - start) as usize,
        }
    }

    fn cook_literal(
        &mut self,
        start: BytePos,
        end: BytePos,
        kind: lexer::LiteralKind,
    ) -> TokenKind {
        match kind {
            lexer::LiteralKind::Char { terminated } => {
                self.cook_char_literal(terminated, start, end)
            }
            lexer::LiteralKind::Str { terminated } => self.cook_str_literal(terminated, start, end),
            lexer::LiteralKind::RawStr { n_hashes } => {
                self.cook_raw_str_literal(n_hashes, start, end)
            }
            lexer::LiteralKind::Int {
                base,
                empty_int: empty_digit,
            } => self.cook_int_literal(base, empty_digit, start, end),
            lexer::LiteralKind::Float {
                base,
                empty_exponent,
            } => self.cook_float_literal(base, empty_exponent, start, end),
        }
    }

    fn cook_int_literal(
        &mut self,
        base: Base,
        empty_digit: bool,
        start: BytePos,
        end: BytePos,
    ) -> TokenKind {
        let mut kind = LitKind::Int;

        if empty_digit {
            self.report_no_digits_literal(start, end);
            kind = LitKind::Err;
        } else if matches!(base, Base::Binary | Base::Octal) {
            let base = base as u32;
            let content = self.str_from_to(start + 2, end); // skips 0b | 0o
            for (idx, c) in content.char_indices() {
                if c != '_' && c.to_digit(base).is_none() {
                    self.report_invalid_digits_literal(base, start + 2 + idx as u32);
                    kind = LitKind::Err;
                }
            }
        }

        TokenKind::Literal(Lit {
            kind,
            symbol: self.symbol_from_to(start, end),
        })
    }

    fn cook_float_literal(
        &mut self,
        base: Base,
        empty_exponent: bool,
        start: BytePos,
        end: BytePos,
    ) -> TokenKind {
        let mut kind = LitKind::Float;
        if empty_exponent {
            self.report_empty_exponent_float(start, (end - start) as usize);
            kind = LitKind::Err;
        }

        let base = match base {
            Base::Binary => Some("binary"),
            Base::Octal => Some("octal"),
            Base::Hexadecimal => Some("hexadecimal"),
            _ => None,
        };

        if let Some(base) = base {
            self.report_float_literal_unsupported_base(
                base.to_string(),
                start,
                (end - start) as usize,
            );
            kind = LitKind::Err;
        }

        TokenKind::Literal(Lit {
            kind,
            symbol: self.symbol_from_to(start, end),
        })
    }

    fn cook_raw_str_literal(
        &mut self,
        n_hashes: Option<u8>,
        start: BytePos,
        end: BytePos,
    ) -> TokenKind {
        if let Some(n) = n_hashes {
            // r##" "#
            self.cook_unicode(
                LitKind::RawStr(n),
                Mode::RawStr,
                start,
                end,
                2 + n as u32,
                1 + n as u32,
            )
        } else {
            self.report_raw_string_error(start);
            TokenKind::Literal(Lit {
                kind: LitKind::Err,
                symbol: self.symbol_from_to(start, end),
            })
        }
    }

    fn cook_str_literal(&mut self, terminated: bool, start: BytePos, end: BytePos) -> TokenKind {
        if !terminated {
            self.report_unterminated_str_literal(start, end);
            return TokenKind::Literal(Lit {
                kind: LitKind::Err,
                symbol: self.symbol_from_to(start, end),
            });
        }

        self.cook_unicode(
            LitKind::Str,
            Mode::Str,
            start,
            end,
            1, // skip "
            1, // skip "
        )
    }

    fn cook_char_literal(&mut self, terminated: bool, start: BytePos, end: BytePos) -> TokenKind {
        if !terminated {
            self.report_unterminated_character_literal(start);
            return TokenKind::Literal(Lit {
                kind: LitKind::Err,
                symbol: self.symbol_from_to(start, end),
            });
        }

        self.cook_unicode(
            LitKind::Char,
            Mode::Char,
            start,
            end,
            1, // skip '
            1, // skip '
        )
    }

    fn cook_unicode(
        &mut self,
        kind: LitKind,
        mode: Mode,
        start: BytePos,
        end: BytePos,
        prefix_len: u32,
        postfix_len: u32,
    ) -> TokenKind {
        self.cook_common(
            kind,
            mode,
            start,
            end,
            prefix_len,
            postfix_len,
            |src, mode, callback| {
                lexer::unescape_unicode(src, mode, &mut |span, res| {
                    callback(span, res.map(drop));
                });
            },
        )
    }

    fn cook_common(
        &mut self,
        mut kind: LitKind,
        mode: Mode,
        start: BytePos,
        end: BytePos,
        prefix_len: u32,
        postfix_len: u32,
        unescape: fn(&str, Mode, &mut dyn FnMut(Range<usize>, Result<(), EscapeError>)),
    ) -> TokenKind {
        let content_start = start + prefix_len;
        let content_end = end - postfix_len;
        let lit_content = self.str_from_to(content_start, content_end);
        unescape(lit_content, mode, &mut |range, res| {
            if let Err(err) = res {
                // just check error
                let span_with_quotes = self.mk_sp(start, end);
                let (start, end) = (range.start as u32, range.end as u32);
                let lo = content_start + start;
                let hi = lo + end - start;
                let span = self.mk_sp(lo, hi);
                self.report_unescape_error(err, span_with_quotes, span);

                kind = LitKind::Err;
            }
        });

        let sym = if !matches!(kind, LitKind::Err) {
            Symbol::from(lit_content)
        } else {
            self.symbol_from_to(start, end)
        };

        TokenKind::Literal(Lit { kind, symbol: sym })
    }

    fn cook_raw_ident(&mut self, content: &'src str) -> TokenKind {
        let symbol: Symbol = content.into();
        TokenKind::Ident(symbol, IdentIsRaw::Yes)
    }

    fn cook_ident(&mut self, content: &'src str) -> TokenKind {
        let symbol: Symbol = content.into();
        TokenKind::Ident(symbol, IdentIsRaw::No)
    }

    fn cook_doc_comment(
        &mut self,
        content: &'src str,
        comment_kind: CommentKind,
        doc_style: lexer::DocStyle,
    ) -> TokenKind {
        let symbol: Symbol = content.into();
        let doc_style = match doc_style {
            lexer::DocStyle::Inner => Some(DocStyle::Inner),
            lexer::DocStyle::Outer => Some(DocStyle::Outer),
        };

        TokenKind::DocComment(comment_kind, doc_style, symbol)
    }

    fn str_from_sp(&self, span: Span) -> String {
        let str = self
            .str_from_to(span.offset, span.offset + span.length as BytePos)
            .to_string();
        // escape special characters
        str.replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\t", "\\t")
            .replace("\0", "\\0")
            .replace("\'", "\\'")
    }

    fn char_from(&self, pos: BytePos) -> char {
        let ch = self.src[pos as usize..].chars().next().unwrap();

        match ch {
            '\n' => 'n',
            '\r' => 'r',
            '\t' => 't',
            '\0' => '0',
            _ => ch,
        }
    }

    /// Slice str from start (inclusive) to end (exclusive).
    fn str_from_to(&self, start: BytePos, end: BytePos) -> &'src str {
        &self.src[start as usize..end as usize]
    }

    /// Slice str from start (inclusive) to end of source code.
    fn str_from(&self, start: BytePos) -> &'src str {
        &self.src[start as usize..]
    }

    /// Symbol from start (inclusive) to end (exclusive).
    fn symbol_from_to(&mut self, start: BytePos, end: BytePos) -> Symbol {
        let content = self.str_from_to(start, end);
        content.into()
    }

    fn report_unterminated_str_literal(&mut self, start_quote_pos: BytePos, end: BytePos) {
        self.session.set_error(ErrorType::Unrecoverable);
        let report = self
            .session
            .error_handler
            .build_unterminated_string_literal_error(Span {
                offset: start_quote_pos,
                length: (end - start_quote_pos) as usize,
            })
            .into();

        self.session.error_handler.report_err(report);
    }

    fn report_no_digits_literal(&mut self, start: BytePos, end: BytePos) {
        self.session.set_error(ErrorType::Recoverable);
        let span = Span {
            offset: start,
            length: (end - start) as usize,
        };
        let report = self
            .session
            .error_handler
            .build_no_digits_literal_error(span)
            .into();
        self.session.error_handler.report_err(report);
    }

    fn report_float_literal_unsupported_base(&mut self, base: String, pos: BytePos, len: usize) {
        self.session.set_error(ErrorType::Recoverable);
        let span = Span {
            offset: pos,
            length: len,
        };
        let report = self
            .session
            .error_handler
            .build_float_literal_unsupported_base_error(base, span)
            .into();
        self.session.error_handler.report_err(report);
    }

    fn report_invalid_digits_literal(&mut self, base: u32, pos: BytePos) {
        self.session.set_error(ErrorType::Recoverable);
        let span = Span {
            offset: pos,
            length: 1,
        };

        let report = self
            .session
            .error_handler
            .build_invalid_digit_literal_error(base, span)
            .into();
        self.session.error_handler.report_err(report);
    }

    fn report_empty_exponent_float(&mut self, pos: BytePos, len: usize) {
        self.session.set_error(ErrorType::Recoverable);
        let span = Span {
            offset: pos,
            length: len,
        };
        let report = self
            .session
            .error_handler
            .build_empty_exponent_float_error(span)
            .into();

        self.session.error_handler.report_err(report);
    }

    fn report_unterminated_block_comment(
        &mut self,
        start: BytePos,
        doc_style: Option<lexer::DocStyle>,
    ) {
        self.session.set_error(ErrorType::Unrecoverable);
        let msg = match doc_style {
            Some(_) => "unterminated block doc-comment",
            None => "unterminated block comment",
        }
        .to_string();

        let mut nested_block_comment_open_idxs = vec![];
        let mut last_nested_block_comment_idxs: Option<(usize, usize)> = None;
        let mut content_chars = self.str_from(start).char_indices().peekable();

        while let Some((idx, current_char)) = content_chars.next() {
            match content_chars.peek() {
                Some((_, '*')) if current_char == '/' => {
                    // case /*
                    nested_block_comment_open_idxs.push(idx);
                }
                Some((_, '/')) if current_char == '*' => {
                    last_nested_block_comment_idxs = nested_block_comment_open_idxs
                        .pop()
                        .map(|open_idx| (open_idx, idx));
                }
                _ => {}
            }
        }

        if let Some((nested_open_idx, nested_close_idx)) = last_nested_block_comment_idxs {
            let report = self
                .session
                .error_handler
                .build_unterminated_block_comment_error(
                    msg,
                    Span {
                        offset: start + nested_open_idx as BytePos,
                        length: 2,
                    },
                    Span {
                        offset: start + nested_close_idx as BytePos,
                        length: 2,
                    },
                )
                .into();

            self.session.error_handler.report_err(report);
        }
    }

    fn report_unterminated_character_literal(&mut self, start_quote_pos: BytePos) {
        self.session.set_error(ErrorType::Unrecoverable);
        let report = self
            .session
            .error_handler
            .build_unterminated_character_literal_error(Span {
                offset: start_quote_pos,
                length: 1,
            })
            .into();

        self.session.error_handler.report_err(report);
    }

    fn report_unescape_error(&mut self, err: EscapeError, full_lit_span: Span, err_span: Span) {
        self.session.set_error(ErrorType::Recoverable);
        let report = match err {
            EscapeError::ZeroChars => self
                .session
                .error_handler
                .build_empty_char_literal_error(err_span)
                .into(),
            EscapeError::MoreThanOneChar => self
                .session
                .error_handler
                .build_more_than_one_char_literal_error(full_lit_span)
                .into(),
            EscapeError::LoneSlash => unimplemented!(),
            EscapeError::InvalidEscape => self
                .session
                .error_handler
                .build_unknown_char_escape_error(self.str_from_sp(err_span), err_span)
                .into(),
            EscapeError::EscapeOnlyChar => self
                .session
                .error_handler
                .build_escape_only_char_error(self.str_from_sp(err_span), err_span)
                .into(),
        };

        self.session.error_handler.report_err(report);
    }

    fn report_unknown_symbol(&mut self, start: BytePos, end: BytePos) {
        self.session.set_error(ErrorType::Recoverable);
        let report = self
            .session
            .error_handler
            .build_unknown_symbol_error(
                self.str_from_to(start, end).to_string(),
                Span {
                    offset: start,
                    length: (end - start) as usize,
                },
            )
            .into();

        self.session.error_handler.report_err(report)
    }

    fn report_raw_string_error(&mut self, start: BytePos) {
        let report = match lexer::validate_raw_string(self.str_from(start)) {
            Err(RawStrError::InvalidStarter { bad_char }) => {
                self.session.set_error(ErrorType::Unrecoverable);
                self.session
                    .error_handler
                    .build_raw_str_invalid_starter_error(
                        bad_char,
                        Span {
                            offset: start,
                            length: (self.pos - start) as usize,
                        },
                    )
                    .into()
            }
            Err(RawStrError::NoTerminator {
                expected,
                found,
                possible_terminator_offset,
            }) => {
                self.session.set_error(ErrorType::Unrecoverable);
                let start_span = Span {
                    offset: start,
                    length: 1,
                };
                let possible_terminator_span = if found > 0 {
                    let terminator_offset = possible_terminator_offset.unwrap();
                    Some(((start + terminator_offset) as usize, found as usize))
                } else {
                    None
                };
                self.session
                    .error_handler
                    .build_raw_str_unterminated_error(
                        start_span,
                        expected,
                        possible_terminator_span.map(|value| Span {
                            offset: value.0 as BytePos,
                            length: value.1,
                        }),
                    )
                    .into()
            }
            Err(RawStrError::TooManyHashes { found }) => {
                self.session.set_error(ErrorType::Unrecoverable);
                self.session
                    .error_handler
                    .build_raw_str_too_many_hashes_error(
                        found,
                        Span {
                            offset: start,
                            length: (self.pos - start) as usize,
                        },
                    )
                    .into()
            }
            Ok(()) => panic!("no error found for supposedly invalid raw string literal"),
        };

        self.session.error_handler.report_err(report);
    }
}
