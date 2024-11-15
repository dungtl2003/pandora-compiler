mod tokentrees;

use crate::ast::{
    BinOpToken, CommentKind, Delimiter, DocStyle, IdentIsRaw, Lit, LitKind, Token, TokenKind,
    TokenStream,
};
use crate::error_handler::ErrorHandler;
use crate::interner::Interner;
use crate::interner::Symbol;
use crate::lexer::{self, Base, Cursor, EscapeError, RawStrError};
use crate::session_global::BytePos;
use crate::span_encoding::Span;

use super::parser::PResult;

pub fn lex_token_tree<'src>(src: &'src str, emitter: ErrorHandler) -> PResult<TokenStream> {
    let string_reader = StringReader::new(src, emitter);

    let (tokenstream, res) = tokentrees::TokenTreesReader::lex_all_token_trees(string_reader);
    if res.is_err() {
        return Err(res.unwrap_err());
    }

    Ok(tokenstream)
}

pub fn tokenize<'src>(src: &'src str, emitter: ErrorHandler) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut string_reader = StringReader::new(src, emitter);

    loop {
        let token = string_reader.next_token();
        let is_last_token = token.kind == TokenKind::Eof;
        tokens.push(token);

        if is_last_token {
            return tokens;
        }
    }
}

struct StringReader<'src> {
    src: &'src str,
    pos: BytePos,
    cursor: Cursor<'src>,
    intener: Interner,
    emitter: ErrorHandler,
}

impl<'src> StringReader<'src> {
    fn new(src: &'src str, emitter: ErrorHandler) -> StringReader {
        StringReader {
            src,
            pos: 0,
            cursor: Cursor::new(src),
            intener: Interner::new(),
            emitter,
        }
    }

    fn next_token(&mut self) -> Token {
        // Skip trivial (whitespaces and comments) tokens.
        loop {
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

            let span = self.mk_sp(start_pos, (self.pos - start_pos) as usize);
            return Token::new(kind, span);
        }
    }

    fn mk_sp(&self, start: BytePos, len: usize) -> Span {
        Span {
            offset: start,
            length: len,
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
            println!("start: {start}, end: {end}");
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
            let start_content = start + n as u32 + 2;
            let end_content = end - n as u32 - 1;
            let symbol = self.symbol_from_to(start_content, end_content);
            TokenKind::Literal(Lit {
                kind: LitKind::RawStr(n),
                symbol,
            })
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
            todo!();
            //self.report_unterminated_str_literal();
        }

        let content_inside_quote = self.str_from_to(start + 1, end - 1); // remove ""
        let res: Result<String, lexer::EscapeError> = lexer::unescape_str(content_inside_quote);

        match res {
            Err(_) => {
                // TODO
                //self.report_unescape_char_literal();
                return TokenKind::Literal(Lit {
                    kind: LitKind::Err,
                    symbol: self.symbol_from_to(start, end),
                });
            }
            Ok(_) => {
                return TokenKind::Literal(Lit {
                    kind: LitKind::Str,
                    symbol: self.symbol_from_to(start + 1, end - 1),
                })
            }
        }
    }

    fn cook_char_literal(&mut self, terminated: bool, start: BytePos, end: BytePos) -> TokenKind {
        if !terminated {
            self.report_unterminated_character_literal(start);
            return TokenKind::Literal(Lit {
                kind: LitKind::Err,
                symbol: self.symbol_from_to(start, end),
            });
        }

        let content_inside_quote = self.str_from_to(start + 1, end - 1); // remove ''
        let res = lexer::unescape_char(content_inside_quote);

        match res {
            Err(e) => {
                self.report_unescape_character_literal(e, start, end);
                return TokenKind::Literal(Lit {
                    kind: LitKind::Err,
                    symbol: self.symbol_from_to(start, end),
                });
            }
            Ok(c) => {
                return TokenKind::Literal(Lit {
                    kind: LitKind::Char,
                    symbol: self.intener.intern(c.to_string().as_str()),
                })
            }
        }
    }

    fn cook_raw_ident(&mut self, content: &'src str) -> TokenKind {
        let symbol = self.intener.intern(content);
        TokenKind::Ident(symbol, IdentIsRaw::No)
    }

    fn cook_ident(&mut self, content: &'src str) -> TokenKind {
        let symbol = self.intener.intern(content);
        TokenKind::Ident(symbol, IdentIsRaw::Yes)
    }

    fn cook_doc_comment(
        &mut self,
        content: &'src str,
        comment_kind: CommentKind,
        doc_style: lexer::DocStyle,
    ) -> TokenKind {
        let symbol = self.intener.intern(content);
        let doc_style = match doc_style {
            lexer::DocStyle::Inner => Some(DocStyle::Inner),
            lexer::DocStyle::Outer => Some(DocStyle::Outer),
        };

        TokenKind::DocComment(comment_kind, doc_style, symbol)
    }

    fn char_from(&self, pos: BytePos) -> char {
        self.src[pos as usize..(pos + 1) as usize]
            .chars()
            .next()
            .unwrap()
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
        self.intener.intern(content)
    }

    fn report_unterminated_block_comment(
        &self,
        start: BytePos,
        doc_style: Option<lexer::DocStyle>,
    ) {
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
            self.emitter.report_unterminated_block_comment(
                msg,
                (start as usize + nested_open_idx, 2).into(),
                (start as usize + nested_close_idx, 2).into(),
            );
        }
    }

    fn report_unterminated_character_literal(&self, start_quote_pos: BytePos) {
        self.emitter
            .report_unterminated_character_literal((start_quote_pos as usize, 1).into());
    }

    fn report_unescape_character_literal(&self, err: EscapeError, start: BytePos, end: BytePos) {
        match err {
            EscapeError::ZeroChars => self
                .emitter
                .report_empty_char_literal(((start + 1) as usize, 1).into()),
            EscapeError::MoreThanOneChar => self
                .emitter
                .report_more_than_one_char_literal((start as usize, (end - start) as usize).into()),
            EscapeError::LoneSlash => unimplemented!(),
            EscapeError::InvalidEscape => self.emitter.report_unknown_char_escape(
                self.char_from(start + 1),
                ((start + 1) as usize, 1).into(),
            ),
            EscapeError::EscapeOnlyChar => self.emitter.report_escape_only_char(
                self.char_from(start + 1),
                ((start + 1) as usize, 1).into(),
            ),
        }
    }

    fn report_unknown_symbol(&self, start: BytePos, end: BytePos) {
        self.emitter.report_unknown_symbol(
            self.str_from_to(start, end).to_string(),
            (start as usize, end as usize).into(),
        );
    }

    fn report_raw_string_error(&self, start: BytePos) {
        match lexer::validate_raw_string(self.str_from(start)) {
            Err(RawStrError::InvalidStarter { bad_char }) => {
                self.emitter.report_raw_str_invalid_starter(
                    bad_char,
                    (start as usize, self.pos as usize).into(),
                );
            }
            Err(RawStrError::NoTerminator {
                expected,
                found,
                possible_terminator_offset,
            }) => {
                let start_span = (start as usize, (start + expected) as usize).into();
                let possible_terminator_span = if found > 0 {
                    let terminator_offset = possible_terminator_offset.unwrap();
                    Some(((start + terminator_offset) as usize, found as usize))
                } else {
                    None
                };
                self.emitter.report_raw_str_unterminated(
                    start_span,
                    expected,
                    possible_terminator_span.map(|value| value.into()),
                );
            }
            Err(RawStrError::TooManyHashes { found }) => {
                self.emitter.report_raw_str_too_many_hashes(
                    found,
                    (start as usize, (self.pos - start) as usize).into(),
                )
            }
            Ok(()) => panic!("no error found for supposedly invalid raw string literal"),
        };
    }

    fn report_no_digits_literal(&self, start: BytePos, end: BytePos) {
        self.emitter
            .report_no_digits_literal((start as usize, (end - start) as usize).into());
    }

    fn report_invalid_digits_literal(&self, base: u32, pos: BytePos) {
        self.emitter
            .report_invalid_digit_literal(base, (pos as usize, 1).into());
    }

    fn report_empty_exponent_float(&self, pos: BytePos, len: usize) {
        self.emitter
            .report_empty_exponent_float((pos as usize, len).into());
    }

    fn report_float_literal_unsupported_base(&self, base: String, pos: BytePos, len: usize) {
        self.emitter
            .report_float_literal_unsupported_base(base, (pos as usize, len).into());
    }
}
