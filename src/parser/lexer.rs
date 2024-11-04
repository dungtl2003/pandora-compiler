use crate::ast::{
    BinOpToken, CommentKind, Delimiter, DocStyle, IdentIsRaw, Lit, LitKind, Token, TokenKind,
};
use crate::interner::Interner;
use crate::interner::Symbol;
use crate::lexer::{self, Base, Cursor};
use crate::session_global::BytePos;

struct StringReader<'src> {
    src: &'src str,
    pos: BytePos,
    cursor: Cursor<'src>,
    intener: Interner,
}

impl<'src> StringReader<'src> {
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
                    let content_end = self.pos - start_pos;
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
                    let content_end = self.pos - start_pos - if terminated { 2 } else { 0 };
                    let content = self.str_from_to(content_start, content_end);
                    self.cook_doc_comment(content, CommentKind::Block, doc_style)
                }
                lexer::TokenKind::Whitespace => {
                    continue;
                }
                lexer::TokenKind::Ident => {
                    let content = self.str_from_to(start_pos, self.pos - start_pos);
                    self.cook_ident(content)
                }
                lexer::TokenKind::RawIdent => {
                    let content = self.str_from_to(start_pos + 2, self.pos - start_pos); // skip r#
                    self.cook_ident(content)
                }
                lexer::TokenKind::Literal(kind) => {
                    self.cook_literal(start_pos, self.pos - start_pos, kind)
                }

                lexer::TokenKind::Eq => TokenKind::Eq,
                lexer::TokenKind::Lt => TokenKind::Lt,
                lexer::TokenKind::Le => TokenKind::Le,
                lexer::TokenKind::EqEq => TokenKind::EqEq,
                lexer::TokenKind::BangEq => TokenKind::Ne,
                lexer::TokenKind::Ge => TokenKind::Ge,
                lexer::TokenKind::Gt => TokenKind::Gt,
                lexer::TokenKind::AndAnd => TokenKind::AndAnd,
                lexer::TokenKind::OrOr => TokenKind::OrOr,
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
                lexer::TokenKind::Shl => TokenKind::BinOp(BinOpToken::Shl),
                lexer::TokenKind::Shr => TokenKind::BinOp(BinOpToken::Shr),
                lexer::TokenKind::Dot => TokenKind::Dot,
                lexer::TokenKind::Comma => TokenKind::Comma,
                lexer::TokenKind::Semicolon => TokenKind::Semicolon,
                lexer::TokenKind::Colon => TokenKind::Colon,
                lexer::TokenKind::Question => TokenKind::Question,
                lexer::TokenKind::OpenParen => TokenKind::OpenDelim(Delimiter::Parenthesis),
                lexer::TokenKind::CloseParen => TokenKind::OpenDelim(Delimiter::Parenthesis),
                lexer::TokenKind::OpenBrace => TokenKind::OpenDelim(Delimiter::Brace),
                lexer::TokenKind::CloseBrace => TokenKind::OpenDelim(Delimiter::Brace),
                lexer::TokenKind::OpenBracket => TokenKind::OpenDelim(Delimiter::Bracket),
                lexer::TokenKind::CloseBracket => TokenKind::OpenDelim(Delimiter::Bracket),

                lexer::TokenKind::Unknown => {
                    self.report_unknown_symbol();
                    continue;
                }

                lexer::TokenKind::Eof => TokenKind::Eof,
            };

            return Token { kind };
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
            lexer::LiteralKind::Number {
                base,
                empty_digit,
                empty_exponent,
            } => self.cook_number_literal(base, empty_digit, empty_exponent, start, end),
        }
    }

    fn cook_number_literal(
        &mut self,
        base: Base,
        empty_digit: bool,
        empty_exponent: bool,
        start: BytePos,
        end: BytePos,
    ) -> TokenKind {
        todo!();
    }

    fn cook_raw_str_literal(
        &mut self,
        n_hashes: Option<u8>,
        start: BytePos,
        end: BytePos,
    ) -> TokenKind {
        todo!();
    }

    fn cook_str_literal(&mut self, terminated: bool, start: BytePos, end: BytePos) -> TokenKind {
        if !terminated {
            self.report_unterminated_str_literal();
        }

        let content_inside_quote = self.str_from_to(start+1, end-1); // remove ""
        let res: Result<String, lexer::EscapeError> = lexer::unescape_str(content_inside_quote);

        match res {
            Err(_) => {
                self.report_unescape_character_literal();
                return TokenKind::Literal(Lit {
                    kind: LitKind::Err,
                    symbol: self.symbol_from_to(start, end),
                });
            }
            Ok(s) => {
                return TokenKind::Literal(Lit {
                    kind: LitKind::Str,
                    symbol: self.intener.intern(s.as_str()),
                })
            }
        }
    }

    fn cook_char_literal(&mut self, terminated: bool, start: BytePos, end: BytePos) -> TokenKind {
        if !terminated {
            self.report_unterminated_character_literal();
        }

        let content_inside_quote = self.str_from_to(start + 1, end - 1); // remove ''
        let res = lexer::unescape_char(content_inside_quote);

        match res {
            Err(_) => {
                self.report_unescape_character_literal();
                return TokenKind::Literal(Lit {
                    kind: LitKind::Err,
                    symbol: self.symbol_from_to(start, end),
                });
            }
            Ok(_) => {
                return TokenKind::Literal(Lit {
                    kind: LitKind::Char,
                    symbol: self.symbol_from_to(start + 1, end - 1),
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

    /// Slice str from start (inclusive) to end (exclusive).
    fn str_from_to(&self, start: BytePos, end: BytePos) -> &'src str {
        &self.src[start as usize..end as usize]
    }

    fn symbol_from_to(&mut self, start: BytePos, end: BytePos) -> Symbol {
        let content = self.str_from_to(start, end);
        self.intener.intern(content)
    }

    fn report_unterminated_block_comment(
        &self,
        start: BytePos,
        doc_style: Option<lexer::DocStyle>,
    ) {
        unimplemented!();
    }

    fn report_unterminated_character_literal(&self) {
        unimplemented!();
    }

    fn report_unescape_character_literal(&self) {
        unimplemented!();
    }

    fn report_unknown_symbol(&self) {
        unimplemented!();
    }

    fn report_unterminated_str_literal(&self){
        unimplemented!();
    }

}
