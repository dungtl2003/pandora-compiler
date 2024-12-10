use crate::ast::{Delimiter, Expr, TokenKind, Ty, TyKind};

use super::{PResult, Parser};

impl Parser {
    pub fn parse_ty(&mut self) -> PResult<Ty> {
        if self.token.is_open_delim(Delimiter::Bracket) {
            self.parse_ty_array()
        } else {
            self.parse_ty_ident()
        }
    }

    fn parse_ty_ident(&mut self) -> PResult<Ty> {
        let start = self.token.span;
        let ident = self.parse_ident()?;
        let span = start.to(self.prev_token.span);

        let kind = TyKind::Named(ident);
        Ok(Ty { kind, span })
    }

    fn parse_ty_array(&mut self) -> PResult<Ty> {
        debug_assert!(self.token.is_open_delim(Delimiter::Bracket));

        let start = self.token.span;
        self.advance();

        let ty = self.parse_ty()?;
        let mut len: Option<Box<Expr>> = None;
        if self.token.kind == TokenKind::Semicolon {
            self.advance();
            len = Some(self.parse_expr()?);
        }

        self.expect(TokenKind::CloseDelim(Delimiter::Bracket))?;
        self.advance();

        let span = start.to(self.prev_token.span);

        let kind = TyKind::Array(Box::new(ty), len);
        Ok(Ty { kind, span })
    }
}
