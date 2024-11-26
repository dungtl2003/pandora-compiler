use std::fmt::{Display, Formatter};
use crate::ast::{
    AngleBracketedArg, AngleBracketedArgs, BinOpToken, GenericArg, GenericArgs, Ident, IdentIsRaw,
    Path, PathSegment, Token, TokenKind,
};

use super::{PResult, Parser};

/// Specifies how to parse a path.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum PathStyle {
    /// In some contexts, notably in expressions, paths with generic arguments are ambiguous
    /// with something else. For example, in expressions `segment < ....` can be interpreted
    /// as a comparison and `segment ( ....` can be interpreted as a function call.
    /// In all such contexts the non-path interpretation is preferred by default for practical
    /// reasons, but the path interpretation can be forced by the disambiguator `::`, e.g.
    /// `x<y>` - comparisons, `x::<y>` - unambiguously a path.
    ///
    /// Also, a path may never be followed by a `:`. This means that we can eagerly recover if
    /// we encounter it.
    Expr,
    /// In other contexts, notably in types, no ambiguity exists and paths can be written
    /// without the disambiguator, e.g., `x<y>` - unambiguously a path.
    /// Paths with disambiguators are still accepted, `x::<Y>` - unambiguously a path too.
    Type,
    /// A path with generic arguments disallowed, e.g., `foo::bar::Baz`, used in imports.
    Mod,
}

impl PathStyle {
    fn has_generic_ambiguity(&self) -> bool {
        matches!(self, Self::Expr)
    }
}

impl Parser<'_> {
    pub fn parse_path(&mut self, style: PathStyle) -> PResult<Box<Path>> {
        let start = self.token.span;
        let segments = self.parse_path_segments(style)?;
        let span = start.to(self.prev_token.span);

        Ok(Box::new(Path { segments, span }))
    }

    fn parse_path_segments(&mut self, style: PathStyle) -> PResult<Vec<PathSegment>> {
        let mut segments = Vec::new();
        loop {
            let segment = self.parse_path_segment(style)?;
            segments.push(segment);
            if !self.eat_path_sep() {
                break;
            }
        }

        Ok(segments)
    }

    fn eat_path_sep(&mut self) -> bool {
        if self.token.kind == TokenKind::PathSep {
            self.advance();
            true
        } else {
            false
        }
    }

    fn parse_path_segment(&mut self, style: PathStyle) -> PResult<PathSegment> {
        let ident = self.parse_path_segment_ident()?;
        let is_args_start = |token: &Token| {
            matches!(
                token.kind,
                TokenKind::Lt | TokenKind::BinOp(BinOpToken::Shl)
            )
        };

        if style != PathStyle::Mod && self.check_path_sep_and_look_ahead(is_args_start) {
            self.eat_path_sep();
            let args = self.parse_generic_args()?;
            let args = Some(args);
            return Ok(PathSegment { ident, args });
        }

        return Ok(PathSegment { ident, args: None });
    }

    pub fn parse_path_segment_ident(&mut self) -> PResult<Ident> {
        match self.token.ident() {
            Some((ident, IdentIsRaw::No)) if ident.is_path_segment_keyword() => {
                self.advance();
                Ok(ident)
            }
            _ => self.parse_ident(),
        }
    }

    fn parse_generic_args(&mut self) -> PResult<Box<GenericArgs>> {
        debug_assert!(self.token.kind == TokenKind::Lt); // `<`
        self.parse_angle_brackets_args()
            .map(|args| Box::new(GenericArgs::AngleBracketed(args)))
    }

    fn parse_angle_brackets_args(&mut self) -> PResult<AngleBracketedArgs> {
        debug_assert!(self.token.kind == TokenKind::Lt); // `<`
        self.eat_lt();
        let start = self.token.span;
        let mut args: Vec<AngleBracketedArg> = Vec::new();

        let is_args_end = |token: &Token| {
            matches!(
                token.kind,
                TokenKind::Gt | TokenKind::BinOp(BinOpToken::Shr) | TokenKind::Ge
            )
        };

        loop {
            self.advance();
            if self.token.kind == TokenKind::Eof {
                return Err("Unexpected EOF".into());
            }
            if self.look_ahead(0, is_args_end) {
                break;
            }

            let arg = self.parse_angle_bracketed_arg()?;
            args.push(arg);
            if self.token.kind == TokenKind::Comma {
                break;
            }
        }

        self.expect_gt()?;
        let span = start.to(self.token.span);
        Ok(AngleBracketedArgs { span, args })
    }

    fn parse_angle_bracketed_arg(&mut self) -> PResult<AngleBracketedArg> {
        let arg = self.parse_generic_arg()?;
        Ok(AngleBracketedArg::Arg(arg))
    }

    fn parse_generic_arg(&mut self) -> PResult<GenericArg> {
        let ty = self.parse_ty()?;

        Ok(GenericArg::Type(ty))
    }

    /// Checks for `::` and then look ahead after it.
    fn check_path_sep_and_look_ahead(&mut self, looker: impl Fn(&Token) -> bool) -> bool {
        if self.check(TokenKind::PathSep) {
            self.look_ahead(1, looker)
        } else {
            false
        }
    }
}
