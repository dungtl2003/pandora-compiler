use crate::ast::{
    AngleBracketedArg, AngleBracketedArgs, GenericArg, GenericArgs, Path, PathSegment, TokenKind,
};

use super::{PResult, Parser};

impl Parser {
    pub fn parse_path(&mut self) -> PResult<Box<Path>> {
        let start = self.token.span;
        let segments = self.parse_path_segments()?;
        let span = start.to(self.prev_token.span);

        Ok(Box::new(Path { segments, span }))
    }

    fn parse_path_segments(&mut self) -> PResult<Vec<PathSegment>> {
        let mut segments = Vec::new();
        loop {
            let segment = self.parse_path_segment()?;
            segments.push(segment);
            match self.token.kind {
                TokenKind::ColonColon => self.advance(), // `::`
                _ => break,
            }
        }

        Ok(segments)
    }

    fn parse_path_segment(&mut self) -> PResult<PathSegment> {
        match self.token.kind {
            TokenKind::Ident(..) => {}
            _ => return Err("Expected identifier".into()),
        }

        self.advance(); // identifier
        let ident = self.prev_token.ident().unwrap().0;
        let args = if self.look_ahead(1, |tok| tok.kind == TokenKind::Lt) {
            // `<`
            Some(self.parse_generic_args()?)
        } else {
            None
        };

        Ok(PathSegment { ident, args })
    }

    fn parse_generic_args(&mut self) -> PResult<Box<GenericArgs>> {
        debug_assert!(self.look_ahead(1, |tok| tok.kind == TokenKind::Lt)); // `<`
        self.parse_angle_brackets_args()
            .map(|args| Box::new(GenericArgs::AngleBracketed(args)))
    }

    fn parse_angle_brackets_args(&mut self) -> PResult<AngleBracketedArgs> {
        debug_assert!(self.look_ahead(1, |tok| tok.kind == TokenKind::Lt)); // `<`
        let start = self.token.span;
        self.advance();
        let mut args: Vec<AngleBracketedArg> = Vec::new();

        loop {
            if self.look_ahead(1, |tok| tok.kind == TokenKind::Eof) {
                return Err("Unexpected EOF".into());
            }
            if self.look_ahead(1, |tok| tok.kind == TokenKind::Gt) {
                break;
            }

            let arg = self.parse_angle_bracketed_arg()?;
            args.push(arg);
            if !self.look_ahead(1, |tok| tok.kind == TokenKind::Comma) {
                break;
            }
            self.advance(); // `,`
        }

        if !self.look_ahead(1, |tok| tok.kind == TokenKind::Gt) {
            // `>`
            return Err("Expected `>`".into());
        }

        self.advance(); // `>`
        let span = start.to(self.prev_token.span);
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
}
