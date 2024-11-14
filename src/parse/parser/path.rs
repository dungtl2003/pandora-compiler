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
            if !self.look_ahead(1, |tok| tok.kind == TokenKind::ColonColon) {
                break;
            }
            self.advance(); // `::`
        }

        Ok(segments)
    }

    fn parse_path_segment(&mut self) -> PResult<PathSegment> {
        if !self.look_ahead(1, |tok| tok.is_ident()) {
            return Err(());
        }
        self.advance(); // identifier
        Ok(PathSegment {
            ident: self.token.ident().unwrap().0,
        })
    }

    fn parse_generic_args(&mut self) -> PResult<Box<GenericArgs>> {
        unimplemented!();
    }

    fn parse_angle_brackets_args(&mut self) -> PResult<AngleBracketedArgs> {
        unimplemented!();
    }

    fn parse_angle_bracketed_arg(&mut self) -> PResult<AngleBracketedArg> {
        unimplemented!();
    }

    fn parse_generic_arg(&mut self) -> PResult<GenericArg> {
        unimplemented!();
    }
}
