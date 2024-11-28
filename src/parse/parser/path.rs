use crate::ast::{Ident, IdentIsRaw, Path, PathSegment, TokenKind};

use super::{PResult, Parser};

impl Parser<'_> {
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

    fn parse_path_segment(&mut self) -> PResult<PathSegment> {
        let ident = self.parse_path_segment_ident()?;

        return Ok(PathSegment { ident });
    }

    pub fn parse_path_segment_ident(&mut self) -> PResult<Ident> {
        match self.token.ident() {
            Some((ident, IdentIsRaw::No)) if !ident.is_keyword() => {
                self.advance();
                Ok(ident)
            }
            _ => self.parse_ident(),
        }
    }
}
