use crate::ast::{Ty, TyKind};

use super::{PResult, Parser};

impl Parser {
    pub fn parse_ty(&mut self) -> PResult<Box<Ty>> {
        let path = self.parse_path()?;
        let span = path.span;
        let kind = TyKind::Path(*path);
        Ok(Box::new(Ty { kind, span }))
    }
}
