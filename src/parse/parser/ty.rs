use crate::ast::Ty;

use super::{PResult, Parser};

impl Parser<'_> {
    pub fn parse_ty(&mut self) -> PResult<Ty> {
        let start = self.token.span;
        let name = self.parse_ident()?.name;
        let span = start.to(self.prev_token.span);

        Ok(Ty { name, span })
    }
}

