use core::fmt;
use std::fmt::{Display, Formatter};

use symbol::Symbol as Sym;

use crate::kw::{self, Keyword};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Symbol {
    sym: Sym,
}

impl From<&str> for Symbol {
    fn from(s: &str) -> Self {
        Self { sym: s.into() }
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        self.sym.as_str()
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.sym.as_str())
    }
}

impl Symbol {
    pub fn as_str(&self) -> &str {
        self.sym.as_str()
    }

    pub fn is_bool_lit(&self) -> bool {
        match kw::from_str(self.sym.as_str()) {
            Ok(Keyword::True) | Ok(Keyword::False) => true,
            _ => false,
        }
    }
}
