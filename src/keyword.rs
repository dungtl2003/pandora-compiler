use std::str::FromStr;
use strum_macros::{AsRefStr, EnumString};

use crate::symbol::Symbol;

#[derive(EnumString, AsRefStr, Debug, PartialEq)]
#[strum(serialize_all = "lowercase")] // This ensures matching with lowercase strings.
pub enum Keyword {
    True,
    False,
    Var,
    Mut,
    If,
    Else,
    Fun,
    Class,
    Interface,
    Enum,
    Break,
    Continue,
    For,
    In,
    While,
    Pub,
    Impl,
}

pub fn is_keyword(symbol: Symbol) -> bool {
    Keyword::from_str(symbol.as_str()).is_ok()
}

pub fn from_str(s: &str) -> Result<Keyword, strum::ParseError> {
    Keyword::from_str(s)
}
