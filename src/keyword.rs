use std::str::FromStr;
use strum_macros::{AsRefStr, EnumString};

use crate::symbol::Symbol;

#[derive(EnumString, AsRefStr, Debug, PartialEq)]
#[strum(serialize_all = "lowercase")] // This ensures matching with lowercase strings.
pub enum Keyword {
    True,
    False,
    Set,
    Mut,
    When,
    Alt,
    Fun,
    Br,
    Skip,
    For,
    In,
    During,
    As,
    Const,
    Add,
    Yeet,
}

pub fn to_symbol(keyword: Keyword) -> Symbol {
    Symbol::from(keyword.as_ref())
}

pub fn is_keyword(symbol: Symbol) -> bool {
    Keyword::from_str(symbol.as_str()).is_ok()
}

pub fn from_str(s: &str) -> Result<Keyword, strum::ParseError> {
    Keyword::from_str(s)
}

pub fn to_string(keyword: Keyword) -> String {
    keyword.as_ref().to_string()
}
