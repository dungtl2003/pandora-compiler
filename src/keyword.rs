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
    Br,
    Con,
    For,
    In,
    While,
    As,
    Const,
    Add,
    Yeet,
}

pub fn is_keyword(symbol: Symbol) -> bool {
    Keyword::from_str(symbol.as_str()).is_ok()
}

/// Returns the keyword if the symbol is a keyword, otherwise returns None.
pub fn is_function_keyword(symbol: Symbol) -> Option<Keyword> {
    match Keyword::from_str(symbol.as_str()) {
        Ok(Keyword::Fun) => Some(Keyword::Fun),
        _ => None,
    }
}

/// Returns the keyword if the symbol is a keyword, otherwise returns None.
pub fn is_return_keyword(symbol: Symbol) -> Option<Keyword> {
    match Keyword::from_str(symbol.as_str()) {
        Ok(Keyword::Yeet) => Some(Keyword::Yeet),
        _ => None,
    }
}

/// Returns the keyword if the symbol is a keyword, otherwise returns None.
pub fn is_import_keyword(symbol: Symbol) -> Option<Keyword> {
    match Keyword::from_str(symbol.as_str()) {
        Ok(Keyword::Add) => Some(Keyword::Add),
        _ => None,
    }
}

pub fn from_str(s: &str) -> Result<Keyword, strum::ParseError> {
    Keyword::from_str(s)
}
