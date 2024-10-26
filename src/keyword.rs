use std::str::FromStr;
use strum_macros::{AsRefStr, EnumString};

#[derive(EnumString, AsRefStr, Debug, PartialEq)]
#[strum(serialize_all = "lowercase")] // This ensures matching with lowercase strings.
pub enum Keyword {
    True,
    False,
    Let,
    When,
    Else,
}

pub fn is_keyword(symbol: &str) -> bool {
    Keyword::from_str(symbol).is_ok()
}

pub fn from_symbol(symbol: &str) -> Result<Keyword, strum::ParseError> {
    Keyword::from_str(symbol)
}
