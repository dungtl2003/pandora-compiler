use errors::PError;

pub mod errors;
pub mod lexer;
pub mod parser;
pub mod util;

pub type PResult<T> = Result<T, PError>;
