use crate::{span_encoding::Span, ErrorHandler};
use miette::{Diagnostic, Report};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[error("expected symbol {symbol}")]
pub struct ExpectedSymbol {
    pub span: Span,
    pub symbol: String,
}

impl ErrorHandler {
    pub fn create_expected_symbol_err(&self, span: Span, symbol: String) -> Report {
        self.create_err(ExpectedSymbol { span, symbol })
    }
}
