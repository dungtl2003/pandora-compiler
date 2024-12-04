use crate::{
    ast::{Delimiter, LitKind, TokenKind},
    span_encoding::Span,
    ErrorHandler,
};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use super::parser::TokenType;

#[derive(Error, Debug, Diagnostic)]
pub enum PError {
    #[error("{}", message)]
    ExpectedToken {
        message: String,
        span_msg: String,
        #[label("{}", span_msg)]
        span: SourceSpan,
    },

    #[error("Expected statement, found {}", token)]
    ExpectedStatement {
        token: TokenType,
        #[label("expected statement")]
        span: SourceSpan,
    },

    #[error("{}", message)]
    ExpectedIdentifier {
        message: String,

        #[label("expected identifier")]
        bad_token_span: SourceSpan,

        #[help]
        help_msg: Option<String>,
    },

    #[error("mismatched closing delimiter: `{delimiter}`")]
    MismatchedClosingDelimiter {
        delimiter: String,
        #[label("mismatched closing delimiter")]
        unmatched: SourceSpan,
        #[label("unclosed delimiter")]
        unclosed: SourceSpan,
    },

    #[error("this file contains an unclosed delimiter")]
    UnclosedDelimiter {
        #[label(collection, "unclosed delimiter")]
        unclosed_delimiter_spans: Vec<SourceSpan>,
        #[label("you may want to close here")]
        suggest_close_pos_span: Option<SourceSpan>,
    },

    #[error("unexpected closing delimiter: `{delimiter}`")]
    UnexpectedClosingDelimiter {
        delimiter: String,
        #[label("unexpected closing delimiter")]
        span: SourceSpan,
    },

    #[error("")]
    DUMMY,
}

impl ErrorHandler {
    pub fn build_expected_statement_error(&self, found: TokenType, span: Span) -> PError {
        let span = span.to_source_span();

        PError::ExpectedStatement { token: found, span }
    }

    pub fn build_expected_identifier_error(&self, found: &TokenType, span: Span) -> PError {
        let span = span.to_source_span();

        let help_msg = match found {
            TokenType::Token(tok) => match tok {
                TokenKind::Literal(lit) => match lit.kind {
                    LitKind::Int | LitKind::Float => {
                        Some("use a valid identifier instead of a number".to_string())
                    }
                    LitKind::Bool => None,
                    LitKind::Str | LitKind::Char | LitKind::RawStr(_) | LitKind::Err => {
                        unreachable!("build_expected_identifier_error should not be called with LitKind::Str, LitKind::Char, LitKind::RawStr or LitKind::Err")
                    }
                },
                _ => None,
            },
            TokenType::Keyword(kw) => Some(format!(
                "escape `{0}` to use it as an identifier (e.g., `r#{0}`)",
                kw.as_str()
            )),
            TokenType::Operator => None,
            TokenType::Type | TokenType::Const | TokenType::Ident => {
                unreachable!(
                    "build_expected_identifier_error should not be called with Type, Ident or Const"
                )
            }
        };

        PError::ExpectedIdentifier {
            message: format!("expected identifier, found {}", found),
            help_msg,
            bad_token_span: span,
        }
    }

    // Only for temporary use
    pub fn build_dummy_error(&self) -> PError {
        PError::DUMMY
    }

    pub fn build_unexpected_closing_delimiter(&self, delimiter: Delimiter, span: Span) -> PError {
        let delimiter = match delimiter {
            Delimiter::Brace => "}".to_string(),
            Delimiter::Bracket => "]".to_string(),
            Delimiter::Parenthesis => ")".to_string(),
        };

        PError::UnexpectedClosingDelimiter {
            delimiter,
            span: span.to_source_span(),
        }
    }

    pub fn build_unclosed_delimiter(
        &self,
        unclosed_delimiter_spans: Vec<Span>,
        suggest_close_pos_span: Option<Span>,
    ) -> PError {
        PError::UnclosedDelimiter {
            unclosed_delimiter_spans: unclosed_delimiter_spans
                .iter()
                .map(|s| s.to_source_span())
                .collect(),
            suggest_close_pos_span: suggest_close_pos_span.map(|s| s.to_source_span()),
        }
    }

    pub fn build_mismatched_closing_delimiter(
        &self,
        delimiter: Delimiter,
        unmatched: Span,
        unclosed: Span,
    ) -> PError {
        let delimiter = match delimiter {
            Delimiter::Brace => "}".to_string(),
            Delimiter::Bracket => "]".to_string(),
            Delimiter::Parenthesis => ")".to_string(),
        };
        PError::MismatchedClosingDelimiter {
            delimiter,
            unmatched: unmatched.to_source_span(),
            unclosed: unclosed.to_source_span(),
        }
    }

    pub fn build_expected_token_error(
        &self,
        expected: Vec<TokenType>,
        found: TokenType,
        span: Span,
    ) -> PError {
        let span = span.to_source_span();

        let expected_str = expected
            .iter()
            .map(|t| format!("{}", t))
            .collect::<Vec<String>>()
            .join(", ");
        let found_str = format!("{}", found);

        if expected.len() == 1 {
            PError::ExpectedToken {
                message: format!("expected {}, found {}", expected_str, found_str),
                span_msg: format!("expected {}", expected_str),
                span,
            }
        } else {
            PError::ExpectedToken {
                message: format!("expected one of {}, found {}", expected_str, found_str),
                span_msg: format!("expected one of {} possible tokens", expected.len()),
                span,
            }
        }
    }
}
