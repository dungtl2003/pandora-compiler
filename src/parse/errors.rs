use crate::{ast::Delimiter, span_encoding::Span, ErrorHandler};

use super::parser::TokenType;

#[derive(Debug, Clone)]
pub enum PError {
    ExpectedToken {
        expected: Vec<TokenType>,
        found: TokenType,
        span: Span,
        prev_span: Span,
    },

    ExpectedStatement {
        token: TokenType,
        span: Span,
    },

    ExpectedIdentifier {
        found: TokenType,
        span: Span,
        prev_span: Span,
    },

    MismatchedClosingDelimiter {
        delimiter: Delimiter,
        unmatched_span: Span,
        unclosed_span: Span,
    },

    UnclosedDelimiter {
        unclosed_delimiter_spans: Vec<Span>,
        suggest_close_pos_span: Option<Span>,
    },

    UnexpectedClosingDelimiter {
        delimiter: Delimiter,
        span: Span,
    },
}

impl PError {
    pub fn to_report(self, error_handler: &ErrorHandler) -> miette::Report {
        match self {
            PError::ExpectedToken {
                expected,
                found,
                span,
                prev_span,
            } => error_handler
                .build_expected_token_error(expected, found, span, prev_span)
                .into(),
            PError::ExpectedStatement { token, span } => error_handler
                .build_expected_statement_error(token, span)
                .into(),
            PError::ExpectedIdentifier {
                found,
                span,
                prev_span,
            } => error_handler
                .build_expected_identifier_error(found, span, prev_span)
                .into(),
            PError::MismatchedClosingDelimiter {
                delimiter,
                unmatched_span,
                unclosed_span,
            } => error_handler
                .build_mismatched_closing_delimiter_error(delimiter, unmatched_span, unclosed_span)
                .into(),
            PError::UnclosedDelimiter {
                unclosed_delimiter_spans,
                suggest_close_pos_span,
            } => error_handler
                .build_unclosed_delimiter_error(unclosed_delimiter_spans, suggest_close_pos_span)
                .into(),
            PError::UnexpectedClosingDelimiter { delimiter, span } => error_handler
                .build_unexpected_closing_delimiter_error(delimiter, span)
                .into(),
        }
    }
}
