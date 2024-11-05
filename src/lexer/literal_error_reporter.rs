use crate::error_handler::{ErrorEmitter, ERROR_CODE_URL};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[error("invalid digit for a base {base} literal")]
struct InvalidDigitLiteral {
    base: u32,
    #[label]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("missing digits after exponent symbol")]
struct EmptyExponentFloat {
    #[label]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("{base} float literal is not supported")]
struct FloatLiteralUnsupportedBase {
    base: String,
    #[label]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("unterminated character literal")]
#[diagnostic(
    code(E0762),
    url("{}#{}", ERROR_CODE_URL, self.code().unwrap())
)]
struct UnterminatedCharLiteral {
    #[label]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("no valid digits found for number")]
struct NoDigitsLiteral {
    #[label]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("too many `#` symbols: raw strings may be delimited by up to 255 `#` symbols, but found {found}")]
struct RawStrTooManyHashes {
    found: u32,
    #[label]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("found invalid character; only `#` is allowed in raw string delimitation: {bad_char}")]
struct RawStrInvalidStarter {
    bad_char: char,
    #[label]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("unterminated raw string")]
#[diagnostic(
    code(E0748),
    url("{}#{}", ERROR_CODE_URL, self.code().unwrap()),
    help("this raw string should be terminated with `\"{}`", "#".repeat(*expected as usize)),
)]
struct RawStrUnterminated {
    start_span: SourceSpan,
    #[label("help: consider terminating the string here: `{}", "#".repeat(*expected as usize))]
    suggest_span: Option<SourceSpan>,
    expected: u32,
}

impl ErrorEmitter {
    pub fn report_raw_str_unterminated(
        &self,
        start_span: SourceSpan,
        expected: u32,
        possible_terminator_span: Option<SourceSpan>,
    ) {
        let suggest_span = if let Some(terminator_span) = possible_terminator_span {
            Some(terminator_span)
        } else {
            None
        };

        self.emit_err(RawStrUnterminated {
            start_span,
            suggest_span,
            expected,
        });
    }

    pub fn report_raw_str_invalid_starter(&self, bad_char: char, span: SourceSpan) {
        self.emit_err(RawStrInvalidStarter { bad_char, span });
    }

    pub fn report_raw_str_too_many_hashes(&self, found: u32, span: SourceSpan) {
        self.emit_err(RawStrTooManyHashes { found, span });
    }

    pub fn report_no_digits_literal(&self, span: SourceSpan) {
        self.emit_err(NoDigitsLiteral { span });
    }

    pub fn report_invalid_digit_literal(&self, base: u32, span: SourceSpan) {
        self.emit_err(InvalidDigitLiteral { base, span });
    }

    pub fn report_empty_exponent_float(&self, span: SourceSpan) {
        self.emit_err(EmptyExponentFloat { span });
    }

    pub fn report_float_literal_unsupported_base(&self, base: String, span: SourceSpan) {
        self.emit_err(FloatLiteralUnsupportedBase { base, span });
    }

    pub fn report_unterminated_character_literal(&self, span: SourceSpan) {
        self.emit_err(UnterminatedCharLiteral { span });
    }
}
