use crate::error_handler::{ErrorHandler, ERROR_CODE_URL};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

// TODO: Don't know where to write block comment and unknown.
#[derive(Error, Debug, Diagnostic)]
#[error("{msg}")]
#[diagnostic(
    code(E0758),
    url("{}#{}", ERROR_CODE_URL, self.code().unwrap())
)]
struct UnterminatedBlockComment {
    msg: String,
    #[label("...as last nested comment started here, maybe you want to close this instead?")]
    last_start_span: SourceSpan,
    #[label("...and last nested comment terminates here.")]
    last_terminated_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("unknown symbol '{sym}'")]
struct UnknownSymbol {
    sym: String,
    #[label]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("unknown character escape: `{bad_char}`")]
#[diagnostic(
    help("if you meant to write a literal backslash (perhaps escaping in a regular expression), consider a raw string literal"),
)]
struct UnknownCharEscape {
    #[label("unknown character escape")]
    bad_char_span: SourceSpan,

    bad_char: char,
}

#[derive(Error, Debug, Diagnostic)]
#[error("character constant must be escaped: `{escape_char}`")]
struct EscapeOnlyChar {
    escape_char: char,
    escape_char_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("character literal may only contain one codepoint")]
#[diagnostic(help("if you meant to write a string literal, use double quotes"))]
struct MoreThanOneCharLiteral {
    #[label]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("empty character literal")]
struct EmptyCharLiteral {
    #[label("empty character literal")]
    expected_char_span: SourceSpan,
}

impl ErrorHandler {
    pub fn report_unknown_symbol(&self, sym: String, span: SourceSpan) {
        self.emit_err(UnknownSymbol { span, sym });
    }

    pub fn report_unterminated_block_comment(
        &self,
        msg: String,
        last_start_span: SourceSpan,
        last_terminated_span: SourceSpan,
    ) {
        self.emit_err(UnterminatedBlockComment {
            msg,
            last_terminated_span,
            last_start_span,
        });
    }

    pub fn report_empty_char_literal(&self, expected_char_span: SourceSpan) {
        self.emit_err(EmptyCharLiteral { expected_char_span });
    }

    pub fn report_more_than_one_char_literal(&self, span: SourceSpan) {
        self.emit_err(MoreThanOneCharLiteral { span });
    }

    pub fn report_escape_only_char_literal(&self, escape_char: char, escape_char_span: SourceSpan) {
        self.emit_err(EscapeOnlyChar {
            escape_char,
            escape_char_span,
        });
    }

    pub fn report_unknown_char_escape(&self, bad_char: char, bad_char_span: SourceSpan) {
        self.emit_err(UnknownCharEscape {
            bad_char,
            bad_char_span,
        });
    }

    pub fn report_escape_only_char(&self, escape_char: char, escape_char_span: SourceSpan) {
        self.emit_err(EscapeOnlyChar {
            escape_char,
            escape_char_span,
        });
    }
}
