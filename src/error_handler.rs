use crate::{
    ast::{Delimiter, TokenKind},
    parse::parser::TokenType,
    session::SourceFile,
    span_encoding::Span,
};
use miette::{Diagnostic, LabeledSpan, Report, SourceSpan};
use std::sync::Arc;
use thiserror::Error;

pub const ERROR_CODE_URL: &str =
    "https://github.com/dungtl2003/pandora-compiler/tree/main/error_codes";

#[derive(Debug, Clone)]
pub struct ErrorHandler {
    pub file: Arc<SourceFile>,
}

impl ErrorHandler {
    pub fn build_neg_array_size_error(&self, span: Span, size: String) -> NegArraySize {
        NegArraySize {
            size,
            span: span.to_source_span(),
        }
    }

    pub fn build_predefined_error(&self, span: Span, message: String) -> PredefinedError {
        PredefinedError {
            span: span.to_source_span(),
            message,
        }
    }

    pub fn build_divided_by_zero_error(&self, divident: String, span: Span) -> DividedByZero {
        DividedByZero {
            divident,
            span: span.to_source_span(),
        }
    }

    pub fn build_modded_by_zero_error(&self, divident: String, span: Span) -> ModdedByZero {
        ModdedByZero {
            divident,
            span: span.to_source_span(),
        }
    }

    pub fn build_lit_out_of_range_error(&self, span: Span, max: i64) -> LitOutOfRange {
        LitOutOfRange {
            span: span.to_source_span(),
            max: max.to_string(),
        }
    }

    pub fn build_function_in_library_not_found_error(
        &self,
        func_name: String,
        lib_name: String,
        span: Span,
    ) -> FunctionInLibraryNotFound {
        FunctionInLibraryNotFound {
            func_name,
            lib_name,
            span: span.to_source_span(),
        }
    }

    pub fn build_invalid_library_name_error(&self, span: Span) -> InvalidLibraryName {
        InvalidLibraryName {
            span: span.to_source_span(),
        }
    }

    pub fn build_non_function_declared_in_external_library_error(
        &self,
        span: Span,
    ) -> NonFunctionDeclaredInExternalLibrary {
        NonFunctionDeclaredInExternalLibrary {
            span: span.to_source_span(),
        }
    }

    pub fn build_array_has_multiple_types_error(
        &self,
        first_el_ty: String,
        first_mismatch_ty: String,
        first_el_span: Span,
        first_mismatch_span: Span,
    ) -> ArrayHasMultipleTypes {
        ArrayHasMultipleTypes {
            first_el_ty,
            first_mismatch_ty,
            first_el_span: first_el_span.to_source_span(),
            first_mismatch_span: first_mismatch_span.to_source_span(),
        }
    }

    pub fn build_parse_library_file_failed_error(
        &self,
        span: Span,
        path: String,
    ) -> ParseLibraryFileFailed {
        ParseLibraryFileFailed {
            span: span.to_source_span(),
            path,
        }
    }

    pub fn build_read_library_file_failed_error(&self, path: String) -> ReadLibraryFileFailed {
        ReadLibraryFileFailed { path }
    }

    pub fn build_external_library_not_found_error(
        &self,
        lib_name: String,
        span: Span,
    ) -> ExternalLibraryNotFound {
        ExternalLibraryNotFound {
            lib_name,
            span: span.to_source_span(),
        }
    }

    pub fn build_directory_not_found_error(&self) -> DirectoryNotFound {
        DirectoryNotFound()
    }

    pub fn build_no_source_file_specified_error(&self) -> NoSourceFileSpecified {
        NoSourceFileSpecified()
    }

    pub fn build_missing_return_statement_error(
        &self,
        ret_kw: String,
        expected: String,
        expected_span: Span,
        func_decl_span: Span,
    ) -> MissingReturnStatement {
        MissingReturnStatement {
            ret_kw,
            expected,
            expected_span: expected_span.to_source_span(),
            func_decl_span: func_decl_span.to_source_span(),
        }
    }

    pub fn build_missing_function_return_type_error(
        &self,
        found: String,
        ret_help_span: Span,
        found_span: Span,
    ) -> MissingFunctionReturnType {
        MissingFunctionReturnType {
            found,
            ret_help_span: ret_help_span.to_source_span(),
            found_span: found_span.to_source_span(),
        }
    }

    pub fn build_mutate_immutable_variable_error(
        &self,
        mut_kw: String,
        var_name: String,
        first_assign_span: Span,
        second_assign_span: Span,
        help_span: Span,
    ) -> MutateImmutableVariable {
        MutateImmutableVariable {
            mut_kw,
            var_name,
            first_assign_span: first_assign_span.to_source_span(),
            second_assign_span: second_assign_span.to_source_span(),
            help_span: help_span.to_source_span(),
        }
    }

    pub fn build_mismatched_function_return_type(
        &self,
        expected: String,
        found: String,
        expected_span: Span,
        found_span: Span,
    ) -> MismatchedFunctionReturnType {
        MismatchedFunctionReturnType {
            expected,
            found,
            expected_span: expected_span.to_source_span(),
            found_span: found_span.to_source_span(),
        }
    }

    pub fn build_function_param_mismatch(
        &self,
        func_decl_span: Span,
        args: usize,
        mismatch_params: Vec<(Span, String, String)>,
        missing_param_tys: Vec<String>, // this and unexpected_params should never be non empty at the
        // same time
        unexpected_param_tys: Vec<(Span, String)>,
        prefix_span: Span,
    ) -> FunctionParamMismatch {
        let mut missing_params: Vec<String> = missing_param_tys;
        let mut param_labels: Vec<LabeledSpan> = vec![];

        for (span, param_ty, arg_ty) in mismatch_params.iter() {
            param_labels.push(LabeledSpan::at(
                span.to_source_span(),
                format!("expected `{}`, found `{}`", param_ty, arg_ty).to_string(),
            ));
        }

        let (message, missing_span, unexpected_spans) =
            if missing_params.is_empty() && unexpected_param_tys.is_empty() {
                (
                    "arguments to this function are incorrect".to_string(),
                    None,
                    vec![],
                )
            } else if !missing_params.is_empty() {
                let total_params = args + missing_params.len();
                let expected_arg_eng = if total_params == 1 {
                    "1 argument".to_string()
                } else {
                    format!("{} arguments", total_params)
                };
                let found_arg_eng = if args == 1 {
                    "1 argument".to_string()
                } else {
                    format!("{} arguments", args)
                };

                let missing_msg: String = if missing_params.len() == 1 {
                    format!("an argument of type `{}` is missing", missing_params[0])
                } else {
                    let args_eng = english_numbers::convert_all_fmt(missing_params.len() as i64);
                    let last_ty = missing_params.pop().unwrap();
                    let tys_eng = [missing_params.join(", "), last_ty].join(" and ");
                    format!("{} arguments of type {} are missing", args_eng, tys_eng)
                };

                (
                    format!(
                        "this function takes {} but {} were supplied",
                        expected_arg_eng, found_arg_eng,
                    ),
                    Some(LabeledSpan::at(
                        prefix_span.to_source_span(),
                        missing_msg.to_string(),
                    )),
                    vec![],
                )
            } else {
                let total_params = args - unexpected_param_tys.len();
                let expected_arg_eng = if total_params == 1 {
                    "1 argument".to_string()
                } else {
                    format!("{} arguments", total_params)
                };
                let found_arg_eng = if args == 1 {
                    "1 argument".to_string()
                } else {
                    format!("{} arguments", args)
                };

                let labels: Vec<LabeledSpan> = unexpected_param_tys
                    .into_iter()
                    .map(|(span, ty)| {
                        LabeledSpan::at(
                            span.to_source_span(),
                            format!("unexpected argument of type `{}`", ty),
                        )
                    })
                    .collect();

                (
                    format!(
                        "this function takes {} but {} were supplied",
                        expected_arg_eng, found_arg_eng,
                    ),
                    None,
                    labels,
                )
            };

        FunctionParamMismatch {
            message,
            missing_span,
            func_decl_span: func_decl_span.to_source_span(),
            mismatch_spans: param_labels,
            unexpected_spans,
        }
    }
    pub fn build_function_not_in_scope_error(
        &self,
        function: String,
        span: Span,
    ) -> FunctionNotInScope {
        FunctionNotInScope {
            function,
            span: span.to_source_span(),
        }
    }
    pub fn build_invalid_function_call_error(&self, span: Span) -> InvalidFunctionCall {
        InvalidFunctionCall {
            span: span.to_source_span(),
        }
    }
    pub fn build_invalid_library_path_error(&self, span: Span) -> InvalidLibraryPath {
        InvalidLibraryPath {
            span: span.to_source_span(),
        }
    }

    pub fn build_library_not_found_error(&self, library: String, span: Span) -> LibraryNotFound {
        LibraryNotFound {
            library,
            span: span.to_source_span(),
        }
    }
    pub fn build_indexing_wrong_type_error(&self, ty: String, span: Span) -> IndexingWrongType {
        IndexingWrongType {
            ty,
            span: span.to_source_span(),
        }
    }
    pub fn build_index_out_of_bounds_error(
        &self,
        len: i64,
        index: i64,
        span: Span,
    ) -> IndexOutOfBounds {
        IndexOutOfBounds {
            len,
            index,
            span: span.to_source_span(),
        }
    }
    pub fn build_neg_repeat_count_error(&self, count: i64, span: Span) -> NegRepeatCount {
        NegRepeatCount {
            count,
            span: span.to_source_span(),
        }
    }
    pub fn build_invalid_lhs_assign_error(
        &self,
        assign_span: Span,
        lhs_span: Span,
    ) -> InvalidLhsAssign {
        InvalidLhsAssign {
            assign_span: assign_span.to_source_span(),
            lhs_span: lhs_span.to_source_span(),
        }
    }
    pub fn build_cannot_apply_unary_op_error(
        &self,
        op: String,
        ty: String,
        span: Span,
    ) -> CannotApplyUnaryOp {
        CannotApplyUnaryOp {
            op,
            ty,
            span: span.to_source_span(),
        }
    }
    pub fn build_cannot_cast_error(&self, from: String, to: String, span: Span) -> CannotCast {
        CannotCast {
            from,
            to,
            span: span.to_source_span(),
        }
    }
    pub fn build_mismatched_type_error(
        &self,
        expected: String,
        found: String,
        span: Span,
    ) -> MismatchedType {
        MismatchedType {
            expected,
            found,
            span: span.to_source_span(),
        }
    }

    pub fn build_cannot_find_type_in_scope_error(
        &self,
        type_name: String,
        span: Span,
    ) -> CannotFindTypeInScope {
        CannotFindTypeInScope {
            type_name,
            span: span.to_source_span(),
        }
    }

    pub fn build_var_is_not_initialized_error(
        &self,
        var_name: String,
        declared_span: Span,
        used_span: Span,
    ) -> VarIsNotInitialized {
        VarIsNotInitialized {
            var_name,
            declared_span: declared_span.to_source_span(),
            used_span: used_span.to_source_span(),
        }
    }

    pub fn build_cannot_find_variable_in_scope_error(
        &self,
        var_name: String,
        span: Span,
    ) -> CannotFindVariableInScope {
        CannotFindVariableInScope {
            var_name,
            span: span.to_source_span(),
        }
    }

    pub fn build_no_implementation_for_op_error(
        &self,
        lhs_ty: String,
        rhs_ty: String,
        op: String,
        op_span: Span,
    ) -> NoImplForOp {
        NoImplForOp {
            lhs_ty,
            rhs_ty,
            op,
            op_span: op_span.to_source_span(),
        }
    }

    pub fn build_cannot_compare_error(
        &self,
        lhs_ty: String,
        rhs_ty: String,
        op: String,
        op_span: Span,
    ) -> CannotCompare {
        CannotCompare {
            lhs_ty,
            rhs_ty,
            op,
            op_span: op_span.to_source_span(),
        }
    }

    pub fn build_cannot_divide_error(
        &self,
        lhs_ty: String,
        rhs_ty: String,
        op_span: Span,
    ) -> CannotDivide {
        CannotDivide {
            lhs_ty,
            rhs_ty,
            op_span: op_span.to_source_span(),
        }
    }

    pub fn build_cannot_multiply_error(
        &self,
        lhs_ty: String,
        rhs_ty: String,
        op_span: Span,
    ) -> CannotMultiply {
        CannotMultiply {
            lhs_ty,
            rhs_ty,
            op_span: op_span.to_source_span(),
        }
    }

    pub fn build_cannot_subtract_error(
        &self,
        lhs_ty: String,
        rhs_ty: String,
        op_span: Span,
    ) -> CannotSubtract {
        CannotSubtract {
            lhs_ty,
            rhs_ty,
            op_span: op_span.to_source_span(),
        }
    }

    pub fn build_cannot_modulo_error(
        &self,
        lhs_ty: String,
        rhs_ty: String,
        op_span: Span,
    ) -> CannotModulo {
        CannotModulo {
            lhs_ty,
            rhs_ty,
            op_span: op_span.to_source_span(),
        }
    }

    pub fn build_cannot_add_error(
        &self,
        lhs_ty: String,
        rhs_ty: String,
        op_span: Span,
    ) -> CannotAdd {
        CannotAdd {
            lhs_ty,
            rhs_ty,
            op_span: op_span.to_source_span(),
        }
    }

    pub fn build_comparison_operators_cannot_be_chained_error(
        &self,
        chain_op_span: Vec<Span>,
    ) -> ComparisonOperatorsCannotBeChained {
        let chain_op_span = chain_op_span
            .into_iter()
            .map(|span| span.to_source_span())
            .collect();
        ComparisonOperatorsCannotBeChained { chain_op_span }
    }

    pub fn build_multiple_libraries_in_scope_error(
        &self,
        lib_name: String,
        first_lib_span: Span,
        second_lib_span: Span,
    ) -> MultipleLibrariesInScope {
        MultipleLibrariesInScope {
            lib_name,
            first_lib_span: first_lib_span.to_source_span(),
            second_lib_span: second_lib_span.to_source_span(),
        }
    }

    pub fn build_function_already_declared_in_scope_error(
        &self,
        func_name: String,
        first_decl_span: Span,
        second_decl_span: Span,
    ) -> FunctionAlreadyDeclaredInScope {
        FunctionAlreadyDeclaredInScope {
            func_name,
            first_decl_span: first_decl_span.to_source_span(),
            second_decl_span: second_decl_span.to_source_span(),
        }
    }

    pub fn build_expected_iterator_error(&self, ty: String, span: Span) -> ExpectedIterator {
        ExpectedIterator {
            ty,
            span: span.to_source_span(),
        }
    }

    pub fn build_continue_outside_loop_error(
        &self,
        symbol: String,
        span: Span,
    ) -> ContinueOutsideLoop {
        ContinueOutsideLoop {
            symbol,
            span: span.to_source_span(),
        }
    }

    pub fn build_break_outside_loop_error(&self, symbol: String, span: Span) -> BreakOutsideLoop {
        BreakOutsideLoop {
            symbol,
            span: span.to_source_span(),
        }
    }

    pub fn build_return_outside_function_error(
        &self,
        symbol: String,
        span: Span,
    ) -> ReturnOutsideFunction {
        ReturnOutsideFunction {
            symbol,
            span: span.to_source_span(),
        }
    }

    pub fn build_expected_block_error(&self, stmt_span: Span) -> ExpectedBlock {
        ExpectedBlock {
            stmt_span: stmt_span.to_source_span(),
        }
    }

    pub fn build_mismatch_array_type_length_error(
        &self,
        expected_len: i64,
        found_len: i64,
        value_span: Span,
        ty_span: Span,
    ) -> MismatchArrayTypeLength {
        let expected_msg = if expected_len == 1 {
            "1 element".to_string()
        } else {
            format!("{} elements", expected_len)
        };
        let found_msg = if found_len == 1 {
            "1 element".to_string()
        } else {
            format!("{} elements", found_len)
        };

        let value_span = LabeledSpan::at(
            value_span.to_source_span(),
            format!(
                "expected an array with a fixed size of {}, found one with {}",
                expected_msg, found_msg
            ),
        );

        MismatchArrayTypeLength {
            expected_len,
            found_len,
            value_span,
            ty_span: ty_span.to_source_span(),
        }
    }

    pub fn build_unknown_size_array_error(&self, ty: String, span: Span) -> UnknownSizeArray {
        UnknownSizeArray {
            ty,
            span: span.to_source_span(),
        }
    }

    pub fn build_expected_block_after_condition_error(
        &self,
        if_symbol: String,
        condition_span: Span,
        stmt_span: Span,
    ) -> ExpectedBlockAfterCondition {
        ExpectedBlockAfterCondition {
            if_symbol,
            condition_span: condition_span.to_source_span(),
            stmt_span: stmt_span.to_source_span(),
        }
    }

    pub fn build_expected_statement_error(
        &self,
        found: TokenType,
        span: Span,
    ) -> ExpectedStatement {
        let span = span.to_source_span();

        ExpectedStatement {
            token: found.to_string(),
            span,
        }
    }

    pub fn build_expected_identifier_error(
        &self,
        found: TokenType,
        span: Span,
        prev_span: Span,
    ) -> ExpectedIdentifier {
        // If the found token is an EOF token, we want to display the error at the end of the file
        let span = match found {
            TokenType::Token(tok) => match tok {
                TokenKind::Eof => Span::new(prev_span.end() - 1, prev_span.end()),
                _ => span,
            },
            _ => span,
        }
        .to_source_span();

        let help_msg = match found {
            TokenType::Token(_) | TokenType::Operator => None,
            TokenType::Keyword(kw) => Some(format!(
                "escape `{0}` to use it as an identifier (e.g., `r#{0}`)",
                kw.as_str()
            )),
            TokenType::Const | TokenType::Ident => {
                unreachable!(
                    "build_expected_identifier_error should not be called with Ident or Const"
                )
            }
        };

        ExpectedIdentifier {
            message: format!("expected identifier, found {}", found),
            help_msg,
            bad_token_span: span,
        }
    }

    pub fn build_unexpected_closing_delimiter_error(
        &self,
        delimiter: Delimiter,
        span: Span,
    ) -> UnexpectedClosingDelimiter {
        let delimiter = match delimiter {
            Delimiter::Brace => "}".to_string(),
            Delimiter::Bracket => "]".to_string(),
            Delimiter::Parenthesis => ")".to_string(),
        };

        UnexpectedClosingDelimiter {
            delimiter,
            span: span.to_source_span(),
        }
    }

    pub fn build_unclosed_delimiter_error(
        &self,
        unclosed_delimiter_spans: Vec<Span>,
        suggest_close_pos_span: Option<Span>,
    ) -> UnclosedDelimiter {
        UnclosedDelimiter {
            unclosed_delimiter_spans: unclosed_delimiter_spans
                .iter()
                .map(|s| s.to_source_span())
                .collect(),
            suggest_close_pos_span: suggest_close_pos_span.map(|s| s.to_source_span()),
        }
    }

    pub fn build_mismatched_closing_delimiter_error(
        &self,
        delimiter: Delimiter,
        unmatched_span: Span,
        unclosed_span: Span,
    ) -> MismatchedClosingDelimiter {
        let delimiter = match delimiter {
            Delimiter::Brace => "}".to_string(),
            Delimiter::Bracket => "]".to_string(),
            Delimiter::Parenthesis => ")".to_string(),
        };
        MismatchedClosingDelimiter {
            delimiter,
            unmatched_span: unmatched_span.to_source_span(),
            unclosed_span: unclosed_span.to_source_span(),
        }
    }

    pub fn build_expected_token_error(
        &self,
        expected: Vec<TokenType>,
        found: TokenType,
        span: Span,
        prev_span: Span,
    ) -> ExpectedToken {
        // If the found token is an EOF token, we want to display the error at the end of the file
        let span = match found {
            TokenType::Token(tok) => match tok {
                TokenKind::Eof => Span::new(prev_span.end() - 1, prev_span.end()),
                _ => span,
            },
            _ => span,
        }
        .to_source_span();

        let expected_str = expected
            .iter()
            .map(|t| format!("{}", t))
            .collect::<Vec<String>>()
            .join(", ");
        let found_str = format!("{}", found);

        if expected.len() == 1 {
            ExpectedToken {
                message: format!("expected {}, found {}", expected_str, found_str),
                span_msg: format!("expected {}", expected_str),
                span,
            }
        } else {
            ExpectedToken {
                message: format!("expected one of {}, found {}", expected_str, found_str),
                span_msg: format!("expected one of {} possible tokens", expected.len()),
                span,
            }
        }
    }

    pub fn build_unknown_symbol_error(&self, sym: String, span: Span) -> UnknownSymbol {
        UnknownSymbol {
            span: span.to_source_span(),
            sym,
        }
    }

    pub fn build_unterminated_block_comment_error(
        &self,
        msg: String,
        last_start_span: Option<Span>,
        last_terminated_span: Option<Span>,
        start_span: Option<Span>,
    ) -> UnterminatedBlockComment {
        UnterminatedBlockComment {
            msg,
            last_terminated_span: last_terminated_span.map(|s| s.to_source_span()),
            last_start_span: last_start_span.map(|s| s.to_source_span()),
            start_span: start_span.map(|s| s.to_source_span()),
        }
    }

    pub fn build_empty_char_literal_error(&self, expected_char_span: Span) -> EmptyCharLiteral {
        EmptyCharLiteral {
            expected_char_span: expected_char_span.to_source_span(),
        }
    }

    pub fn build_more_than_one_char_literal_error(&self, span: Span) -> MoreThanOneCharLiteral {
        MoreThanOneCharLiteral {
            span: span.to_source_span(),
        }
    }

    pub fn build_unknown_char_escape_error(
        &self,
        bad_char: String,
        bad_char_span: Span,
    ) -> UnknownCharEscape {
        UnknownCharEscape {
            bad_char,
            bad_char_span: bad_char_span.to_source_span(),
        }
    }

    pub fn build_escape_only_char_error(
        &self,
        escape_char: String,
        escape_char_span: Span,
    ) -> EscapeOnlyChar {
        EscapeOnlyChar {
            escape_char,
            escape_char_span: escape_char_span.to_source_span(),
        }
    }

    pub fn build_raw_str_unterminated_error(
        &self,
        start_span: Span,
        expected: u32,
        possible_terminator_span: Option<Span>,
    ) -> RawStrUnterminated {
        let suggest_span = if let Some(terminator_span) = possible_terminator_span {
            Some(terminator_span.to_source_span())
        } else {
            None
        };

        RawStrUnterminated {
            start_span: start_span.to_source_span(),
            suggest_span,
            expected,
        }
    }

    pub fn build_raw_str_invalid_starter_error(
        &self,
        bad_char: char,
        span: Span,
    ) -> RawStrInvalidStarter {
        RawStrInvalidStarter {
            bad_char,
            span: span.to_source_span(),
        }
    }

    pub fn build_raw_str_too_many_hashes_error(
        &self,
        found: u32,
        span: Span,
    ) -> RawStrTooManyHashes {
        RawStrTooManyHashes {
            found,
            span: span.to_source_span(),
        }
    }

    pub fn build_no_digits_literal_error(&self, span: Span) -> NoDigitsLiteral {
        NoDigitsLiteral {
            span: span.to_source_span(),
        }
    }

    pub fn build_invalid_digit_literal_error(&self, base: u32, span: Span) -> InvalidDigitLiteral {
        InvalidDigitLiteral {
            base,
            span: span.to_source_span(),
        }
    }

    pub fn build_empty_exponent_float_error(&self, span: Span) -> EmptyExponentFloat {
        EmptyExponentFloat {
            span: span.to_source_span(),
        }
    }

    pub fn build_float_literal_unsupported_base_error(
        &self,
        base: String,
        span: Span,
    ) -> FloatLiteralUnsupportedBase {
        FloatLiteralUnsupportedBase {
            base,
            span: span.to_source_span(),
        }
    }

    pub fn build_unterminated_character_literal_error(
        &self,
        span: Span,
    ) -> UnterminatedCharLiteral {
        UnterminatedCharLiteral {
            span: span.to_source_span(),
        }
    }

    pub fn build_unterminated_string_literal_error(
        &self,
        span: Span,
    ) -> UnterminatedDoubleQuoteString {
        UnterminatedDoubleQuoteString {
            span: span.to_source_span(),
        }
    }

    pub fn new(file: Arc<SourceFile>) -> Self {
        Self { file }
    }

    pub fn report_err(&self, build: Report) {
        println!("{:?}", build.with_source_code(Arc::clone(&self.file)));
    }
}

// ========================== LEXER ==========================
#[derive(Error, Debug, Diagnostic)]
#[error("unterminated double quote string")]
#[diagnostic(
    code(E0001),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap())
)]
pub struct UnterminatedDoubleQuoteString {
    #[label(primary)]
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("invalid digit for a base {base} literal")]
#[diagnostic(
    code(E0003),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap())
)]
pub struct InvalidDigitLiteral {
    pub base: u32,
    #[label]
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("expected at least one digit in exponent")]
#[diagnostic(
    code(E0004),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap())
)]
pub struct EmptyExponentFloat {
    #[label]
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("{base} float literal is not supported")]
#[diagnostic(
    code(E0005),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap())
)]
pub struct FloatLiteralUnsupportedBase {
    pub base: String,
    #[label]
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("unterminated character literal")]
#[diagnostic(
    code(E0006),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap())
)]
pub struct UnterminatedCharLiteral {
    #[label("needs a closing `'`")]
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("no valid digits found for number")]
#[diagnostic(
    code(E0002),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap())
)]
pub struct NoDigitsLiteral {
    #[label]
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("too many `#` symbols: raw strings may be delimited by up to 255 `#` symbols, but found {found}")]
#[diagnostic(
    code(E0007),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap())
)]
pub struct RawStrTooManyHashes {
    pub found: u32,
    #[label]
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("found invalid character; only `#` is allowed in raw string delimitation: {bad_char}")]
#[diagnostic(
    code(E0008),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap())
)]
pub struct RawStrInvalidStarter {
    pub bad_char: char,
    #[label]
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("unterminated raw string")]
#[diagnostic(
    code(E0009),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
    help("this raw string should be terminated with `\"{}`", "#".repeat(*expected as usize)),
)]
pub struct RawStrUnterminated {
    #[label("unterminated raw string")]
    pub start_span: SourceSpan,
    #[label("help: consider terminating the string here: `{}", "#".repeat(*expected as usize))]
    pub suggest_span: Option<SourceSpan>,
    pub expected: u32,
}

#[derive(Error, Debug, Diagnostic)]
#[error("{msg}")]
#[diagnostic(
    code(E0010),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct UnterminatedBlockComment {
    pub msg: String,
    #[label("...as last nested comment started here, maybe you want to close this instead?")]
    pub last_start_span: Option<SourceSpan>,
    #[label("...and last nested comment terminates here.")]
    pub last_terminated_span: Option<SourceSpan>,
    #[label]
    pub start_span: Option<SourceSpan>, // used when there is only opening comment
}

#[derive(Error, Debug, Diagnostic)]
#[error("unknown symbol '{sym}'")]
#[diagnostic(
    code(E0011),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct UnknownSymbol {
    pub sym: String,
    #[label]
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("unknown character escape: `{bad_char}`")]
#[diagnostic(
    code(E0012),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
    help("if you meant to write a literal backslash (perhaps escaping in a regular expression), consider a raw string literal"),
)]
pub struct UnknownCharEscape {
    #[label("unknown character escape")]
    pub bad_char_span: SourceSpan,

    pub bad_char: String,
}

#[derive(Error, Debug, Diagnostic)]
#[error("character constant must be escaped: `{escape_char}`")]
#[diagnostic(
    code(E0013),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct EscapeOnlyChar {
    pub escape_char: String,
    #[label("help: escape the character: `{}`", escape_char)]
    pub escape_char_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("character literal may only contain one codepoint")]
#[diagnostic(
    code(E0014),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
    help("if you meant to write a string literal, use double quotes")
)]
pub struct MoreThanOneCharLiteral {
    #[label]
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("empty character literal")]
#[diagnostic(
    code(E0015),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct EmptyCharLiteral {
    #[label("empty character literal")]
    pub expected_char_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("mismatched closing delimiter: `{delimiter}`")]
#[diagnostic(
    code(E0016),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct MismatchedClosingDelimiter {
    delimiter: String,
    #[label("mismatched closing delimiter")]
    unmatched_span: SourceSpan,
    #[label("unclosed delimiter")]
    unclosed_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("this file contains an unclosed delimiter")]
#[diagnostic(
    code(E0017),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct UnclosedDelimiter {
    #[label(collection, "unclosed delimiter")]
    unclosed_delimiter_spans: Vec<SourceSpan>,
    #[label("you may want to close here")]
    suggest_close_pos_span: Option<SourceSpan>,
}

#[derive(Error, Debug, Diagnostic)]
#[error("unexpected closing delimiter: `{delimiter}`")]
#[diagnostic(
    code(E0018),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct UnexpectedClosingDelimiter {
    delimiter: String,
    #[label("unexpected closing delimiter")]
    span: SourceSpan,
}

// ========================== PARSER ==========================
#[derive(Error, Debug, Diagnostic)]
#[error("{}", message)]
#[diagnostic(
    code(E0100),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct ExpectedToken {
    message: String,
    span_msg: String,
    #[label("{}", span_msg)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("expected statement, found `{}`", token)]
#[diagnostic(
    code(E0101),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct ExpectedStatement {
    token: String,
    #[label("expected statement")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("{}", message)]
#[diagnostic(
    code(E0102),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct ExpectedIdentifier {
    message: String,

    #[label("expected identifier")]
    bad_token_span: SourceSpan,

    #[help]
    help_msg: Option<String>,
}

// ========================== INTERPRET ==========================
#[derive(Error, Debug, Diagnostic)]
#[error("expected `{{`")]
#[diagnostic(
    code(E0200),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct ExpectedBlockAfterCondition {
    if_symbol: String,
    #[label(
        "the `{}` expression is missing a block after this condition",
        if_symbol
    )]
    condition_span: SourceSpan,
    #[label("help: try placing this code inside a block")]
    stmt_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("the size for values of type `{}` cannot be known", ty)]
#[diagnostic(
    code(E0201),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct UnknownSizeArray {
    ty: String,
    #[label("doesn't have a size known")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("mismatched types")]
#[diagnostic(
    code(E0202),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
    help("consider specifying the actual array length: `{}`", found_len)
)]
pub struct MismatchArrayTypeLength {
    expected_len: i64,
    found_len: i64,
    value_span: LabeledSpan,
    #[label("expected due to this")]
    ty_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("`{}` outside of a function", symbol)]
#[diagnostic(
    code(E0203),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct ReturnOutsideFunction {
    symbol: String,
    #[label("cannot `{}` outside of a function", symbol)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("`{}` outside of a loop", symbol)]
#[diagnostic(
    code(E0204),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct ContinueOutsideLoop {
    symbol: String,
    #[label("cannot `{}` outside of a loop", symbol)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("`{}` outside of a loop", symbol)]
#[diagnostic(
    code(E0205),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct BreakOutsideLoop {
    symbol: String,
    #[label("cannot `{}` outside of a loop", symbol)]
    span: SourceSpan,
}
#[derive(Error, Debug, Diagnostic)]
#[error("`{}` is not an iterator", ty)]
#[diagnostic(
    code(E0206),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct ExpectedIterator {
    ty: String,
    #[label("`{}` is not an iterator", ty)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("Expected `{{`")]
#[diagnostic(
    code(E0207),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct ExpectedBlock {
    #[label("help: try placing this code inside a block")]
    stmt_span: SourceSpan,
}
#[derive(Error, Debug, Diagnostic)]
#[error("Function with name `{}` already exists in this scope", func_name)]
#[diagnostic(
    code(E0208),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct FunctionAlreadyDeclaredInScope {
    func_name: String,
    #[label("first declared here")]
    first_decl_span: SourceSpan,
    #[label("help: remove this")]
    second_decl_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error(
    "there are multiple libraries with the same name `{}` in the same scope",
    lib_name
)]
#[diagnostic(
    code(E0209),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct MultipleLibrariesInScope {
    lib_name: String,
    #[label("first added here")]
    first_lib_span: SourceSpan,
    #[label("help: remove this")]
    second_lib_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("comparison operators cannot be chained")]
#[diagnostic(
    code(E0218),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
    help("split the comparison into two")
)]
pub struct ComparisonOperatorsCannotBeChained {
    #[label(collection)]
    chain_op_span: Vec<SourceSpan>,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot add `{}` to `{}`", rhs_ty, lhs_ty)]
#[diagnostic(
    code(E0219),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct CannotAdd {
    lhs_ty: String,
    rhs_ty: String,
    #[label("no implementation for `{} + {}`", lhs_ty, rhs_ty)]
    op_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot subtract `{}` from `{}`", rhs_ty, lhs_ty)]
#[diagnostic(
    code(E0220),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct CannotSubtract {
    lhs_ty: String,
    rhs_ty: String,
    #[label("no implementation for `{} - {}`", lhs_ty, rhs_ty)]
    op_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot multiply `{}` by `{}`", lhs_ty, rhs_ty)]
#[diagnostic(
    code(E0221),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct CannotMultiply {
    lhs_ty: String,
    rhs_ty: String,
    #[label("no implementation for `{} * {}`", lhs_ty, rhs_ty)]
    op_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot divide `{}` by `{}`", lhs_ty, rhs_ty)]
#[diagnostic(
    code(E0222),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct CannotDivide {
    lhs_ty: String,
    rhs_ty: String,
    #[label("no implementation for `{} / {}`", lhs_ty, rhs_ty)]
    op_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error(
    "cannot calculate the remainder of `{}` divided by `{}`",
    lhs_ty,
    rhs_ty
)]
#[diagnostic(
    code(E0223),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct CannotModulo {
    lhs_ty: String,
    rhs_ty: String,
    #[label("no implementation for `{} % {}`", lhs_ty, rhs_ty)]
    op_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("array has multiple types")]
#[diagnostic(
    code(E0250),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct ArrayHasMultipleTypes {
    pub first_el_ty: String,
    pub first_mismatch_ty: String,
    #[label("first element has type `{}`", first_el_ty)]
    pub first_el_span: SourceSpan,
    #[label("but this element has type `{}`", first_mismatch_ty)]
    pub first_mismatch_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("can't compare `{}` with `{}`", lhs_ty, rhs_ty)]
#[diagnostic(
    code(E0224),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct CannotCompare {
    lhs_ty: String,
    rhs_ty: String,
    op: String,
    #[label("no implementation for `{} {} {}`", lhs_ty, op, rhs_ty)]
    op_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("no implementation for `{} {} {}`", lhs_ty, op, rhs_ty)]
#[diagnostic(
    code(E0225),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct NoImplForOp {
    lhs_ty: String,
    rhs_ty: String,
    op: String,
    #[label("no implementation for `{} {} {}`", lhs_ty, op, rhs_ty)]
    op_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot find variable `{}` in this scope", var_name)]
#[diagnostic(
    code(E0226),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct CannotFindVariableInScope {
    var_name: String,
    #[label("not found in this scope")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot find type `{}` in this scope", type_name)]
#[diagnostic(
    code(E0227),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct CannotFindTypeInScope {
    type_name: String,
    #[label("not found in this scope")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("used binding `{}` isn't initialized", var_name)]
#[diagnostic(
    code(E0228),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
    help = "consider assigning a value"
)]
pub struct VarIsNotInitialized {
    var_name: String,
    #[label("binding declared here but left uninitialized")]
    declared_span: SourceSpan,
    #[label("`{}` used here but it isn't initialized", var_name)]
    used_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot cast `{}` as `{}`", from, to)]
#[diagnostic(
    code(E0229),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct CannotCast {
    from: String,
    to: String,
    #[label("this has type `{}`", from)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("mismatched types")]
#[diagnostic(
    code(E0230),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct MismatchedType {
    expected: String,
    found: String,
    #[label("expected `{}`, found `{}`", expected, found)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot apply unary operator `{}` to type `{}`", op, ty)]
#[diagnostic(
    code(E0231),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct CannotApplyUnaryOp {
    op: String,
    ty: String,
    #[label("cannot apply unary operator `{}`", op)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("invalid left-hand side of assignment")]
#[diagnostic(
    code(E0232),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct InvalidLhsAssign {
    #[label(primary)]
    assign_span: SourceSpan,
    #[label("cannot assign to this expression")]
    lhs_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("Repeat count must be non-negative, found `{}`", count)]
#[diagnostic(
    code(E0233),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct NegRepeatCount {
    count: i64,
    #[label("expected non-negative repeat count")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error(
    "index out of bounds: the len is `{}` but the index is `{}`",
    len,
    index
)]
#[diagnostic(
    code(E0234),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct IndexOutOfBounds {
    index: i64,
    len: i64,
    #[label("index out of bounds")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot index into a value of type `{}`", ty)]
#[diagnostic(
    code(E0235),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct IndexingWrongType {
    #[label(primary)]
    span: SourceSpan,
    ty: String,
}

#[derive(Error, Debug, Diagnostic)]
#[error("library `{}` not found", library)]
#[diagnostic(
    code(E0236),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct LibraryNotFound {
    library: String,
    #[label("library not found")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("invalid library path")]
#[diagnostic(
    code(E0237),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct InvalidLibraryPath {
    #[label("This should be an identifier")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("invalid function call")]
#[diagnostic(
    code(E0238),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct InvalidFunctionCall {
    #[label(primary)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot find function `{}` in this scope", function)]
#[diagnostic(
    code(E0239),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct FunctionNotInScope {
    function: String,
    #[label("not found in this scope")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("{}", message)]
#[diagnostic(
    code(E0240),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct FunctionParamMismatch {
    message: String,
    #[label("function defined here")]
    func_decl_span: SourceSpan,
    #[label(collection)]
    mismatch_spans: Vec<LabeledSpan>,
    #[label(collection)]
    missing_span: Option<LabeledSpan>,
    #[label(collection)]
    unexpected_spans: Vec<LabeledSpan>,
}

#[derive(Error, Debug, Diagnostic)]
#[error("mismatched types")]
#[diagnostic(
    code(E0241),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct MismatchedFunctionReturnType {
    expected: String,
    found: String,
    #[label("expected `{}` because of return type", expected)]
    expected_span: SourceSpan,
    #[label("expected `{}`, found `{}`", expected, found)]
    found_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("mismatched types")]
#[diagnostic(
    code(E0242),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct MissingFunctionReturnType {
    found: String,
    #[label("help: try adding a return type: `-> {}`", found)]
    ret_help_span: SourceSpan,
    #[label("expected `()`, found `{}`", found)]
    found_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("mismatched types")]
#[diagnostic(
    code(E0243),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct MissingReturnStatement {
    ret_kw: String,
    expected: String,
    #[label("expected `{}`, found `()`", expected)]
    expected_span: SourceSpan,
    #[label(
        "implicitly returns `()` as its body has no tail or `{}` expression",
        ret_kw
    )]
    func_decl_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot assign twice to immutable variable `{}`", var_name)]
#[diagnostic(
    code(E0244),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct MutateImmutableVariable {
    mut_kw: String,
    var_name: String,
    #[label("first assignment to `{}`", var_name)]
    first_assign_span: SourceSpan,
    #[label("cannot assign twice to immutable variable")]
    second_assign_span: SourceSpan,
    #[label(
        "help: consider making this binding mutable: `{} {}`",
        mut_kw,
        var_name
    )]
    help_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("no source file specified")]
#[diagnostic(
    code(E0210),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct NoSourceFileSpecified();

#[derive(Error, Debug, Diagnostic)]
#[error("could not determine source file directory")]
#[diagnostic(
    code(E0211),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct DirectoryNotFound();

#[derive(Error, Debug, Diagnostic)]
#[error("external library `{}` not found", lib_name)]
#[diagnostic(
    code(E0212),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct ExternalLibraryNotFound {
    lib_name: String,
    #[label("library not found")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("failed to read library file `{}`", path)]
#[diagnostic(
    code(E0213),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct ReadLibraryFileFailed {
    path: String,
}

#[derive(Error, Debug, Diagnostic)]
#[error("failed to parse library file `{}`", path)]
#[diagnostic(
    code(E0214),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct ParseLibraryFileFailed {
    #[label("failed to parse library file")]
    span: SourceSpan,
    path: String,
}

#[derive(Error, Debug, Diagnostic)]
#[error("only function declarations are allowed in external libraries")]
#[diagnostic(
    code(E0215),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct NonFunctionDeclaredInExternalLibrary {
    #[label("this library contains a non-function declaration")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("invalid library name")]
#[diagnostic(
    code(E0216),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct InvalidLibraryName {
    #[label(primary)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("function `{}` not found in library `{}`", func_name, lib_name)]
#[diagnostic(
    code(E0217),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct FunctionInLibraryNotFound {
    lib_name: String,
    func_name: String,
    #[label("function not found")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot divide by zero")]
#[diagnostic(
    code(E0245),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct DividedByZero {
    pub divident: String,
    #[label("attempt to divide `{}` by zero", divident)]
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot calculate the remainder of `{}` divided by zero", divident)]
#[diagnostic(
    code(E0246),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct ModdedByZero {
    pub divident: String,
    #[label(
        "attempt to calculate the remainder of `{}` with a divisor of zero",
        divident
    )]
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("integer literal is too large")]
#[diagnostic(
    code(E0247),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct LitOutOfRange {
    pub max: String,
    #[label("note: value exceeds limit of `{}`", max)]
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("Array size must be non-negative, found `{}`", size)]
#[diagnostic(
    code(E0249),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct NegArraySize {
    pub size: String,
    #[label("expected non-negative size")]
    pub span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("{}", message)]
#[diagnostic(
    code(E0248),
    url("{}/{}.md", ERROR_CODE_URL, self.code().unwrap()),
)]
pub struct PredefinedError {
    #[label(primary)]
    pub span: SourceSpan,
    pub message: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ErrorType {
    Recoverable,
    Unrecoverable,
    NoError,
}
