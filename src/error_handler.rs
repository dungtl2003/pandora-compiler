use crate::{session::SourceFile, span_encoding::Span};
use miette::{Diagnostic, LabeledSpan, Report, SourceSpan};
use std::sync::Arc;
use thiserror::Error;

// TODO: this will be fixed temporary (maybe env in future).
pub const ERROR_CODE_URL: &str =
    "https://github.com/dungtl2003/pandora-compiler/tree/dungtl2003/feature/src/error_codes.md";

#[derive(Debug, Clone)]
pub struct ErrorHandler {
    pub file: Arc<SourceFile>,
}

impl ErrorHandler {
    // ========================== INTERPRET ==========================
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
    ) -> NonFunctionDeclaredInExternalLibrary {
        NonFunctionDeclaredInExternalLibrary
    }

    pub fn build_parse_library_file_failed_error(&self, path: String) -> ParseLibraryFileFailed {
        ParseLibraryFileFailed { path }
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
        expected: String,
        expected_span: Span,
        func_decl_span: Span,
    ) -> MissingReturnStatement {
        MissingReturnStatement {
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
        var_name: String,
        first_assign_span: Span,
        second_assign_span: Span,
        help_span: Span,
    ) -> MutateImmutableVariable {
        MutateImmutableVariable {
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
        mismatch_params: Vec<(Span, String, String)>,
        missing_param_tys: Vec<String>, // this and unexpected_params should never be non empty at the
        // same time
        unexpected_param_tys: Vec<(Span, String)>,
        after_sig_span: Span,
    ) -> FunctionParamMismatch {
        let mut missing_params: Vec<String> = missing_param_tys;
        let mut param_labels: Vec<LabeledSpan> = vec![];

        for (span, arg_ty, param_ty) in mismatch_params.iter() {
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
                let total_params = mismatch_params.len() + missing_params.len();
                let open_span = after_sig_span.to_source_span();
                let expected_arg_eng = if total_params == 1 {
                    "1 argument".to_string()
                } else {
                    format!("{} arguments", total_params)
                };
                let found_arg_eng = if mismatch_params.len() == 1 {
                    "1 argument".to_string()
                } else {
                    format!("{} arguments", mismatch_params.len())
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
                    Some(LabeledSpan::at(open_span, missing_msg.to_string())),
                    vec![],
                )
            } else {
                let total_params = mismatch_params.len() - unexpected_param_tys.len();
                let expected_arg_eng = if total_params == 1 {
                    "1 argument".to_string()
                } else {
                    format!("{} arguments", total_params)
                };
                let found_arg_eng = if mismatch_params.len() == 1 {
                    "1 argument".to_string()
                } else {
                    format!("{} arguments", mismatch_params.len())
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
    pub fn build_library_function_not_found_error(
        &self,
        library: String,
        function: String,
        span: Span,
    ) -> LibraryFunctionNotFound {
        LibraryFunctionNotFound {
            library,
            function,
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

    pub fn new(file: Arc<SourceFile>) -> Self {
        Self { file }
    }

    pub fn create_err<T>(&self, err: T) -> Report
    where
        T: Diagnostic + Send + Sync + 'static,
    {
        let report: Report = err.into();
        report.with_source_code(Arc::clone(&self.file))
    }

    pub fn emit_err<T>(&self, err: T)
    where
        T: Diagnostic + Send + Sync + 'static,
    {
        let report: Report = err.into();
        println!("{:?}", report.with_source_code(Arc::clone(&self.file)));
    }

    pub fn report_err(&self, report: Report) {
        println!("{:?}", report.with_source_code(Arc::clone(&self.file)));
    }
}

// ========================== INTERPRET ==========================

#[derive(Error, Debug, Diagnostic)]
#[error("the size for values of type `{}` cannot be known", ty)]
pub struct UnknownSizeArray {
    ty: String,
    #[label("doesn't have a size known")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("mismatched types")]
#[diagnostic(help("consider specifying the actual array length: `{}`", found_len))]
pub struct MismatchArrayTypeLength {
    expected_len: i64,
    found_len: i64,
    value_span: LabeledSpan,
    #[label("expected due to this")]
    ty_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("`{}` outside of a function", symbol)]
pub struct ReturnOutsideFunction {
    symbol: String,
    #[label("cannot `{}` outside of a function", symbol)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("`{}` outside of a loop", symbol)]
pub struct ContinueOutsideLoop {
    symbol: String,
    #[label("cannot `{}` outside of a loop", symbol)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("`{}` outside of a loop", symbol)]
pub struct BreakOutsideLoop {
    symbol: String,
    #[label("cannot `{}` outside of a loop", symbol)]
    span: SourceSpan,
}
#[derive(Error, Debug, Diagnostic)]
#[error("`{}` is not an iterator", ty)]
pub struct ExpectedIterator {
    ty: String,
    #[label("`{}` is not an iterator", ty)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("Expected `{{`")]
pub struct ExpectedBlock {
    #[label("help: try placing this code inside a block")]
    stmt_span: SourceSpan,
}
#[derive(Error, Debug, Diagnostic)]
#[error("Function with name `{}` already exists in this scope", func_name)]
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
pub struct MultipleLibrariesInScope {
    lib_name: String,
    #[label("first added here")]
    first_lib_span: SourceSpan,
    #[label("help: remove this")]
    second_lib_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("comparison operators cannot be chained")]
#[diagnostic(help("split the comparison into two"))]
pub struct ComparisonOperatorsCannotBeChained {
    #[label(collection)]
    chain_op_span: Vec<SourceSpan>,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot add `{}` to `{}`", rhs_ty, lhs_ty)]
pub struct CannotAdd {
    lhs_ty: String,
    rhs_ty: String,
    #[label("no implementation for `{} + {}`", lhs_ty, rhs_ty)]
    op_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot subtract `{}` from `{}`", rhs_ty, lhs_ty)]
pub struct CannotSubtract {
    lhs_ty: String,
    rhs_ty: String,
    #[label("no implementation for `{} - {}`", lhs_ty, rhs_ty)]
    op_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot multiply `{}` by `{}`", lhs_ty, rhs_ty)]
pub struct CannotMultiply {
    lhs_ty: String,
    rhs_ty: String,
    #[label("no implementation for `{} * {}`", lhs_ty, rhs_ty)]
    op_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot divide `{}` by `{}`", lhs_ty, rhs_ty)]
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
pub struct CannotModulo {
    lhs_ty: String,
    rhs_ty: String,
    #[label("no implementation for `{} % {}`", lhs_ty, rhs_ty)]
    op_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("can't compare `{}` with `{}`", lhs_ty, rhs_ty)]
pub struct CannotCompare {
    lhs_ty: String,
    rhs_ty: String,
    op: String,
    #[label("no implementation for `{} {} {}`", lhs_ty, op, rhs_ty)]
    op_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("no implementation for `{} {} {}`", lhs_ty, op, rhs_ty)]
pub struct NoImplForOp {
    lhs_ty: String,
    rhs_ty: String,
    op: String,
    #[label("no implementation for `{} {} {}`", lhs_ty, op, rhs_ty)]
    op_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot find variable `{}` in this scope", var_name)]
pub struct CannotFindVariableInScope {
    var_name: String,
    #[label("not found in this scope")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot find type `{}` in this scope", type_name)]
pub struct CannotFindTypeInScope {
    type_name: String,
    #[label("not found in this scope")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("used binding `{}` isn't initialized", var_name)]
#[diagnostic(help = "consider assigning a value")]
pub struct VarIsNotInitialized {
    var_name: String,
    #[label("binding declared here but left uninitialized")]
    declared_span: SourceSpan,
    #[label("`{}` used here but it isn't initialized", var_name)]
    used_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot cast `{}` as `{}`", from, to)]
pub struct CannotCast {
    from: String,
    to: String,
    #[label(primary)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("mismatched types")]
#[diagnostic(code(E0308))]
pub struct MismatchedType {
    expected: String,
    found: String,
    #[label("expected `{}`, found `{}`", expected, found)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot apply unary operator `{}` to type `{}`", op, ty)]
pub struct CannotApplyUnaryOp {
    op: String,
    ty: String,
    #[label("cannot apply unary operator `{}`", op)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("invalid left-hand side of assignment")]
pub struct InvalidLhsAssign {
    #[label(primary)]
    assign_span: SourceSpan,
    #[label("cannot assign to this expression")]
    lhs_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("Repeat count must be non-negative, found `{}`", count)]
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
pub struct IndexOutOfBounds {
    index: i64,
    len: i64,
    #[label("index out of bounds")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot index into a value of type `{}`", ty)]
#[diagnostic(code(E0608))]
pub struct IndexingWrongType {
    #[label(primary)]
    span: SourceSpan,
    ty: String,
}

#[derive(Error, Debug, Diagnostic)]
#[error("library `{}` not found", library)]
pub struct LibraryNotFound {
    library: String,
    #[label("library not found")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("function `{}` not found in library `{}`", function, library)]
pub struct LibraryFunctionNotFound {
    library: String,
    function: String,
    #[label("function not found in library")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("invalid library path")]
pub struct InvalidLibraryPath {
    #[label(primary)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("invalid function call")]
pub struct InvalidFunctionCall {
    #[label(primary)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot find function `{}` in this scope", function)]
#[diagnostic(code(E0425))]
pub struct FunctionNotInScope {
    function: String,
    #[label("not found in this scope")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("{}", message)]
#[diagnostic(code(E0308))]
pub struct FunctionParamMismatch {
    message: String,
    #[label("function defined here")]
    func_decl_span: SourceSpan,
    #[label(collection)]
    mismatch_spans: Vec<LabeledSpan>,
    missing_span: Option<LabeledSpan>,
    #[label(collection)]
    unexpected_spans: Vec<LabeledSpan>,
}

#[derive(Error, Debug, Diagnostic)]
#[error("mismatched types")]
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
pub struct MissingFunctionReturnType {
    found: String,
    #[label("help: try adding a return type: `-> {}`", found)]
    ret_help_span: SourceSpan,
    #[label("expected `()`, found `{}`", found)]
    found_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("mismatched types")]
pub struct MissingReturnStatement {
    expected: String,
    #[label("expected `{}`, found `()`", expected)]
    expected_span: SourceSpan,
    #[label("implicitly returns `()` as its body has no tail or `return` expression")]
    func_decl_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("cannot assign twice to immutable variable `{}`", var_name)]
pub struct MutateImmutableVariable {
    var_name: String,
    #[label("first assignment to `{}`", var_name)]
    first_assign_span: SourceSpan,
    #[label("cannot assign twice to immutable variable")]
    second_assign_span: SourceSpan,
    #[label("help: consider making this binding mutable: `mut {}`", var_name)]
    help_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("no source file specified")]
pub struct NoSourceFileSpecified();

#[derive(Error, Debug, Diagnostic)]
#[error("could not determine source file directory")]
pub struct DirectoryNotFound();

#[derive(Error, Debug, Diagnostic)]
#[error("external library `{}` not found", lib_name)]
pub struct ExternalLibraryNotFound {
    lib_name: String,
    #[label("library not found")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("failed to read library file `{}`", path)]
pub struct ReadLibraryFileFailed {
    path: String,
}

#[derive(Error, Debug, Diagnostic)]
#[error("failed to parse library file `{}`", path)]
pub struct ParseLibraryFileFailed {
    path: String,
}

#[derive(Error, Debug, Diagnostic)]
#[error("only function declarations are allowed in external libraries")]
pub struct NonFunctionDeclaredInExternalLibrary;

#[derive(Error, Debug, Diagnostic)]
#[error("invalid library name")]
pub struct InvalidLibraryName {
    #[label(primary)]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("function `{}` not found in library `{}`", func_name, lib_name)]
pub struct FunctionInLibraryNotFound {
    lib_name: String,
    func_name: String,
    #[label("function not found")]
    span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
#[error("")]
pub struct DUMMY();
