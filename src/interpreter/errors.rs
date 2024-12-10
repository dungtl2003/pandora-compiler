use crate::{span_encoding::Span, ErrorHandler};

#[derive(Debug, Clone)]
pub enum IError {
    PredefinedError {
        message: String,
        span: Span,
    },
    ModdedByZero {
        divident: String,
        span: Span,
    },
    DividedByZero {
        divident: String,
        span: Span,
    },
    LitOutOfRange {
        span: Span,
        max: i64,
    },
    ArrayHasMultipleTypes {
        first_el_ty: String,
        first_mismatch_ty: String,
        first_el_span: Span,
        first_mismatch_span: Span,
    },
    ExpectedBlockAfterCondition {
        if_symbol: String,
        condition_span: Span,
        stmt_span: Span,
    },
    UnknownSizeArray {
        ty: String,
        span: Span,
    },
    MismatchArrayTypeLength {
        expected_len: i64,
        found_len: i64,
        value_span: Span,
        ty_span: Span,
    },
    ReturnOutsideFunction {
        symbol: String,
        span: Span,
    },
    ContinueOutsideLoop {
        symbol: String,
        span: Span,
    },
    BreakOutsideLoop {
        symbol: String,
        span: Span,
    },
    ExpectedIterator {
        ty: String,
        span: Span,
    },
    ExpectedBlock {
        stmt_span: Span,
    },
    FunctionAlreadyDeclaredInScope {
        func_name: String,
        first_decl_span: Span,
        second_decl_span: Span,
    },
    MultipleLibrariesInScope {
        lib_name: String,
        first_lib_span: Span,
        second_lib_span: Span,
    },
    FunctionInLibraryNotFound {
        func_name: String,
        lib_name: String,
        span: Span,
    },

    InvalidLibraryName {
        span: Span,
    },

    NonFunctionDeclaredInExternalLibrary {
        span: Span,
    },

    ParseLibraryFileFailed {
        span: Span,
        path: String,
    },

    ReadLibraryFileFailed {
        path: String,
    },

    ExternalLibraryNotFound {
        lib_name: String,
        span: Span,
    },

    DirectoryNotFound,

    NoSourceFileSpecified,

    MissingReturnStatement {
        ret_kw: String,
        expected: String,
        expected_span: Span,
        func_decl_span: Span,
    },

    MissingFunctionReturnType {
        found: String,
        ret_help_span: Span,
        found_span: Span,
    },

    MutateImmutableVariable {
        mut_kw: String,
        var_name: String,
        first_assign_span: Span,
        second_assign_span: Span,
        help_span: Span,
    },

    MismatchedFunctionReturnType {
        expected: String,
        found: String,
        expected_span: Span,
        found_span: Span,
    },

    FunctionParamMismatch {
        func_decl_span: Span,
        args: usize,
        mismatch_params: Vec<(Span, String, String)>,
        missing_param_tys: Vec<String>, // this and unexpected_params should never be non empty at the
        // same time
        unexpected_param_tys: Vec<(Span, String)>,
        prefix_span: Span,
    },

    FunctionNotInScope {
        function: String,
        span: Span,
    },

    InvalidFunctionCall {
        span: Span,
    },

    InvalidLibraryPath {
        span: Span,
    },

    LibraryNotFound {
        library: String,
        span: Span,
    },

    IndexingWrongType {
        ty: String,
        span: Span,
    },

    IndexOutOfBounds {
        len: i64,
        index: i64,
        span: Span,
    },

    NegRepeatCount {
        count: i64,
        span: Span,
    },

    InvalidLhsAssign {
        assign_span: Span,
        lhs_span: Span,
    },

    CannotApplyUnaryOp {
        op: String,
        ty: String,
        span: Span,
    },

    CannotCast {
        from: String,
        to: String,
        span: Span,
    },

    MismatchedType {
        expected: String,
        found: String,
        span: Span,
    },

    CannotFindTypeInScope {
        type_name: String,
        span: Span,
    },

    VariableIsNotInitialized {
        var_name: String,
        declared_span: Span,
        used_span: Span,
    },

    CannotFindVariableInScope {
        var_name: String,
        span: Span,
    },

    NoImplForOp {
        lhs_ty: String,
        rhs_ty: String,
        op: String,
        op_span: Span,
    },

    CannotCompare {
        lhs_ty: String,
        rhs_ty: String,
        op: String,
        op_span: Span,
    },

    CannotDivide {
        lhs_ty: String,
        rhs_ty: String,
        op_span: Span,
    },

    CannotMultiply {
        lhs_ty: String,
        rhs_ty: String,
        op_span: Span,
    },

    CannotSubtract {
        lhs_ty: String,
        rhs_ty: String,
        op_span: Span,
    },

    CannotModulo {
        lhs_ty: String,
        rhs_ty: String,
        op_span: Span,
    },

    CannotAdd {
        lhs_ty: String,
        rhs_ty: String,
        op_span: Span,
    },

    ComparisonOperatorsCannotBeChained {
        chain_op_span: Vec<Span>,
    },
}

impl IError {
    pub fn to_report(self, error_handler: &ErrorHandler) -> miette::Report {
        match self {
            IError::CannotAdd {
                lhs_ty,
                rhs_ty,
                op_span,
            } => error_handler
                .build_cannot_add_error(lhs_ty, rhs_ty, op_span)
                .into(),
            IError::CannotSubtract {
                lhs_ty,
                rhs_ty,
                op_span,
            } => error_handler
                .build_cannot_subtract_error(lhs_ty, rhs_ty, op_span)
                .into(),
            IError::CannotMultiply {
                lhs_ty,
                rhs_ty,
                op_span,
            } => error_handler
                .build_cannot_multiply_error(lhs_ty, rhs_ty, op_span)
                .into(),
            IError::CannotDivide {
                lhs_ty,
                rhs_ty,
                op_span,
            } => error_handler
                .build_cannot_divide_error(lhs_ty, rhs_ty, op_span)
                .into(),
            IError::CannotModulo {
                lhs_ty,
                rhs_ty,
                op_span,
            } => error_handler
                .build_cannot_modulo_error(lhs_ty, rhs_ty, op_span)
                .into(),
            IError::CannotCast { from, to, span } => {
                error_handler.build_cannot_cast_error(from, to, span).into()
            }
            IError::CannotCompare {
                lhs_ty,
                rhs_ty,
                op,
                op_span,
            } => error_handler
                .build_cannot_compare_error(lhs_ty, rhs_ty, op, op_span)
                .into(),
            IError::CannotApplyUnaryOp { op, ty, span } => error_handler
                .build_cannot_apply_unary_op_error(op, ty, span)
                .into(),
            IError::CannotFindTypeInScope { type_name, span } => error_handler
                .build_cannot_find_type_in_scope_error(type_name, span)
                .into(),
            IError::CannotFindVariableInScope { var_name, span } => error_handler
                .build_cannot_find_variable_in_scope_error(var_name, span)
                .into(),
            IError::ComparisonOperatorsCannotBeChained { chain_op_span } => error_handler
                .build_comparison_operators_cannot_be_chained_error(chain_op_span)
                .into(),
            IError::FunctionParamMismatch {
                func_decl_span,
                args,
                mismatch_params,
                missing_param_tys,
                unexpected_param_tys,
                prefix_span,
            } => error_handler
                .build_function_param_mismatch(
                    func_decl_span,
                    args,
                    mismatch_params,
                    missing_param_tys,
                    unexpected_param_tys,
                    prefix_span,
                )
                .into(),
            IError::NoImplForOp {
                lhs_ty,
                rhs_ty,
                op,
                op_span,
            } => error_handler
                .build_no_implementation_for_op_error(lhs_ty, rhs_ty, op, op_span)
                .into(),
            IError::NegRepeatCount { count, span } => error_handler
                .build_neg_repeat_count_error(count, span)
                .into(),
            IError::MismatchedType {
                expected,
                found,
                span,
            } => error_handler
                .build_mismatched_type_error(expected, found, span)
                .into(),
            IError::LibraryNotFound { library, span } => error_handler
                .build_library_not_found_error(library, span)
                .into(),
            IError::IndexOutOfBounds { len, index, span } => error_handler
                .build_index_out_of_bounds_error(len, index, span)
                .into(),
            IError::InvalidLhsAssign {
                assign_span,
                lhs_span,
            } => error_handler
                .build_invalid_lhs_assign_error(assign_span, lhs_span)
                .into(),
            IError::FunctionInLibraryNotFound {
                func_name,
                lib_name,
                span,
            } => error_handler
                .build_function_in_library_not_found_error(func_name, lib_name, span)
                .into(),
            IError::DirectoryNotFound => error_handler.build_directory_not_found_error().into(),
            IError::IndexingWrongType { ty, span } => error_handler
                .build_indexing_wrong_type_error(ty, span)
                .into(),
            IError::InvalidLibraryName { span } => {
                error_handler.build_invalid_library_name_error(span).into()
            }
            IError::FunctionNotInScope { function, span } => error_handler
                .build_function_not_in_scope_error(function, span)
                .into(),
            IError::InvalidLibraryPath { span } => {
                error_handler.build_invalid_library_path_error(span).into()
            }
            IError::InvalidFunctionCall { span } => {
                error_handler.build_invalid_function_call_error(span).into()
            }
            IError::ReadLibraryFileFailed { path } => error_handler
                .build_read_library_file_failed_error(path)
                .into(),
            IError::NoSourceFileSpecified => {
                error_handler.build_no_source_file_specified_error().into()
            }
            IError::ParseLibraryFileFailed { span, path } => error_handler
                .build_parse_library_file_failed_error(span, path)
                .into(),
            IError::MissingReturnStatement {
                ret_kw,
                expected,
                expected_span,
                func_decl_span,
            } => error_handler
                .build_missing_return_statement_error(
                    ret_kw,
                    expected,
                    expected_span,
                    func_decl_span,
                )
                .into(),
            IError::ExternalLibraryNotFound { lib_name, span } => error_handler
                .build_external_library_not_found_error(lib_name, span)
                .into(),
            IError::MutateImmutableVariable {
                mut_kw,
                var_name,
                first_assign_span,
                second_assign_span,
                help_span,
            } => error_handler
                .build_mutate_immutable_variable_error(
                    mut_kw,
                    var_name,
                    first_assign_span,
                    second_assign_span,
                    help_span,
                )
                .into(),
            IError::VariableIsNotInitialized {
                var_name,
                declared_span,
                used_span,
            } => error_handler
                .build_var_is_not_initialized_error(var_name, declared_span, used_span)
                .into(),
            IError::MissingFunctionReturnType {
                found,
                ret_help_span,
                found_span,
            } => error_handler
                .build_missing_function_return_type_error(found, ret_help_span, found_span)
                .into(),
            IError::MismatchedFunctionReturnType {
                expected,
                found,
                expected_span,
                found_span,
            } => error_handler
                .build_mismatched_function_return_type(expected, found, expected_span, found_span)
                .into(),
            IError::NonFunctionDeclaredInExternalLibrary { span } => error_handler
                .build_non_function_declared_in_external_library_error(span)
                .into(),
            IError::MultipleLibrariesInScope {
                lib_name,
                first_lib_span,
                second_lib_span,
            } => error_handler
                .build_multiple_libraries_in_scope_error(lib_name, first_lib_span, second_lib_span)
                .into(),
            IError::FunctionAlreadyDeclaredInScope {
                func_name,
                first_decl_span,
                second_decl_span,
            } => error_handler
                .build_function_already_declared_in_scope_error(
                    func_name,
                    first_decl_span,
                    second_decl_span,
                )
                .into(),
            IError::ExpectedBlock { stmt_span } => {
                error_handler.build_expected_block_error(stmt_span).into()
            }
            IError::ExpectedIterator { ty, span } => {
                error_handler.build_expected_iterator_error(ty, span).into()
            }
            IError::BreakOutsideLoop { symbol, span } => error_handler
                .build_break_outside_loop_error(symbol, span)
                .into(),
            IError::ContinueOutsideLoop { symbol, span } => error_handler
                .build_continue_outside_loop_error(symbol, span)
                .into(),
            IError::ReturnOutsideFunction { symbol, span } => error_handler
                .build_return_outside_function_error(symbol, span)
                .into(),
            IError::MismatchArrayTypeLength {
                expected_len,
                found_len,
                value_span,
                ty_span,
            } => error_handler
                .build_mismatch_array_type_length_error(
                    expected_len,
                    found_len,
                    value_span,
                    ty_span,
                )
                .into(),
            IError::UnknownSizeArray { ty, span } => error_handler
                .build_unknown_size_array_error(ty, span)
                .into(),
            IError::ExpectedBlockAfterCondition {
                if_symbol,
                condition_span,
                stmt_span,
            } => error_handler
                .build_expected_block_after_condition_error(if_symbol, condition_span, stmt_span)
                .into(),
            IError::ArrayHasMultipleTypes {
                first_el_ty,
                first_mismatch_ty,
                first_el_span,
                first_mismatch_span,
            } => error_handler
                .build_array_has_multiple_types_error(
                    first_el_ty,
                    first_mismatch_ty,
                    first_el_span,
                    first_mismatch_span,
                )
                .into(),
            IError::LitOutOfRange { span, max } => {
                error_handler.build_lit_out_of_range_error(span, max).into()
            }
            IError::DividedByZero { divident, span } => error_handler
                .build_divided_by_zero_error(divident, span)
                .into(),
            IError::ModdedByZero { divident, span } => error_handler
                .build_modded_by_zero_error(divident, span)
                .into(),
            IError::PredefinedError { message, span } => {
                error_handler.build_predefined_error(span, message).into()
            }
        }
    }
}
