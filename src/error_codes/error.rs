enum error {
    FuncInLibNotFound {
        func_name: String,
        lib_name: String,
        span: Span,
    },

    InvalidLibraryName {
        span: Span,
    },

    NonFunctionDeclaredInExternalLibrary,

    ParseLibraryFileFailed {
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

    MissingReturnStatement {    //thiếu câu lệnh trả về
        expected: String,
        expected_span: Span,
        func_decl_span: Span,
    },

    MissingFunctionReturnType { //sai kiểu dữ liệu trả về của hàm/phương thức
        found: String,
        ret_help_span: Span,
        found_span: Span,
    },

    MutateImmutableVariable {   //gán giá trị cho biến immut
        var_name: String,
        first_assign_span: Span,
        second_assign_span: Span,
        help_span: Span,
    },

    MismatchedFunctionReturnType {  //sai kiểu dữ liệu trả về của hàm/phương thức
        found: String,
        expected: String,
        found: String,
        expected_span: Span,
        found_span: Span,
    },

    FunctionParamMismatch { //Sai tham số truyền vào (số lượng và kiểu không hợp lệ)
        func_decl_span: Span,
        mismatch_params: Vec<(Span, String, String)>,
        missing_param_tys: Vec<String>, // this and unexpected_params should never be non empty at the
        // same time
        unexpected_param_tys: Vec<(Span, String)>,
        after_sig_span: Span,
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

    LibraryFunctionNotFound {
        library: String,
        function: String,
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

    DUMMY,
}