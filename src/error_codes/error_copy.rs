enum error {
    FuncInLibNotFound,

    InvalidLibraryName,

    NonFunctionDeclaredInExternalLibrary,

    ParseLibraryFileFailed,

    ReadLibraryFileFailed,

    ExternalLibraryNotFound,

    DirectoryNotFound,

    NoSourceFileSpecified,

    MissingReturnStatement,

    MissingFunctionReturnType,

    MutateImmutableVariable,

    MismatchedFunctionReturnType,

    FunctionParamMismatch,

    FunctionNotInScope,

    InvalidFunctionCall,

    InvalidLibraryPath,

    LibraryFunctionNotFound,

    LibraryNotFound,

    IndexingWrongType,

    IndexOutOfBounds,

    NegRepeatCount,

    InvalidLhsAssign,

    CannotApplyUnaryOp ,

    CannotCast ,

    MismatchedType,

    CannotFindTypeInScope,

    VariableIsNotInitialized,

    CannotFindVariableInScope,

    NoImplForOp,

    CannotCompare,

    CannotDivide,

    CannotMultiply,

    CannotSubtract,

    CannotModulo,

    CannotAdd,

    ComparisonOperatorsCannotBeChained,

}