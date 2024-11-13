use crate::session_global::SourceFile;
use miette::{Diagnostic, Report};
use std::sync::Arc;

// TODO: this will be fixed temporary (maybe env in future).
pub const ERROR_CODE_URL: &str =
    "https://github.com/dungtl2003/pandora-compiler/tree/dungtl2003/feature/src/error_codes.md";

pub struct ErrorHandler {
    pub file: Arc<SourceFile>,
}

impl ErrorHandler {
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
}
