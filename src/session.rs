use std::sync::Arc;

use miette::NamedSource;

use crate::ErrorHandler;

#[derive(Debug)]
pub struct Session {
    pub has_errors: bool,
    pub error_handler: ErrorHandler,
}

pub type BytePos = u32;

pub type SourceFile = NamedSource<Arc<String>>;

impl Session {
    pub fn new(file: Arc<SourceFile>) -> Self {
        Self {
            has_errors: false,
            error_handler: ErrorHandler::new(Arc::clone(&file)),
        }
    }

    pub fn set_has_errors(&mut self) {
        self.has_errors = true;
    }
}
