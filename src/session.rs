use std::sync::Arc;

use miette::NamedSource;

use crate::{ErrorHandler, ErrorType};

#[derive(Debug)]
pub struct Session {
    pub error_type: ErrorType,
    pub error_handler: ErrorHandler,
}

pub type BytePos = u32;

pub type SourceFile = NamedSource<Arc<String>>;

impl Session {
    pub fn new(file: Arc<SourceFile>) -> Self {
        Self {
            error_type: ErrorType::NoError,
            error_handler: ErrorHandler::new(Arc::clone(&file)),
        }
    }

    pub fn set_error(&mut self, error_type: ErrorType) {
        self.error_type = error_type;
    }

    pub fn has_error(&self) -> bool {
        self.error_type != ErrorType::NoError
    }

    pub fn can_recover(&self) -> bool {
        self.error_type == ErrorType::Recoverable
    }
}
