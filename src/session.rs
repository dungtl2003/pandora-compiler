use std::sync::Arc;

use miette::NamedSource;

use crate::ErrorHandler;

#[derive(Debug)]
pub struct Session {
    pub error_handler: ErrorHandler,
}

pub type BytePos = u32;

pub type SourceFile = NamedSource<Arc<String>>;

impl Session {
    pub fn new(file: Arc<SourceFile>) -> Self {
        Self {
            error_handler: ErrorHandler::new(Arc::clone(&file)),
        }
    }
}
