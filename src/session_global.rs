use std::sync::Arc;

use miette::NamedSource;

#[derive(Debug)]
pub struct SessionGlobal {
    pub source_map: SourceMap,
    pub has_errors: bool,
}

#[derive(Debug)]
pub struct SourceMap {
    pub files: Vec<SourceFile>,
}

pub type BytePos = u32;

pub type SourceFile = NamedSource<Arc<String>>;

impl SessionGlobal {
    pub fn new() -> Self {
        Self {
            source_map: SourceMap { files: Vec::new() },
            has_errors: false,
        }
    }
}
