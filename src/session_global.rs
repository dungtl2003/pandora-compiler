use std::sync::Arc;

use miette::NamedSource;

pub type BytePos = u32;

pub type SourceFile = NamedSource<Arc<String>>;
