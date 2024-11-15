use crate::interner;
use std::sync::Arc;

use crate::interner::{Interner, Symbol};
use miette::NamedSource;

#[derive(Debug)]
pub struct SessionGlobal {
    pub source_map: SourceMap,
    pub interner: Interner,
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
            interner: Interner::new(),
            has_errors: false,
        }
    }

    pub fn intern(&mut self, string: &str) -> interner::Symbol {
        println!("Interning: {}", string);
        self.interner.intern(string)
    }

    pub fn get(&mut self, symbol: Symbol) -> &str {
        println!("Getting: {:?}", symbol);
        self.interner.get(symbol)
    }
}
