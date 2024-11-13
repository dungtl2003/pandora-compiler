use crate::interner;
use std::mem::MaybeUninit;
use std::sync::Arc;

use crate::interner::{Interner, Symbol};
use miette::NamedSource;
use std::sync::Once;

pub struct SessionGlobal {
    pub source_map: SourceMap,
    pub interner: Interner,
    pub has_errors: bool,
}

pub struct SourceMap {
    pub files: Vec<SourceFile>,
}

pub type BytePos = u32;

pub type SourceFile = NamedSource<Arc<String>>;

impl SessionGlobal {
    pub fn instance() -> &'static mut Self {
        static mut SESSION_GLOBAL: MaybeUninit<SessionGlobal> = MaybeUninit::uninit();
        static ONCE: Once = Once::new();

        unsafe {
            ONCE.call_once(|| {
                let session_global = SessionGlobal {
                    source_map: SourceMap { files: Vec::new() },
                    interner: Interner::new(),
                    has_errors: false,
                };

                SESSION_GLOBAL.write(session_global);
            });

            SESSION_GLOBAL.assume_init_mut()
        }
    }

    pub fn intern(&mut self, string: &str) -> interner::Symbol {
        self.interner.intern(string)
    }

    pub fn get(&mut self, symbol: Symbol) -> &str {
        self.interner.get(symbol)
    }
}
