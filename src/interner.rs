use string_interner::{backend::BucketBackend, symbol::SymbolU32, StringInterner, Symbol as Sym};

pub struct Interner {
    string_interner: StringInterner<BucketBackend<SymbolU32>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Symbol(u32);

impl Interner {
    pub fn new() -> Self {
        Interner {
            string_interner: StringInterner::new(),
        }
    }

    /// Get a symbol from a string.
    pub fn intern(&mut self, string: &str) -> Symbol {
        let idx = self.string_interner.get_or_intern(string).to_usize() as u32;

        Symbol(idx)
    }

    /// Get the symbol as a string.
    pub fn get(&mut self, symbol: Symbol) -> &str {
        let idx = symbol.0 as usize;

        self.string_interner
            .resolve(Sym::try_from_usize(idx).unwrap())
            .unwrap()
    }
}
