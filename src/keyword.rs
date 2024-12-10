use once_cell::sync::Lazy;
use std::{collections::HashMap, str::FromStr, sync::Mutex};

use crate::symbol::Symbol;

static KW_TO_TXT_MAP: Lazy<Mutex<HashMap<Keyword, (&'static str, &'static str)>>> =
    Lazy::new(|| {
        let mut map = HashMap::new();
        let kw_map_arr = get_kw_map_arr();

        for (kw, mode) in kw_map_arr.into_iter() {
            map.insert(kw, mode);
        }

        Mutex::new(map)
    });

static NORM_TXT_TO_KW_MAP: Lazy<Mutex<HashMap<&'static str, Keyword>>> = Lazy::new(|| {
    let mut map = HashMap::new();
    let kw_map_arr = get_kw_map_arr();

    for (kw, (norm, _)) in kw_map_arr.into_iter() {
        map.insert(norm, kw);
    }

    Mutex::new(map)
});

static GENZ_TXT_TO_KW_MAP: Lazy<Mutex<HashMap<&'static str, Keyword>>> = Lazy::new(|| {
    let mut map = HashMap::new();
    let kw_map_arr = get_kw_map_arr();

    for (kw, (_, genz)) in kw_map_arr.into_iter() {
        map.insert(genz, kw);
    }

    Mutex::new(map)
});

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Keyword {
    True,
    False,
    Set,
    Mut,
    When,
    Alt,
    Fun,
    Br,
    Skip,
    For,
    In,
    During,
    As,
    Add,
    Yeet,
}

impl FromStr for Keyword {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let is_genz = crate::is_genz_mode();
        let map = if is_genz {
            GENZ_TXT_TO_KW_MAP.lock().unwrap()
        } else {
            NORM_TXT_TO_KW_MAP.lock().unwrap()
        };

        map.get(s).cloned().ok_or("Invalid keyword".to_string())
    }
}

impl AsRef<str> for Keyword {
    fn as_ref(&self) -> &str {
        let is_genz = crate::is_genz_mode();
        let map = KW_TO_TXT_MAP.lock().unwrap();
        let (normal, genz) = map.get(self).unwrap();

        if is_genz {
            genz
        } else {
            normal
        }
    }
}

pub fn to_symbol(keyword: Keyword) -> Symbol {
    Symbol::from(keyword.as_ref())
}

pub fn is_keyword(symbol: Symbol) -> bool {
    Keyword::from_str(symbol.as_str()).is_ok()
}

pub fn from_str(s: &str) -> Result<Keyword, String> {
    Keyword::from_str(s)
}

pub fn to_string(keyword: Keyword) -> String {
    keyword.as_ref().to_string()
}

fn get_kw_map_arr() -> [(Keyword, (&'static str, &'static str)); 15] {
    [
        (Keyword::True, ("true", "yass")),
        (Keyword::False, ("false", "nope")),
        (Keyword::Set, ("set", "vibe")),
        (Keyword::Mut, ("mut", "chill")),
        (Keyword::When, ("when", "fr")),
        (Keyword::Alt, ("alt", "nah")),
        (Keyword::Fun, ("fun", "doit")),
        (Keyword::Br, ("br", "bruhstop")),
        (Keyword::Skip, ("skip", "keepitup")),
        (Keyword::For, ("for", "onloop")),
        (Keyword::In, ("in", "among")),
        (Keyword::During, ("during", "staylit")),
        (Keyword::As, ("as", "flexin")),
        (Keyword::Add, ("add", "snatch")),
        (Keyword::Yeet, ("yeet", "bounce")),
    ]
}
