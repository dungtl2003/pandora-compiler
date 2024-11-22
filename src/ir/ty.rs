//use inkwell::types::PointerType;

#[derive(Debug, PartialEq, Clone)]
pub struct Ty {
    pub kind: TyKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TyKind {
    Prim(PrimTy),
    //Class(PointerType<'static>),
    //Interface(PointerType<'static>),
    Void,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrimTy {
    Int(Option<i64>),
    Float(Option<f64>),
    Bool(Option<bool>),
}

impl PrimTy {
    pub fn to_str_ty(&self) -> &str {
        match self {
            PrimTy::Int(_) => "int",
            PrimTy::Float(_) => "float",
            PrimTy::Bool(_) => "bool",
        }
    }

    pub fn from_str_ty(s: &str) -> Option<Self> {
        match s {
            "int" => Some(PrimTy::Int(None)),
            "float" => Some(PrimTy::Float(None)),
            "bool" => Some(PrimTy::Bool(None)),
            _ => None,
        }
    }
}
