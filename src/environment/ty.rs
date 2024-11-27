#[derive(Debug, PartialEq, Clone)]
pub struct Ty {
    pub kind: TyKind,
}

impl Ty {
    pub fn new(kind: TyKind) -> Self {
        Self { kind }
    }

    pub fn is_primitive(&self) -> bool {
        self.kind.is_primitive()
    }

    pub fn to_rust_ty_str(&self) -> String {
        match &self.kind {
            TyKind::Prim(prim_ty) => prim_ty.to_rust_str_ty().to_string(),
            TyKind::Void => "()".to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TyKind {
    Prim(PrimTy),
    //Class(PointerType<'static>),
    //Interface(PointerType<'static>),
    Void,
}

impl TyKind {
    pub fn is_primitive(&self) -> bool {
        match self {
            TyKind::Prim(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrimTy {
    Int,
    Float,
    Bool,
    Str,
    Char,
}

impl PrimTy {
    pub fn can_cast_to(&self, other: &PrimTy) -> bool {
        match self {
            PrimTy::Int => match other {
                PrimTy::Int => true,
                PrimTy::Float => true,
                _ => false,
            },
            PrimTy::Float => match other {
                PrimTy::Int => true,
                PrimTy::Float => true,
                _ => false,
            },
            PrimTy::Bool => match other {
                PrimTy::Bool => true,
                _ => false,
            },
            PrimTy::Str => match other {
                PrimTy::Str => true,
                PrimTy::Char => true,
                PrimTy::Int => true,
                PrimTy::Float => true,
                _ => false,
            },
            PrimTy::Char => match other {
                PrimTy::Char => true,
                _ => false,
            },
        }
    }

    pub fn to_rust_str_ty(&self) -> &str {
        match self {
            PrimTy::Int => "i64",
            PrimTy::Float => "f64",
            PrimTy::Bool => "bool",
            PrimTy::Str => "&str",
            PrimTy::Char => "char",
        }
    }
}
