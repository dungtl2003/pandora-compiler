use super::ty::Ty;

#[derive(Debug, Clone)]
pub struct Item {
    pub name: String,
    pub kind: ItemKind,
}

impl Item {
    pub fn new(name: String, kind: ItemKind) -> Self {
        Self { name, kind }
    }
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Func(Fun),
}

#[derive(Debug, Clone)]
pub struct Fun {
    pub params: Vec<Ty>,
    pub ret_ty: Ty,
}
