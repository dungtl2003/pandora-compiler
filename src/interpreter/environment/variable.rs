use crate::interpreter::{eval::Value, ty::Ty};

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub is_mut: bool,
    pub value: Option<Value>,
    pub ty: Ty,
}

impl Variable {
    pub fn can_be_assigned(&self) -> bool {
        self.is_mut || self.value.is_none()
    }
}
