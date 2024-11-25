#[derive(Debug)]
pub struct Item {
    pub kind: ItemKind,
}

#[derive(Debug)]
pub enum ItemKind {
    Function,
}

//pub struct Function {
//    pub name: String,
//    pub params: Vec<Param>,
//    pub ret_ty: Option<Ty>,
//    pub body: Vec<Stmt>,
//}
