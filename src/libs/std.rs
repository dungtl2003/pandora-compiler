use std::collections::HashMap;

use crate::environment::{
    item::{Fun, Item, ItemKind},
    ty::{PrimTy, Ty, TyKind},
};

use super::Library;

pub struct StdLib {
    items: HashMap<String, Item>,
}

impl Library for StdLib {
    fn get_item(&self, name: &str) -> Option<Item> {
        self.items.get(name).cloned()
    }
}

impl StdLib {
    pub fn new() -> Self {
        let items = HashMap::new();

        let mut lib = Self { items };
        lib.register_items();

        lib
    }

    fn register_items(&mut self) {
        // print() function
        let params = vec![Ty::new(TyKind::Prim(PrimTy::Str))];
        let ret_ty = Ty::new(TyKind::Void);
        let name = "print".to_string();
        let print_fun_item = Item::new(name, ItemKind::Func(Fun { params, ret_ty }));
        self.items
            .insert(print_fun_item.name.clone(), print_fun_item);

        // input() function
        let params = vec![];
        let ret_ty = Ty::new(TyKind::Prim(PrimTy::Str));
        let name = "input".to_string();
        let input_fun_item = Item::new(name, ItemKind::Func(Fun { params, ret_ty }));
        self.items
            .insert(input_fun_item.name.clone(), input_fun_item);

        // len() function
        let params = vec![Ty::new(TyKind::Prim(PrimTy::Str))];
        let ret_ty = Ty::new(TyKind::Prim(PrimTy::Int));
        let name = "len".to_string();
        let len_fun_item = Item::new(name, ItemKind::Func(Fun { params, ret_ty }));
        self.items.insert(len_fun_item.name.clone(), len_fun_item);
    }
}
