use crate::environment::item::Item;

mod std;

pub trait Library {
    fn get_item(&self, name: &str) -> Option<Item>;
}
