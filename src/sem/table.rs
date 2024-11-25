use core::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct Table<T> {
    items: Vec<T>,
}

impl<T> Table<T> {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn push(&mut self, item: T) {
        self.items.push(item);
    }

    pub fn iter(&self) -> std::slice::Iter<T> {
        self.items.iter()
    }
}
