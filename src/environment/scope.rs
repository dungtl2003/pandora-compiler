use std::{cell::RefCell, rc::Rc};

use crate::span_encoding::Span;

use super::{item::Item, table::Table, variable::Variable, Wrapper};

#[derive(Debug)]
pub struct SematicScope {
    pub context: SematicContext,
    pub id: String,
    child_id_counter: u64,
}

#[derive(Debug)]
pub struct SematicContext {
    pub items: Table<Rc<Item>>,
    // Maybe in the future, we want to add value in to variables. So we need a way to borrow and
    // modify value easily. We can use Rc<RefCell<T>> for this purpose.
    pub variables: Table<Wrapper<Variable>>,
}

impl SematicScope {
    pub fn new(id: String) -> Self {
        Self {
            context: SematicContext {
                items: Table::new(),
                variables: Table::new(),
            },
            id,
            child_id_counter: 0,
        }
    }

    pub fn insert_variable(&mut self, variable: Variable) {
        let variable = Rc::new(RefCell::new(variable));
        self.context.variables.push(variable);
    }

    pub fn insert_item(&mut self, item: Item) -> Result<(), String> {
        let same_item = self.context.items.iter().find(|i| i.name == item.name);
        if same_item.is_some() {
            return Err(format!("Item with name {} already exists", item.name));
        }

        self.context.items.push(Rc::new(item));
        Ok(())
    }

    pub fn lookup_variable(&self, name: &str, span: Span) -> Option<Wrapper<Variable>> {
        let variables = self
            .context
            .variables
            .iter()
            .filter(|var| var.borrow().name == name)
            .rev(); // Reverse to get the most recent variable (shadowing)

        for var in variables {
            if span.is_after(var.borrow().span) {
                return Some(Rc::clone(var));
            }
        }

        None
    }

    pub fn lookup_item(&self, name: &str) -> Option<Rc<Item>> {
        let item = self.context.items.iter().find(|item| item.name == name)?;

        Some(Rc::clone(item))
    }

    /// Returns the level of the scope. The level is 0-based.   
    pub fn level(&self) -> usize {
        self.id.split('.').count() - 1
    }

    /// Returns the prefix id of the scope. The prefix id is the id without the last part.
    pub fn prefix_id(&self) -> String {
        let mut ids = self.id.split('.').collect::<Vec<&str>>();
        ids.pop();
        ids.join(".")
    }

    /// Returns a new child id for the scope. The child id is the id with a new part appended. The
    /// counter is incremented.
    pub fn child_id(&mut self) -> String {
        let id = format!("{}.{}", self.id, self.child_id_counter);
        self.child_id_counter += 1;
        id
    }
}
