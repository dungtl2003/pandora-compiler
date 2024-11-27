pub mod item;
mod scope;
mod table;
pub mod ty;
pub mod variable;

use core::fmt;
use std::{
    cell::RefCell,
    fmt::{Display, Formatter},
    rc::Rc,
};

use item::Item;
use scope::SematicScope;
use ty::Ty;
use variable::Variable;

use crate::{span_encoding::Span, symbol::Symbol};

pub type Wrapper<T> = Rc<RefCell<T>>;

pub struct Environment {
    pub scopes: Vec<Vec<Wrapper<SematicScope>>>,
    /// This only tracks when building the context. It is not used for lookups.
    pub current_scope: Wrapper<SematicScope>,
}

impl Display for Environment {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut level = 0;
        let spaces = "  ";
        for scope in &self.scopes {
            for s in scope {
                write!(f, "{}", spaces.repeat(level))?;
                write!(f, "{}\n", s.borrow().id)?;
                s.borrow().context.variables.iter().for_each(|var| {
                    write!(f, "{}", spaces.repeat(level + 1));
                    write!(f, "{:?}\n", var);
                });
            }
            level += 1;
        }

        Ok(())
    }
}

impl Environment {
    pub fn new() -> Self {
        let current_scope = Rc::new(RefCell::new(SematicScope::new(String::from("0"))));
        let current_scope_borrowed = Rc::clone(&current_scope);
        let scopes = vec![vec![current_scope]];
        Self {
            scopes,
            current_scope: current_scope_borrowed,
        }
    }

    /// Returns the parent scope of the current scope.
    /// This should be used when building the table, not for lookups process.
    pub fn previous_scope(&mut self) -> Wrapper<SematicScope> {
        let level = self.current_scope.borrow().level();

        if level == 0 {
            return Rc::clone(&self.current_scope);
        }

        let parent_id = self.current_scope.borrow().prefix_id();
        let parent_level = level - 1;
        self.current_scope = Rc::clone(
            &self
                .scopes
                .get(parent_level)
                .unwrap()
                .iter()
                .find(|scope| scope.borrow().id == parent_id)
                .unwrap(),
        );

        Rc::clone(&self.current_scope)
    }

    /// Returns the new child scope of the current scope.
    /// This should be used when building the table, not for lookups process.
    pub fn enter_new_scope(&mut self) -> Wrapper<SematicScope> {
        let next_level = self.current_scope.borrow().level() + 1;
        let new_child_id = self.current_scope.borrow_mut().child_id();

        let new_scope = Rc::new(RefCell::new(SematicScope::new(new_child_id)));
        self.current_scope = Rc::clone(&new_scope);

        if next_level >= self.scopes.len() {
            self.scopes.push(vec![new_scope]);
        } else {
            self.scopes.get_mut(next_level).unwrap().push(new_scope);
        }

        Rc::clone(&self.current_scope)
    }

    /// Returns the variable if it exists in the current scope or any of its parent scopes.
    /// If the variable is not found, it returns None. This function is used ONLY after the table is
    /// built.
    pub fn lookup_variable(
        &self,
        name: &str,
        span: Span,
        scope_id: &str,
    ) -> Option<Wrapper<Variable>> {
        if scope_id.is_empty() {
            return None;
        }

        let level = self.level(scope_id) as isize;

        let scope: Wrapper<SematicScope> = Rc::clone(
            &self
                .scopes
                .get(level as usize)
                .unwrap()
                .iter()
                .find(|scope| scope.borrow().id == scope_id)
                .unwrap(),
        );

        let variable = scope.borrow().lookup_variable(name, span);

        if variable.is_some() {
            return variable;
        }

        let prefix_id = scope.borrow().prefix_id();
        self.lookup_variable(name, span, &prefix_id)
    }

    pub fn lookup_item(&self, name: &str, scope_id: &str) -> Option<Rc<Item>> {
        if scope_id.is_empty() {
            return None;
        }

        let level = self.level(scope_id) as isize;

        let scope: Wrapper<SematicScope> = Rc::clone(
            &self
                .scopes
                .get(level as usize)
                .unwrap()
                .iter()
                .find(|scope| scope.borrow().id == scope_id)
                .unwrap(),
        );

        let item = scope.borrow().lookup_item(name);

        if item.is_some() {
            return item;
        }

        let prefix_id = scope.borrow().prefix_id();
        self.lookup_item(name, &prefix_id)
    }

    /// Inserts a variable into the current scope and returns the scope id of the current scope.
    pub fn insert_variable(&self, variable: Variable) -> String {
        let scope_id = self.current_scope.borrow().id.clone();
        self.current_scope.borrow_mut().insert_variable(variable);
        scope_id
    }

    pub fn insert_item(&self, item: Item) -> Result<String, String> {
        let scope_id = self.current_scope.borrow().id.clone();
        self.current_scope.borrow_mut().insert_item(item)?;
        Ok(scope_id)
    }

    fn level(&self, scope_id: &str) -> usize {
        scope_id.split('.').count() - 1
    }
}
