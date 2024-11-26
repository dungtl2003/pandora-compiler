use core::fmt;
use std::{
    cell::RefCell,
    fmt::{Display, Formatter},
    rc::Rc,
};

use crate::{span_encoding::Span, symbol::Symbol};

use super::{item::Item, table::Table, variable::Variable, PrimTy, Ty, TyKind};

pub type Wrapper<T> = Rc<RefCell<T>>;

pub struct ContextManager {
    pub scopes: Vec<Vec<Wrapper<SematicScope>>>,
    /// This only tracks when building the context. It is not used for lookups.
    pub current_scope: Wrapper<SematicScope>,
}

impl Display for ContextManager {
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

impl ContextManager {
    pub fn new() -> Self {
        let current_scope = Rc::new(RefCell::new(SematicScope::new(String::from("0"))));
        let current_scope_borrowed = Rc::clone(&current_scope);
        let scopes = vec![vec![current_scope]];
        Self {
            scopes,
            current_scope: current_scope_borrowed,
        }
    }

    /// Returns the parent scope of the current scope (unless the current scope is the root scope).
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

    pub fn lookup_variable(
        &self,
        name: Symbol,
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

    // FIX: This function is not implemented correctly.
    pub fn lookup_type(&self, name: Symbol, scope_id: Option<String>) -> Option<Ty> {
        if Ty::is_primitive(name.as_str()) {
            return Some(Ty {
                kind: TyKind::Prim(PrimTy::from_str_ty(name.as_str()).unwrap()),
            });
        }

        let binding = scope_id.unwrap();
        let scope_id = binding.as_str();
        let level = self.level(scope_id) as isize;
        if level < 0 {
            return None;
        }

        let scope = self
            .scopes
            .get(level as usize)
            .unwrap()
            .iter()
            .find(|scope| scope.borrow().id == scope_id)
            .unwrap();

        let ty = scope.borrow().lookup_type(name);

        if ty.is_some() {
            return ty;
        }

        let prefix_id = scope.borrow().prefix_id();
        self.lookup_type(name, Some(prefix_id))
    }

    /// Inserts a variable into the current scope and returns the scope id of the current scope.
    pub fn insert_variable(&self, variable: Variable) -> String {
        let scope_id = self.current_scope.borrow().id.clone();
        self.current_scope.borrow_mut().insert_variable(variable);
        scope_id
    }

    pub fn level(&self, scope_id: &str) -> usize {
        scope_id.split('.').count() - 1
    }
}

#[derive(Debug)]
pub struct SematicScope {
    pub context: SematicContext,
    pub id: String,
    child_id_counter: u64,
}

#[derive(Debug)]
pub struct SematicContext {
    pub items: Table<Item>,
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

    pub fn lookup_variable(&self, name: Symbol, span: Span) -> Option<Rc<RefCell<Variable>>> {
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

    // FIX: Currently, this function does not care about the path. It also handles only primitive
    // types.
    pub fn lookup_type(&self, name: Symbol) -> Option<Ty> {
        if let Some(kind) = PrimTy::from_str_ty(name.as_str()) {
            Some(Ty {
                kind: TyKind::Prim(kind),
            })
        } else {
            None
        }
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
