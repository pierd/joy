use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{symbol::Symbol, value::Value};

#[derive(Debug, Default, Clone)]
pub struct Scope(Rc<RefCell<ScopeInternal>>);
#[derive(Debug, Default, Clone)]
struct ScopeInternal {
    parent: Option<Scope>,
    // FIXME: those values probably have to be scoped (so they have access to private/public scopes)
    values: HashMap<Symbol, ScopedValue>,
}
impl Scope {
    pub fn set(&self, name: Symbol, value: impl Into<Value>) {
        self.0.borrow_mut().values.insert(
            name,
            ScopedValue::Value {
                scopes: self.clone().into(),
                value: value.into(),
            },
        );
    }

    pub fn get(&self, name: Symbol) -> Option<ScopedValue> {
        self.0
            .borrow()
            .values
            .get(&name)
            .cloned()
            // .. or try getting from the parent
            .or_else(|| self.0.borrow().parent.as_ref().and_then(|p| p.get(name)))
    }
}

#[derive(Debug, Clone)]
pub struct Scopes {
    pub(crate) read: Scope,
    pub(crate) write: Scope,
}
impl std::default::Default for Scopes {
    fn default() -> Self {
        Scope::default().into()
    }
}
impl From<Scope> for Scopes {
    fn from(value: Scope) -> Self {
        Self {
            read: value.clone(),
            write: value,
        }
    }
}
impl Scopes {
    pub fn with_parent(parent: Scopes) -> Self {
        let new = Scopes::default();
        new.read.0.borrow_mut().parent = Some(parent.read);
        new.write.0.borrow_mut().parent = Some(parent.write);
        new
    }
    // FIXME: rethink how public/private works - this is a bit convoluted
    pub fn with_sealed(private: Scopes) -> Self {
        let new = Scopes::default();
        // new scope should read from the private first
        *new.read.0.borrow_mut() = private.read.0.take();
        new.write.0.borrow_mut().parent = private.write.0.borrow().parent.clone();
        new
    }

    pub fn set(&self, name: Symbol, value: impl Into<Value>) {
        self.write.set(name, value);
    }
    // FIXME: should we keep just the write one?
    pub fn set_scope(&self, name: Symbol, scopes: Scopes) {
        self.write
            .0
            .borrow_mut()
            .values
            .insert(name, ScopedValue::Scope(scopes.write));
    }
    // FIXME: should we keep just the write one?
    pub fn merge_in(&self, scopes: Scopes) {
        self.write
            .0
            .borrow_mut()
            .values
            .extend(scopes.write.0.borrow().values.clone());
    }
    pub fn get(&self, name: Symbol) -> Option<ScopedValue> {
        self.read.get(name)
    }
}

#[derive(Debug, Clone)]
pub enum ScopedValue {
    Scope(Scope),
    Value { scopes: Scopes, value: Value },
}

#[cfg(test)]
mod tests {
    use crate::symbol::symbol;

    use super::*;

    #[test]
    fn getting() {
        assert!(Scope::default().get(symbol("foo")).is_none());
    }
}
