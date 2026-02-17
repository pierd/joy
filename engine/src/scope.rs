use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    symbol::Symbol,
    value::Value,
    vm::{BuiltinFn, Code},
};

#[derive(Debug, Clone)]
pub enum NamedValue {
    Value(Value),
    Namespace(Namespace),
    Builtin(BuiltinFn),
    Code(Code),
}
macro_rules! impl_From_for_NamedValue {
    ($t:ty, $v:ident, $e:expr) => {
        impl From<$t> for NamedValue {
            fn from($v: $t) -> NamedValue {
                $e
            }
        }
    };
}
impl_From_for_NamedValue!(Value, v, NamedValue::Value(v));
impl_From_for_NamedValue!(Namespace, v, NamedValue::Namespace(v));
impl_From_for_NamedValue!(BuiltinFn, v, NamedValue::Builtin(v));
impl_From_for_NamedValue!(Code, v, NamedValue::Code(v));

#[derive(Debug, Default, Clone)]
pub struct Namespace(Rc<RefCell<HashMap<Symbol, NamedValue>>>);
impl IntoIterator for Namespace {
    type Item = (Symbol, NamedValue);
    type IntoIter = <HashMap::<Symbol, NamedValue> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.0.borrow().clone().into_iter()
    }
}
impl Namespace {
    pub fn set(&self, name: impl Into<Symbol>, value: impl Into<NamedValue>) -> Option<NamedValue> {
        self.0.borrow_mut().insert(name.into(), value.into())
    }
    pub fn get(&self, name: Symbol) -> Option<NamedValue> {
        self.0.borrow().get(&name).cloned()
    }
    pub fn extend(&self, named_values: impl IntoIterator<Item = (Symbol, NamedValue)>) {
        self.0.borrow_mut().extend(named_values);
    }
}

#[derive(Debug, Default, Clone)]
pub struct Scope(Rc<RefCell<ScopeInternal>>);
#[derive(Debug, Default, Clone)]
struct ScopeInternal {
    parent: Option<Scope>,
    namespace: Namespace,
}
impl Scope {
    pub fn with_parent(parent: Self) -> Self {
        Self(Rc::new(RefCell::new(ScopeInternal {
            parent: Some(parent),
            ..Default::default()
        })))
    }

    pub fn get_namespace(&self) -> Namespace {
        self.0.borrow().namespace.clone()
    }
    pub fn extend_namespace(&self, named_values: impl IntoIterator<Item = (Symbol, NamedValue)>) {
        self.0.borrow().namespace.extend(named_values);
    }

    pub fn set(&self, name: impl Into<Symbol>, value: impl Into<NamedValue>) -> Option<NamedValue> {
        self.0.borrow_mut().namespace.set(name, value)
    }

    pub fn get(&self, name: Symbol) -> Option<NamedValue> {
        self.0
            .borrow()
            .namespace
            .get(name.clone())
            // .. or try getting from the parent
            .or_else(|| self.0.borrow().parent.as_ref().and_then(|p| p.get(name)))
    }
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
