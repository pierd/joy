use crate::{
    builtins::add_builtins,
    scope::{ScopedValue, Scopes},
    symbol::Symbol,
    value::{Value, ValueType},
};
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Clone)]
pub enum VMError {
    #[error("symbol not found: {0}")]
    SymbolNotFound(Symbol),
    #[error("module not found: {0:?}")]
    ModuleNotFound(Vec<Symbol>),
    #[error("stack underflow")]
    StackUnderflow,
    #[error("type error, found: {found}, expected: {expected:?}")]
    TypeError {
        found: ValueType,
        expected: Vec<ValueType>,
    },
    #[error("type error, found: {found}, expected quoted: {expected}")]
    TypeErrorExpectedQuotedValue {
        found: ValueType,
        expected: ValueType,
    },
    #[error("scoping violation")]
    ScopingViolation,
}

#[derive(Debug, Clone)]
pub struct VM {
    stack: Vec<Value>,
    scopes: Vec<Scopes>,
}
impl std::default::Default for VM {
    fn default() -> Self {
        add_builtins(Self {
            stack: Default::default(),
            scopes: vec![Default::default()],
        })
    }
}
impl VM {
    ///
    /// eval functions
    ///
    pub fn eval(&mut self, command: impl Into<Value>) -> Result<(), VMError> {
        match command.into() {
            Value::Symbol(sym) => {
                let ScopedValue::Value { value, scopes } =
                    self.current_scopes()
                        .get(sym.clone())
                        .ok_or(VMError::SymbolNotFound(sym.clone()))?
                else {
                    return Err(VMError::SymbolNotFound(sym));
                };
                self.eval_scoped_value(value, scopes)
            }
            Value::QualifiedAccess(mut path) => {
                // keep path for nicer debug message
                let mut path_so_far = Vec::new();

                // chop the last for the final value lookup
                let last = path
                    .pop()
                    .expect("qualified access should have at least 2 symbols path");

                // descent down to the final scope that represents the module
                let mut scope = self.current_scopes().read.clone();
                for sym in path {
                    path_so_far.push(sym.clone());
                    let Some(ScopedValue::Scope(sub_scope)) = scope.get(sym) else {
                        return Err(VMError::ModuleNotFound(path_so_far));
                    };
                    scope = sub_scope;
                }

                // lookup value in the final scope
                path_so_far.push(last.clone());
                let Some(ScopedValue::Value { scopes, value }) = scope.get(last.clone()) else {
                    return Err(VMError::ModuleNotFound(path_so_far));
                };
                self.eval_scoped_value(value, scopes)
            }
            Value::Code(commands) => self.eval_commands(commands),
            Value::Builtin(fun) => fun(self),
            v => {
                self.stack.push(v);
                Ok(())
            }
        }
    }

    pub fn eval_commands(
        &mut self,
        commands: impl IntoIterator<Item = Value>,
    ) -> Result<(), VMError> {
        for command in commands {
            self.eval(command)?;
        }
        Ok(())
    }

    fn eval_scoped_value(&mut self, command: Value, scopes: Scopes) -> Result<(), VMError> {
        self.scopes.push(scopes);
        let result = self.eval(command);
        self.scopes.pop();
        result
    }

    ///
    /// stack functions
    ///
    pub fn push(&mut self, v: impl Into<Value>) {
        self.stack.push(v.into());
    }
    pub fn pop(&mut self) -> Result<Value, VMError> {
        self.stack.pop().ok_or(VMError::StackUnderflow)
    }
    pub fn get_stack(&self) -> &[Value] {
        self.stack.as_ref()
    }
    pub fn drain_stack(&mut self) -> Vec<Value> {
        std::mem::take(&mut self.stack)
    }

    ///
    /// scope functions
    ///
    pub fn set_value(&self, name: impl Into<Symbol>, value: impl Into<Value>) {
        self.current_scopes().set(name.into(), value);
    }
    fn current_scopes(&self) -> Scopes {
        self.scopes.last().expect("scope-less VM").clone()
    }
    pub fn push_scopes(&mut self) {
        let top = self.current_scopes();
        self.scopes.push(Scopes::with_parent(top));
    }
    pub fn seal_scopes(&mut self) -> Result<(), VMError> {
        let popped = self.scopes.pop().ok_or(VMError::ScopingViolation)?;
        self.scopes.push(Scopes::with_sealed(popped));
        Ok(())
    }
    pub fn pop_scopes(&mut self, name: Option<Symbol>) -> Result<(), VMError> {
        let popped = self.scopes.pop().ok_or(VMError::ScopingViolation)?;
        if let Some(name) = name {
            self.current_scopes().set_scope(name, popped);
        } else {
            self.current_scopes().merge_in(popped);
        }
        Ok(())
    }
}

pub trait ValueExt {
    fn into_list(self) -> Result<Vec<Value>, VMError>;
    fn into_optional_quoted_value(self) -> Result<Option<Value>, VMError>;
    fn into_quoted_value(self) -> Result<Value, VMError>;
}
impl ValueExt for Value {
    fn into_list(self) -> Result<Vec<Value>, VMError> {
        match self {
            Value::List(items) => Ok(items),
            v => Err(VMError::TypeError {
                found: v.type_(),
                expected: vec![ValueType::List],
            }),
        }
    }
    fn into_optional_quoted_value(self) -> Result<Option<Value>, VMError> {
        match self {
            Value::List(items) if items.is_empty() => Ok(None),
            Value::List(mut items) if items.len() == 1 => Ok(Some(
                items.pop().expect("list should have exactly 1 element"),
            )),
            v => Err(VMError::TypeErrorExpectedQuotedValue {
                found: v.type_(),
                expected: ValueType::List,
            }),
        }
    }
    fn into_quoted_value(self) -> Result<Value, VMError> {
        match self {
            Value::List(mut items) if items.len() == 1 => {
                Ok(items.pop().expect("list should have exactly 1 element"))
            }
            v => Err(VMError::TypeErrorExpectedQuotedValue {
                found: v.type_(),
                expected: ValueType::List,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{symbol::symbol, value::Value};

    use super::ValueExt;

    macro_rules! e {
        ($cmds:expr, $($expected_stack:expr),*) => {
            let mut vm = $crate::vm::VM::default();
            for cmd in $cmds {
                vm.eval(cmd).expect("should evaluate");
            }
            assert_eq!(vm.drain_stack(), vec![$($expected_stack),*]);
        };
    }

    #[test]
    fn bools() {
        e!([symbol("true"), symbol("false")], true.into(), false.into());
    }

    #[test]
    fn value_into_list() {
        assert!(Value::Boolean(false).into_list().is_err());
        assert_eq!(Value::list0().into_list(), Ok(vec![]));
    }
}
