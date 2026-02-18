use std::rc::Rc;

use crate::{
    builtins::add_builtins,
    parser::MODULE_CREATION_BUILTIN,
    scope::{NamedValue, Namespace, Scope},
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
    #[error("out of range integer")]
    IntegerOutOfRange,
}
impl From<std::convert::Infallible> for VMError {
    fn from(x: std::convert::Infallible) -> Self {
        match x {}
    }
}
impl From<std::num::TryFromIntError> for VMError {
    fn from(_: std::num::TryFromIntError) -> Self {
        Self::IntegerOutOfRange
    }
}

type BuiltinFnInternal<T> = Rc<dyn Fn(&mut VM) -> Result<T, VMError>>;

#[derive(Clone)]
pub struct BuiltinFn(BuiltinFnInternal<()>);
impl std::fmt::Debug for BuiltinFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("BuiltinFn")
    }
}
impl BuiltinFn {
    pub fn new(f: impl Fn(&mut VM) -> Result<(), VMError> + 'static) -> Self {
        BuiltinFn(Rc::new(f))
    }
}

#[derive(Debug, Clone)]
pub struct Code {
    commands: Vec<Value>,
    capture: Scope,
}
#[derive(Debug, Clone)]
pub struct VM {
    stack: Vec<Value>,
    scopes: Vec<Scope>,
}
impl std::default::Default for VM {
    fn default() -> Self {
        let vm = Self {
            stack: Default::default(),
            scopes: vec![Default::default()],
        };

        vm.set_value(".", BuiltinFn::new(|_: &mut VM| {
            // FIXME
            // no-op
            Ok(())
        }));

        vm.set_value(
            "==",
            BuiltinFn::new(|vm: &mut VM| {
                let name: Symbol = vm.pop()?.into_quoted_value()?.try_into()?;
                let commands = vm.pop()?.into_list()?;
                let capture = vm.current_scope();
                vm.set_value(name, NamedValue::Code(Code { commands, capture }));
                Ok(())
            }),
        );

        vm.set_value(
            MODULE_CREATION_BUILTIN,
            BuiltinFn::new(|vm: &mut VM| {
                let name: Option<Symbol> = vm
                    .pop()?
                    .into_optional_quoted_value()?
                    .map(|v| v.try_into())
                    .transpose()?;
                let public = vm.pop()?.into_list()?;
                let private = vm.pop()?.into_list()?;

                // push private scope
                vm.push_sub_scope();
                vm.eval_commands(private)?;
                // push public scope
                vm.push_sub_scope();
                vm.eval_commands(public)?;
                // clean up extra scoping
                let public_scope = vm.pop_scope()?;
                vm.pop_scope()?;
                // expose public namespace
                if let Some(name) = name {
                    // attach namespace at its name
                    // FIXME: check for overwrites?
                    vm.set_value(name, public_scope.get_namespace());
                } else {
                    // incorporate namespace into the current scope
                    // FIXME: check for overwrites?
                    vm.current_scope().extend_namespace(public_scope.get_namespace());
                }
                Ok(())
            }),
        );

        add_builtins(vm)
    }
}
impl VM {
    ///
    /// eval functions
    ///
    pub fn eval(&mut self, command: impl Into<Value>) -> Result<(), VMError> {
        match command.into() {
            Value::Symbol(sym) => {
                let named_value = self
                    .current_scope()
                    .get(sym.clone())
                    .ok_or(VMError::SymbolNotFound(sym.clone()))?;
                if self.eval_named_value(named_value)?.is_some() {
                    Err(VMError::SymbolNotFound(sym))
                } else {
                    Ok(())
                }
            }
            Value::QualifiedAccess(mut path) => {
                // keep path for nicer debug message
                let mut path_so_far = Vec::new();

                // chop the last for the final value lookup
                let last = path
                    .pop()
                    .expect("qualified access should have at least 2 symbols path");

                // descent down to the final scope that represents the module
                let mut namespace = self.current_scope().get_namespace();
                for sym in path {
                    path_so_far.push(sym.clone());
                    if let Some(NamedValue::Namespace(sub_namespace)) = namespace.get(sym) {
                        namespace = sub_namespace;
                    } else {
                        return Err(VMError::ModuleNotFound(path_so_far));
                    };
                }

                // lookup value in the final scope
                path_so_far.push(last.clone());
                let named_value = namespace
                    .get(last.clone())
                    .ok_or(VMError::SymbolNotFound(last))?;

                // eval
                if self.eval_named_value(named_value)?.is_some() {
                    Err(VMError::ModuleNotFound(path_so_far))
                } else {
                    Ok(())
                }
            }
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

    fn eval_named_value(&mut self, v: NamedValue) -> Result<Option<Namespace>, VMError> {
        match v {
            NamedValue::Namespace(n) => return Ok(Some(n)),
            NamedValue::Value(v) => self.eval(v)?,
            NamedValue::Builtin(BuiltinFn(fun)) => fun(self)?,
            NamedValue::Code(code) => {
                self.scopes.push(code.capture);
                let mut result = Ok(());
                for command in code.commands {
                    result = self.eval(command);
                    if result.is_err() {
                        break;
                    }
                }
                self.scopes.pop();
                result?;
            }
        }
        Ok(None)
    }

    ///
    /// stack functions
    ///
    pub fn push(&mut self, v: impl Into<Value>) {
        self.stack.push(v.into());
    }
    pub fn try_push<T: TryInto<Value>>(&mut self, v: T) -> Result<(), T::Error> {
        self.stack.push(v.try_into()?);
        Ok(())
    }
    pub fn pop(&mut self) -> Result<Value, VMError> {
        self.stack.pop().ok_or(VMError::StackUnderflow)
    }
    pub fn peek(&mut self) -> Result<&Value, VMError> {
        self.stack.last().ok_or(VMError::StackUnderflow)
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
    pub fn set_value(
        &self,
        name: impl Into<Symbol>,
        value: impl Into<NamedValue>,
    ) -> Option<NamedValue> {
        self.current_scope().set(name, value)
    }
    fn current_scope(&self) -> Scope {
        self.scopes.last().expect("scope-less VM").clone()
    }
    fn push_sub_scope(&mut self) -> Scope {
        let scope = Scope::with_parent(self.current_scope());
        self.scopes.push(scope.clone());
        scope
    }
    fn pop_scope(&mut self) -> Result<Scope, VMError> {
        self.scopes.pop().ok_or(VMError::ScopingViolation)
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
    use crate::{
        errors::Error,
        lexer::lex,
        parser::parse,
        symbol::symbol,
        value::Value,
    };

    use super::*;

    macro_rules! e {
        ($cmds:expr, $($expected_stack:expr),*) => {
            let mut vm = $crate::vm::VM::default();
            for cmd in $cmds {
                vm.eval(cmd).expect("should evaluate");
            }
            assert_eq!(vm.drain_stack(), vec![$($expected_stack),*]);
        };
    }

    fn run(code: &str) -> Result<Vec<Value>, Error> {
        let tokens = lex(code)?;
        let program = parse(tokens)?;
        let mut vm = VM::default();
        for cmd in program {
            vm.eval(cmd)?;
        }
        Ok(vm.drain_stack())
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

    #[test]
    fn simple_declare() {
        assert_eq!(
            run(r#"
                DEFINE
                    square == dup *
                END.

                2 square.
            "#).unwrap(),
            vec![4.into()]
        )
    }

    #[test]
    fn simple_module() {
        assert_eq!(
            run(r#"
                MODULE test
                PUBLIC
                    square == dup *
                END.

                2 test.square.
            "#).unwrap(),
            vec![4.into()]
        )
    }

    #[test]
    fn nested_modules() {
        assert_eq!(
            run(r#"
                MODULE math
                    MODULE ops
                    PUBLIC
                        double == 2 *;
                        mul == *
                    END
                PUBLIC
                    square == dup ops.mul
                END.

                5 math.ops.double
                2 math.square
            "#).unwrap(),
            vec![10.into(), 4.into()]
        )
    }
}
