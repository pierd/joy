use crate::{
    symbol::Symbol,
    value::{NumValue, Value},
    vm::{VM, ValueExt},
};

pub(crate) const MODULE_CREATION_BUILTIN: &str = "MODULE";

macro_rules! fun {
    ($vm:ident, $f:expr) => {
        $crate::value::Value::Builtin(std::rc::Rc::new(
            move |$vm: &mut crate::vm::VM| -> Result<(), $crate::vm::VMError> { $f },
        ))
    };
}

fn add_bools(vm: &VM) {
    vm.set_value("false", false);
    vm.set_value("true", true);
}

fn add_stack(vm: &VM) {
    vm.set_value(
        "pop",
        fun!(vm, {
            vm.pop()?;
            Ok(())
        }),
    );
    vm.set_value(
        "dup",
        fun!(vm, {
            let v = vm.pop()?;
            vm.push(v.clone());
            vm.push(v);
            Ok(())
        }),
    );
    vm.set_value(
        "swap",
        fun!(vm, {
            let a = vm.pop()?;
            let b = vm.pop()?;
            vm.push(a);
            vm.push(b);
            Ok(())
        }),
    );
}

fn add_modules(vm: &VM) {
    vm.set_value(
        "==",
        fun!(vm, {
            let name: Symbol = vm.pop()?.into_quoted_value()?.try_into()?;
            let commands = vm.pop()?.into_list()?;
            vm.set_value(name, Value::Code(commands));
            Ok(())
        }),
    );
    vm.set_value(
        MODULE_CREATION_BUILTIN,
        fun!(vm, {
            let name = vm
                .pop()?
                .into_optional_quoted_value()?
                .map(|v| v.try_into())
                .transpose()?;
            let public = vm.pop()?.into_list()?;
            let private = vm.pop()?.into_list()?;
            // FIXME: rethink this scoping
            vm.push_scopes();
            vm.eval_commands(private)?;
            vm.seal_scopes()?;
            vm.eval_commands(public)?;
            vm.pop_scopes(name)?;
            Ok(())
        }),
    );
}

fn add_math(vm: &VM) {
    vm.set_value(
        "pred",
        fun!(vm, {
            let n: NumValue = vm.pop()?.try_into()?;
            vm.push(n.map(|n| n - 1, |n| n - 1.0));
            Ok(())
        }),
    );
    vm.set_value(
        "succ",
        fun!(vm, {
            let n: NumValue = vm.pop()?.try_into()?;
            vm.push(n.map(|n| n + 1, |n| n + 1.0));
            Ok(())
        }),
    );
    vm.set_value(
        "+",
        fun!(vm, {
            let a = vm.pop()?.try_into()?;
            let b = vm.pop()?.try_into()?;
            vm.push(NumValue::operation(a, b, |a, b| a + b, |a, b| a + b));
            Ok(())
        }),
    );
    vm.set_value(
        "*",
        fun!(vm, {
            let a = vm.pop()?.try_into()?;
            let b = vm.pop()?.try_into()?;
            vm.push(NumValue::operation(a, b, |a, b| a * b, |a, b| a * b));
            Ok(())
        }),
    );
}

pub fn add_builtins(vm: VM) -> VM {
    add_bools(&vm);
    add_stack(&vm);
    add_modules(&vm);
    add_math(&vm);
    vm
}

#[cfg(test)]
mod tests {
    use crate::{symbol::symbol, value::Value};

    macro_rules! e {
        ($cmds:expr, $($expected_stack:expr),*) => {
            let mut vm = $crate::vm::VM::default();
            for cmd in $cmds {
                vm.eval(cmd).expect("should evaluate");
            }
            assert_eq!(vm.drain_stack(), vec![$($expected_stack),*]);
        };
    }

    fn v(x: impl Into<Value>) -> Value {
        x.into()
    }

    #[test]
    fn bools() {
        e!([symbol("true")], true.into());
        e!([symbol("false")], false.into());
    }

    #[test]
    fn stack() {
        e!([v(1), v(2), v(symbol("pop"))], 1.into());
        e!([v(1), v(symbol("dup"))], 1.into(), 1.into());
        e!([v(1), v(2), v(symbol("swap"))], 2.into(), 1.into());
    }

    #[test]
    fn numbers() {
        e!([v(2), v(3), v(symbol("*"))], v(6));
    }
}
