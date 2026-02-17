use crate::{
    value::{NumValue, Value},
    vm::VM,
};

#[macro_export]
macro_rules! impl_builtin_fn {
    ( $name:expr => ( $vm:ident $(, $arg:ident : $typ:ty )* ) $body:block ) => {
        |$vm: &mut $crate::vm::VM| -> Result<(), $crate::vm::VMError> {
            $(
                let $arg: $typ = $vm.pop()?.try_into()?;
            )*
            $body
            Ok(())
        }
    };
}

#[macro_export]
macro_rules! impl_builtins {
    ( $outer_vm:ident, { $( $name:ident ( $vm:ident $(, $arg:ident : $typ:ty )* ) $body:block )* } ) => {
        $(
            $outer_vm.set_value(
                stringify!($name),
                $crate::vm::BuiltinFn::new(
                    $crate::impl_builtin_fn!(stringify!($name) => ($vm $(,$arg:$typ)*) $body)
                )
            );
        )*
    };
}

macro_rules! impl_builtins_str {
    ( $outer_vm:ident, { $( $name:expr => ( $vm:ident $(, $arg:ident : $typ:ty )* ) $body:block )* } ) => {
        $(
            $outer_vm.set_value(
                $name,
                $crate::vm::BuiltinFn::new(
                    impl_builtin_fn!($name => ($vm $(,$arg:$typ)*) $body)
                )
            );
        )*
    };
}

fn add_bools(vm: &VM) {
    vm.set_value("false", Value::from(false));
    vm.set_value("true", Value::from(true));
}

fn add_stack(vm: &VM) {
    impl_builtins!(vm, {
        pop(vm) {
            vm.pop()?;
        }

        dup(vm, x: Value) {
            vm.push(x.clone());
            vm.push(x);
        }

        swap(vm, a: Value, b: Value) {
            vm.push(a);
            vm.push(b);
        }
    });
}

fn add_math(vm: &VM) {
    impl_builtins!(vm, {
        pred(vm, n: NumValue) {
            vm.push(n.map(|n| n - 1, |n| n - 1.0));
        }

        succ(vm, n: NumValue) {
            vm.push(n.map(|n| n + 1, |n| n + 1.0));
        }
    });

    impl_builtins_str!(vm, {
        "+" => (vm, x: NumValue, y: NumValue) {
            vm.push(NumValue::operation(x, y, |a, b| a + b, |a, b| a + b));
        }
        "-" => (vm, x: NumValue, y: NumValue) {
            vm.push(NumValue::operation(y, x, |a, b| a - b, |a, b| a - b));
        }
        "*" => (vm, x: NumValue, y: NumValue) {
            vm.push(NumValue::operation(x, y, |a, b| a * b, |a, b| a * b));
        }
        "/" => (vm, x: NumValue, y: NumValue) {
            vm.push(NumValue::operation(y, x, |a, b| a / b, |a, b| a / b));
        }
    });
}

pub fn add_builtins(vm: VM) -> VM {
    add_bools(&vm);
    add_stack(&vm);
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
