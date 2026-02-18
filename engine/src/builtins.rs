use crate::{
    value::{NumValue, NumValueTupleExt, Value},
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

#[macro_export]
macro_rules! impl_builtins_str {
    ( $outer_vm:ident, { $( $name:expr => ( $vm:ident $(, $arg:ident : $typ:ty )* ) $body:block )* } ) => {
        $(
            $outer_vm.set_value(
                $name,
                $crate::vm::BuiltinFn::new(
                    $crate::impl_builtin_fn!($name => ($vm $(,$arg:$typ)*) $body)
                )
            );
        )*
    };
}

pub trait VMBuiltinsExt {
    fn init_all(&self);
    fn init_bools(&self);
    fn init_stack(&self);
    fn init_math(&self);
}
impl VMBuiltinsExt for VM {
    fn init_all(&self) {
        self.init_bools();
        self.init_stack();
        self.init_math();
    }

    fn init_bools(&self) {
        self.set_value("false", Value::from(false));
        self.set_value("true", Value::from(true));
    }

    fn init_stack(&self) {
        impl_builtins!(self, {
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

    fn init_math(&self) {
        impl_builtins!(self, {
            pred(vm, n: NumValue) {
                vm.push(n.map(|n| n - 1, |n| n - 1.0));
            }

            succ(vm, n: NumValue) {
                vm.push(n.map(|n| n + 1, |n| n + 1.0));
            }
        });

        impl_builtins_str!(self, {
            "+" => (vm, x: NumValue, y: NumValue) {
                vm.push((x, y).map(|a, b| a + b, |a, b| a + b));
            }
            "-" => (vm, x: NumValue, y: NumValue) {
                vm.push((y, x).map(|a, b| a - b, |a, b| a - b));
            }
            "*" => (vm, x: NumValue, y: NumValue) {
                vm.push((x, y).map(|a, b| a * b, |a, b| a * b));
            }
            "/" => (vm, x: NumValue, y: NumValue) {
                vm.push((y, x).map(|a, b| a / b, |a, b| a / b));
            }
        });
    }
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
