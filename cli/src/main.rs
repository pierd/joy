use std::io::Read;

use joy_engine::impl_builtins;
use joy_engine::lexer::lex;
use joy_engine::parser::parse;
use joy_engine::value::Value;
use joy_engine::vm::VM;

fn create_vm() -> VM {
    let vm = VM::default();
    impl_builtins!(vm, {
        argc(vm) {
            vm.try_push(std::env::args().len())?;
        }

        argv(vm) {
            vm.push(std::env::args().map(Into::into).collect::<Vec<_>>());
        }
    });
    vm
}

fn main() {
    let mut buf = Vec::new();
    std::io::stdin().read_to_end(&mut buf).unwrap();
    let source = String::from_utf8(buf).unwrap();
    let tokens = lex(&source).unwrap();
    let program = parse(tokens).unwrap();
    let mut vm = create_vm();
    for cmd in program {
        vm.eval(cmd).unwrap();
    }
    println!("Stack: {}", Value::List(vm.drain_stack()));
}
