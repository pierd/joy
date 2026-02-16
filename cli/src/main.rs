use std::io::Read;
use std::rc::Rc;

use joy_engine::lexer::lex;
use joy_engine::parser::parse;
use joy_engine::value::{BuiltinFn, Value};
use joy_engine::vm::VM;

fn create_vm() -> VM {
    let vm = VM::default();
    vm.set_value(
        "argc",
        Rc::new(move |vm: &mut VM| {
            vm.push(std::env::args().len());
            Ok(())
        }) as BuiltinFn,
    );
    vm.set_value(
        "argv",
        Rc::new(move |vm: &mut VM| {
            vm.push(Value::List(
                std::env::args().map(Into::into).collect::<Vec<_>>(),
            ));
            Ok(())
        }) as BuiltinFn,
    );
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
    println!("Stack: {:?}", vm.drain_stack());
}
