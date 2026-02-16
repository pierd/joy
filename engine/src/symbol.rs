use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol(Rc<str>);

thread_local! {
    static SYMBOLS: RefCell<HashMap<String, Symbol>> = RefCell::new(HashMap::new());
}

impl Symbol {
    pub fn intern(s: impl AsRef<str>) -> Self {
        SYMBOLS.with(|symbols| {
            if let Some(symbol) = symbols.borrow().get(s.as_ref()).cloned() {
                return symbol;
            }
            let symbol = Symbol(s.as_ref().into());
            let symbol_key = s.as_ref().to_string();
            symbols.borrow_mut().insert(symbol_key, symbol.clone());
            symbol
        })
    }
}
