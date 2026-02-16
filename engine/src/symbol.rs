use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Symbol(Rc<str>);
impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl<S: AsRef<str>> From<S> for Symbol {
    fn from(s: S) -> Self {
        symbol(s)
    }
}

thread_local! {
    static SYMBOLS: RefCell<HashMap<String, Symbol>> = RefCell::new(HashMap::new());
}

pub fn symbol(s: impl AsRef<str>) -> Symbol {
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
