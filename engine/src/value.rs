use crate::{lexer::Literal, symbol::Symbol};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Boolean(bool),
    Char(char),
    String(String),
    Float(f64),
    Integer(isize),
    Symbol(Symbol),
    QualifiedAccess(Vec<Symbol>),
    List(Vec<Value>),
    Set(Vec<Value>),
}

impl From<Literal> for Value {
    fn from(l: Literal) -> Self {
        match l {
            Literal::Char(c) => Self::Char(c),
            Literal::String(s) => Self::String(s),
            Literal::Float(f) => Self::Float(f),
            Literal::Integer(i) => Self::Integer(i),
        }
    }
}

impl From<Symbol> for Value {
    fn from(s: Symbol) -> Self {
        Self::Symbol(s)
    }
}