use std::fmt::Write;

use crate::{lexer::Literal, symbol::Symbol, vm::VMError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    Boolean,
    Char,
    String,
    Float,
    Integer,
    Symbol,
    QualifiedAccess,
    List,
    Set,
}
impl std::fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean => write!(f, "boolean"),
            Self::Char => write!(f, "char"),
            Self::String => write!(f, "string"),
            Self::Float => write!(f, "float"),
            Self::Integer => write!(f, "integer"),
            Self::Symbol => write!(f, "symbol"),
            Self::QualifiedAccess => write!(f, "qualified access"),
            Self::List => write!(f, "list"),
            Self::Set => write!(f, "set"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Boolean(bool),
    Char(char),
    String(String),
    Float(f64),
    Integer(isize),
    Symbol(Symbol),
    QualifiedAccess(Vec<Symbol>),
    List(Vec<Value>),
    // FIXME: something better for the set? specialize it maybe?
    Set(Vec<Value>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Boolean(b) => b.fmt(f),
            Value::Char(c) => f.write_char(*c),
            Value::String(s) => f.write_str(s),
            Value::Float(fl) => f.write_fmt(format_args!("{}", *fl)),
            Value::Integer(i) => i.fmt(f),
            Value::Symbol(symbol) => symbol.fmt(f),
            Value::QualifiedAccess(symbols) => {
                if let Some(first) = symbols.first() {
                    first.fmt(f)?
                }
                for x in symbols.iter().skip(1) {
                    f.write_char('.')?;
                    x.fmt(f)?;
                }
                Ok(())
            }
            Value::List(values) => {
                f.write_char('[')?;
                if let Some(first) = values.first() {
                    first.fmt(f)?
                }
                for x in values.iter().skip(1) {
                    f.write_char(' ')?;
                    x.fmt(f)?;
                }
                f.write_char(']')
            }
            Value::Set(values) => {
                f.write_char('{')?;
                if let Some(first) = values.first() {
                    first.fmt(f)?
                }
                for x in values.iter().skip(1) {
                    f.write_char(' ')?;
                    x.fmt(f)?;
                }
                f.write_char('}')
            }
        }
    }
}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Boolean(b1), Self::Boolean(b2)) => b1 == b2,
            (Self::Char(c1), Self::Char(c2)) => c1 == c2,
            (Self::String(s1), Self::String(s2)) => s1 == s2,
            (Self::Float(fl1), Self::Float(fl2)) => fl1 == fl2,
            (Self::Integer(i1), Self::Integer(i2)) => i1 == i2,
            (Self::Symbol(s1), Self::Symbol(s2)) => s1 == s2,
            (Self::QualifiedAccess(qa1), Self::QualifiedAccess(qa2)) => qa1 == qa2,
            (Self::List(l1), Self::List(l2)) => l1 == l2,
            (Self::Set(s1), Self::Set(s2)) => s1 == s2,
            _ => false,
        }
    }
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

macro_rules! simple_from {
    ($t:ty, $v:ident, $e:expr) => {
        impl From<$t> for Value {
            fn from($v: $t) -> Value {
                $e
            }
        }
    };
}
simple_from!(bool, v, Value::Boolean(v));
simple_from!(char, v, Value::Char(v));
simple_from!(String, v, Value::String(v));
simple_from!(f64, v, Value::Float(v));
simple_from!(isize, v, Value::Integer(v));
simple_from!(i8, v, Value::Integer(v as isize));
simple_from!(i16, v, Value::Integer(v as isize));
simple_from!(i32, v, Value::Integer(v as isize));
simple_from!(u8, v, Value::Integer(v as isize));
simple_from!(u16, v, Value::Integer(v as isize));
simple_from!(u32, v, Value::Integer(v as isize));
simple_from!(Symbol, v, Value::Symbol(v));
simple_from!(Vec<Value>, v, Value::List(v));

impl TryFrom<usize> for Value {
    type Error = VMError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(Self::Integer(value.try_into()?))
    }
}

impl Value {
    pub fn type_(&self) -> ValueType {
        match self {
            Self::Boolean(_) => ValueType::Boolean,
            Self::Char(_) => ValueType::Char,
            Self::String(_) => ValueType::String,
            Self::Float(_) => ValueType::Float,
            Self::Integer(_) => ValueType::Integer,
            Self::Symbol(_) => ValueType::Symbol,
            Self::QualifiedAccess(_) => ValueType::QualifiedAccess,
            Self::List(_) => ValueType::List,
            Self::Set(_) => ValueType::Set,
        }
    }

    pub fn list0() -> Self {
        Value::List(vec![])
    }
    pub fn list1(x: impl Into<Value>) -> Self {
        Value::List(vec![x.into()])
    }
    pub fn list2(x: impl Into<Value>, y: impl Into<Value>) -> Self {
        Value::List(vec![x.into(), y.into()])
    }
    pub fn list3(x: impl Into<Value>, y: impl Into<Value>, z: impl Into<Value>) -> Self {
        Value::List(vec![x.into(), y.into(), z.into()])
    }
}

impl TryFrom<Value> for Symbol {
    type Error = VMError;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Symbol(sym) => Ok(sym),
            v => Err(VMError::TypeError {
                found: v.type_(),
                expected: vec![ValueType::Symbol],
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum NumValue {
    Integer(isize),
    Float(f64),
}
impl TryFrom<Value> for NumValue {
    type Error = VMError;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Integer(i) => Ok(Self::Integer(i)),
            Value::Float(f) => Ok(Self::Float(f)),
            v => Err(VMError::TypeError {
                found: v.type_(),
                expected: vec![ValueType::Integer, ValueType::Float],
            }),
        }
    }
}
impl From<NumValue> for Value {
    fn from(value: NumValue) -> Self {
        match value {
            NumValue::Integer(i) => Value::Integer(i),
            NumValue::Float(f) => Value::Float(f),
        }
    }
}
impl NumValue {
    pub fn map<IntFn: Fn(isize) -> isize, FloatFn: Fn(f64) -> f64>(
        self,
        int_fn: IntFn,
        float_fn: FloatFn,
    ) -> NumValue {
        match self {
            Self::Integer(n) => Self::Integer(int_fn(n)),
            Self::Float(n) => Self::Float(float_fn(n)),
        }
    }

    pub fn operation<IntFn: Fn(isize, isize) -> isize, FloatFn: Fn(f64, f64) -> f64>(
        a: NumValue,
        b: NumValue,
        int_fn: IntFn,
        float_fn: FloatFn,
    ) -> NumValue {
        match (a, b) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(int_fn(a, b)),
            (Self::Integer(a), Self::Float(b)) => Self::Float(float_fn(a as f64, b)),
            (Self::Float(a), Self::Integer(b)) => Self::Float(float_fn(a, b as f64)),
            (Self::Float(a), Self::Float(b)) => Self::Float(float_fn(a, b)),
        }
    }
}

pub enum CollectionValue {
    List(Vec<Value>),
    Set(Vec<Value>),
}
