use thiserror::Error;

use crate::{lexer::LexerError, parser::ParserError, vm::VMError};

#[derive(Debug, Error, PartialEq, Clone)]
pub enum Error {
    #[error("{0}")]
    Lexer(LexerError),
    #[error("{0}")]
    Parser(ParserError),
    #[error("{0}")]
    VM(VMError),
}
macro_rules! impl_From_for_Error {
    ($t:ty, $v:ident, $e:expr) => {
        impl From<$t> for Error {
            fn from($v: $t) -> Error {
                $e
            }
        }
    };
}
impl_From_for_Error!(LexerError, e, Error::Lexer(e));
impl_From_for_Error!(ParserError, e, Error::Parser(e));
impl_From_for_Error!(VMError, e, Error::VM(e));
