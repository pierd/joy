/// Parser
///
/// Converts stream of `Token`s into a stream of `Value`s that represent a Joy program.
///
/// Apart from interpreting common nestings (lists, sets) it resolves ambiguities around
/// qualified access to module components (because "." can be either qualified access or act as
/// END keyword).
///
/// Finally it resolves common syntax sugar cases:
/// - converting definitions from "atomic-symbol == term1 term2 ... termN" into
///   "[term1 term2 ... termN] [atomic-symbol] ==" which my implementation evaluates directly
/// - converting MODULE/DEFINE/LIBRA/HIDE into regular code wrapped into variable scoping builtins:
///   current "module" variants:
///   - "MODULE name PRIVATE ..private_defs.. PUBLIC ..public_defs.. END|."
///     becomes: "[..private_defs..] [..public_defs..] [name] MODULE"
///   - "LIBRA ..public_defs.. END|."
///   - "DEFINE ..public_defs.. END|."
///   - "HIDE ..private_defs.. IN ..public_defs.. END|."
///   - "CONST ..public_defs.. END|."
///   - "INLINE ..public_defs.. END|."
///     5 above become: "[..private_defs..] [..public_defs..] [] MODULE"
///   note: if there are no private_defs then given list is simply empty
///   note2: as input private_defs and public_defs are definitions but after parsing they are
///          regular Joy source code
///   note3: using "MODULE" for the builtin since it's already a restricted keyword so it won't
///          normally appear in Joy source code
///
/// Both of the above allows the modified Joy code to be executed directly because definitions
/// and modules stop being magical constructs and become regular Joy source code.
use thiserror::Error;

use crate::lexer::Token;
use crate::symbol::Symbol;
use crate::value::Value;

#[derive(Debug, Error, PartialEq, Clone)]
pub enum ParserError {
    #[error("unexpected token: {0:#?}")]
    UnexpectedToken(Token),
    #[error("unexpected end of input")]
    UnexpectedEndOfInput,
    #[error("unexpected end of input, expected tokens: {expected_tokens:#?}")]
    UnexpectedEndOfInputExpectedTokens { expected_tokens: Vec<Token> },
    #[error("unknown error: {0}")]
    Unknown(String),
}

#[derive(Debug, Default, PartialEq, Clone)]
enum ParserState {
    #[default]
    Empty,
    SymbolOrQualifiedAccess(SymbolOrQualifiedAccess),
    Collection(Collection),
    Module(Module),
    Definition(Definition),
}
impl From<SymbolOrQualifiedAccess> for ParserState {
    fn from(value: SymbolOrQualifiedAccess) -> Self {
        Self::SymbolOrQualifiedAccess(value)
    }
}
impl From<Collection> for ParserState {
    fn from(value: Collection) -> Self {
        Self::Collection(value)
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
struct SymbolOrQualifiedAccess {
    symbols: Vec<Symbol>,
    trailing_dot: bool,
}
impl SymbolOrQualifiedAccess {
    fn new(symbol: Symbol) -> Self {
        Self {
            symbols: vec![symbol],
            trailing_dot: false,
        }
    }
    fn push(&mut self, symbol: Symbol) {
        assert!(self.trailing_dot);
        self.symbols.push(symbol);
        self.trailing_dot = false;
    }

    fn drain(&mut self) -> Value {
        assert!(!self.symbols.is_empty());
        if self.symbols.len() == 1 {
            // one symbol
            Value::Symbol(
                self.symbols
                    .pop()
                    .expect("symbols should have at least one element"),
            )
        } else {
            // more symbols -> qualified access
            Value::QualifiedAccess(std::mem::take(&mut self.symbols))
        }
    }

    fn drain_all(&mut self) -> Vec<Value> {
        let mut values = vec![self.drain()];
        if self.trailing_dot {
            values.push(Symbol::intern(".").into());
        }
        values
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Collection {
    collection_type: CollectionType,
    items: Vec<Value>,
}
#[derive(Debug, PartialEq, Clone, Copy)]
enum CollectionType {
    List,
    Set,
}
impl Collection {
    fn new_list() -> Self {
        Self {
            collection_type: CollectionType::List,
            items: Default::default(),
        }
    }
    fn new_set() -> Self {
        Self {
            collection_type: CollectionType::Set,
            items: Default::default(),
        }
    }
    fn expected_end(&self) -> Token {
        match self.collection_type {
            CollectionType::List => Token::ListEnd,
            CollectionType::Set => Token::SetEnd,
        }
    }
    fn drain(&mut self) -> Value {
        let items = std::mem::take(&mut self.items);
        match self.collection_type {
            CollectionType::List => Value::List(items),
            CollectionType::Set => Value::Set(items),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Module {
    stage: ModuleStage,
    name: Option<Symbol>,
    private_defs: Vec<Value>,
    public_defs: Vec<Value>,
}
#[derive(Debug, Default, PartialEq, Clone)]
enum ModuleStage {
    #[default]
    AwaitingName,
    ReadingPrivateDefs,
    ReadingPublicDefs,
}

#[derive(Debug, PartialEq, Clone)]
struct Definition {
    name: Option<Symbol>,
    terms: Vec<Value>,
}
#[derive(Debug, Default, PartialEq, Clone)]
enum DefinitionStage {
    #[default]
    AwaitingName,
    AwaitingEquals,
    ReadingTerms,
}

#[derive(Debug, Default, PartialEq, Clone)]
struct ParserStateResult {
    decision: ParserStateResultDecision,
    values: Vec<Value>,
    rejected_token: Option<Token>,
}
#[derive(Debug, PartialEq, Clone)]
enum ParserStateResultDecision {
    Continue { forward_to: Option<ParserState> },
    Done,
}
impl std::default::Default for ParserStateResultDecision {
    fn default() -> Self {
        Self::Continue { forward_to: None }
    }
}
impl ParserStateResult {
    fn done() -> Self {
        Self {
            decision: ParserStateResultDecision::Done,
            ..Default::default()
        }
    }
    fn continued() -> Self {
        Self {
            decision: ParserStateResultDecision::Continue { forward_to: None },
            ..Default::default()
        }
    }
    fn forward_to(sub_state: impl Into<ParserState>) -> Self {
        Self {
            decision: ParserStateResultDecision::Continue {
                forward_to: Some(sub_state.into()),
            },
            ..Default::default()
        }
    }
    fn with_values(mut self, values: impl Into<Vec<Value>>) -> Self {
        self.values = values.into();
        self
    }
    fn with_value(mut self, value: impl Into<Value>) -> Self {
        self.values = vec![value.into()];
        self
    }
    fn reject(mut self, token: impl Into<Token>) -> Self {
        self.rejected_token = Some(token.into());
        self
    }
}
impl ParserState {
    fn handle_token(&mut self, token: Token) -> Result<ParserStateResult, ParserError> {
        match self {
            Self::Empty => match token {
                Token::Keyword(_) => {
                    // TODO: resolve all the syntax sugar into pure Joy code
                    todo!()
                }
                Token::Symbol(symbol) => Ok(ParserStateResult::forward_to(
                    Self::SymbolOrQualifiedAccess(SymbolOrQualifiedAccess::new(symbol)),
                )),
                Token::Literal(l) => Ok(ParserStateResult::done().with_value(l)),
                Token::Dot => Ok(ParserStateResult::done().with_value(Symbol::intern("."))),
                Token::SemiColon => {
                    // ; - shouldn't really happen (not present outside of definitions/modules)
                    Ok(ParserStateResult::done().with_value(Symbol::intern(";")))
                }
                Token::ListStart => Ok(ParserStateResult::forward_to(Collection::new_list())),
                Token::SetStart => Ok(ParserStateResult::forward_to(Collection::new_set())),
                _ => Err(ParserError::UnexpectedToken(token)),
            },
            Self::SymbolOrQualifiedAccess(sym_or_qa) => {
                match token {
                    Token::Symbol(symbol) => {
                        if sym_or_qa.trailing_dot {
                            // symbols "." symbol -> another qualified access component
                            sym_or_qa.push(symbol);
                            Ok(Default::default())
                        } else {
                            // no trailing dot but we got a new symbol - > consume qualified access state
                            Ok(ParserStateResult::done()
                                .reject(symbol)
                                .with_values(sym_or_qa.drain_all()))
                        }
                    }
                    Token::Dot => {
                        if sym_or_qa.trailing_dot {
                            // symbols "." "." -> consume qualified access state
                            Ok(ParserStateResult::done()
                                .with_values(sym_or_qa.drain_all())
                                .reject(token))
                        } else {
                            // symbols "." -> just add the dot
                            sym_or_qa.trailing_dot = true;
                            Ok(ParserStateResult::continued())
                        }
                    }
                    _ => {
                        // symbols "."? anything -> drain qualified access state, emit dot if needed, reject token
                        Ok(ParserStateResult::done()
                            .with_values(sym_or_qa.drain_all())
                            .reject(token))
                    }
                }
            }
            Self::Collection(col) => {
                if token == col.expected_end() {
                    Ok(ParserStateResult::done().with_value(col.drain()))
                } else {
                    Ok(ParserStateResult::forward_to(Self::Empty).reject(token))
                }
            }
            Self::Module(module) => {
                todo!()
            }
            Self::Definition(def) => {
                todo!()
            }
        }
    }

    // return Some(..) if this state is done too
    fn handle_sub_state_done(
        &mut self,
        values: Vec<Value>,
    ) -> Result<Option<Vec<Value>>, ParserError> {
        match self {
            Self::Empty => Ok(Some(values)),
            Self::SymbolOrQualifiedAccess(_) => {
                unreachable!()
            }
            Self::Collection(col) => {
                col.items.extend(values);
                Ok(None)
            }
            Self::Module(module) => {
                todo!()
            }
            Self::Definition(def) => {
                todo!()
            }
        }
    }

    fn finish(self) -> Result<Vec<Value>, ParserError> {
        match self {
            Self::Empty => Ok(vec![]),
            Self::SymbolOrQualifiedAccess(mut sym_or_qa) => Ok(sym_or_qa.drain_all()),
            Self::Collection(col) => Err(ParserError::UnexpectedEndOfInputExpectedTokens {
                expected_tokens: vec![col.expected_end()],
            }),
            Self::Module(module) => {
                todo!()
            }
            Self::Definition(def) => {
                todo!()
            }
        }
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
struct Parser {
    stack: Vec<ParserState>,
}
impl Parser {
    fn handle_token(&mut self, token: Token) -> Result<Vec<Value>, ParserError> {
        let mut resulting_values = Vec::new();
        let mut token_to_process = Some(token);
        while let Some(token) = token_to_process {
            // get the state that should handle the token
            let mut top_state = self.stack.pop().unwrap_or_default();
            // handle the token
            let ParserStateResult {
                decision,
                values,
                rejected_token,
            } = top_state.handle_token(token)?;
            let mut pending_done = match decision {
                ParserStateResultDecision::Done => {
                    // done -> handle the sub state in the parent state (new top state)
                    if let Some(new_top_state) = self.stack.last_mut() {
                        Some(new_top_state.handle_sub_state_done(values)?)
                    } else {
                        resulting_values.extend(values);
                        None
                    }
                }
                ParserStateResultDecision::Continue {
                    forward_to: sub_state,
                } => {
                    // not done, push back to stack
                    self.stack.push(top_state);
                    // push sub_state if present
                    if let Some(sub_state) = sub_state {
                        self.stack.push(sub_state);
                    }
                    None
                }
            };
            while let Some(Some(done)) = pending_done {
                pending_done = if let Some(top_state) = self.stack.last_mut() {
                    let new_pending_done = top_state.handle_sub_state_done(done)?;
                    if new_pending_done.is_some() {
                        // top_state is done too -> pop it
                        self.stack.pop();
                    }
                    Some(new_pending_done)
                } else {
                    resulting_values.extend(done);
                    None
                };
            }
            
            token_to_process = rejected_token;
        }
        Ok(resulting_values)
    }

    fn finish(mut self) -> Result<Vec<Value>, ParserError> {
        let mut resulting_values = Vec::new();
        while let Some(top_state) = self.stack.pop() {
            let values = top_state.finish()?;
            let mut pending_done = if let Some(new_top_state) = self.stack.last_mut() {
                new_top_state.handle_sub_state_done(values)?
            } else {
                resulting_values.extend(values);
                None
            };
            while let Some(done) = pending_done {
                pending_done = if let Some(top_state) = self.stack.last_mut() {
                    let new_pending_done = top_state.handle_sub_state_done(done)?;
                    if new_pending_done.is_some() {
                        // top_state is done too -> pop it
                        self.stack.pop();
                    }
                    new_pending_done
                } else {
                    resulting_values.extend(done);
                    None
                };
            }
        }
        Ok(resulting_values)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Literal, lex};

    use super::*;

    macro_rules! p {
        ($tokens:expr, $($expected:expr),*) => {
            let mut results = Vec::new();
            let mut parser = Parser::default();
            for token in $tokens {
                results.extend(parser.handle_token(token).unwrap());
            }
            results.extend(parser.finish().unwrap());
            assert_eq!(results, vec![$($expected),*]);
        };
    }

    fn token_sym(s: &str) -> Token {
        Token::Symbol(Symbol::intern(s))
    }

    fn token_lit(l: impl Into<Literal>) -> Token {
        Token::Literal(l.into())
    }

    fn tokens(s: &str) -> Vec<Token> {
        lex(s).unwrap()
    }

    fn value_sym(s: &str) -> Value {
        Value::Symbol(Symbol::intern(s))
    }

    fn value_qualified_access(s: &str) -> Value {
        Value::QualifiedAccess(s.split('.').map(Symbol::intern).collect())
    }

    fn value_lit(l: impl Into<Literal>) -> Value {
        l.into().into()
    }

    #[test]
    fn it_parses_simple_symbol() {
        p!(tokens("foo"), value_sym("foo"));
    }

    #[test]
    fn it_parses_simple_qualified_access() {
        p!(tokens("foo.bar"), value_qualified_access("foo.bar"));
    }

    #[test]
    fn it_parses_simple_qualified_access_with_trailing_dot() {
        p!(
            tokens("foo.bar."),
            value_qualified_access("foo.bar"),
            value_sym(".")
        );
    }

    #[test]
    fn it_parses_simple_list() {
        p!(
            tokens("[1 2 3]"),
            Value::List(vec![value_lit(1), value_lit(2), value_lit(3)])
        );
    }

    #[test]
    fn it_parses_simple_set() {
        p!(
            tokens("{1 2 3}"),
            Value::Set(vec![value_lit(1), value_lit(2), value_lit(3)])
        );
    }

    #[test]
    fn it_parses_simple_list_with_trailing_dot() {
        p!(
            tokens("[1 2 3]."),
            Value::List(vec![value_lit(1), value_lit(2), value_lit(3)]),
            value_sym(".")
        );
    }

    #[test]
    fn it_parses_nested_lists() {
        p!(
            tokens("[1 [2 3] 4]"),
            Value::List(vec![
                value_lit(1),
                Value::List(vec![value_lit(2), value_lit(3)]),
                value_lit(4)
            ])
        );
        p!(
            tokens("[1 [2 [3]] 4] [] [5 6 7]"),
            Value::List(vec![
                value_lit(1),
                Value::List(vec![value_lit(2), Value::List(vec![value_lit(3)])]),
                value_lit(4)
            ]),
            Value::List(vec![]),
            Value::List(vec![value_lit(5), value_lit(6), value_lit(7)])
        );
    }

    #[test]
    fn it_parses_empty_modules() {
        p!(
            tokens("MODULE foo END"),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![value_sym("foo")]),
            Value::List(vec![value_sym("MODULE")])
        );
        p!(
            tokens("MODULE foo."),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![value_sym("foo")]),
            Value::List(vec![value_sym("MODULE")])
        );

        p!(
            tokens("LIBRA END"),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![value_sym("MODULE")])
        );
        p!(
            tokens("LIBRA."),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![value_sym("MODULE")])
        );

        p!(
            tokens("DEFINE END"),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![value_sym("MODULE")])
        );
        p!(
            tokens("DEFINE."),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![value_sym("MODULE")])
        );

        p!(
            tokens("HIDE IN END"),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![value_sym("MODULE")])
        );
        p!(
            tokens("HIDE IN."),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![value_sym("MODULE")])
        );

        p!(
            tokens("CONST END"),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![value_sym("MODULE")])
        );
        p!(
            tokens("CONST."),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![value_sym("MODULE")])
        );

        p!(
            tokens("INLINE END"),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![value_sym("MODULE")])
        );
        p!(
            tokens("INLINE."),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![]),
            Value::List(vec![value_sym("MODULE")])
        );
    }

    #[test]
    fn it_parses_simple_modules() {
        p!(
            tokens("MODULE foo PRIVATE whatever == something END"),
            Value::List(vec![
                Value::List(vec![value_sym("something")]),
                Value::List(vec![value_sym("whatever")]),
                value_sym("==")
            ]),
            Value::List(vec![]),
            Value::List(vec![value_sym("foo")]),
            Value::List(vec![value_sym("MODULE")])
        );
    }
}
