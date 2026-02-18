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
///   - note: if there are no private_defs then given list is simply empty
///   - note2:
///     as input private_defs and public_defs are definitions but after parsing they are
///     regular Joy source code
///   - note3:
///     using "MODULE" for the builtin since it's already a restricted keyword so it won't
///     normally appear in Joy source code
///
/// Both of the above allows the modified Joy code to be executed directly because definitions
/// and modules stop being magical constructs and become regular Joy source code.
use thiserror::Error;

use crate::lexer::{Keyword, Token, TokenType};
use crate::symbol::{Symbol, symbol};
use crate::value::Value;

pub(crate) const MODULE_CREATION_BUILTIN: &str = "MODULE";

#[derive(Debug, Error, PartialEq, Clone)]
pub enum ParserError {
    #[error("unexpected token: {0:#?}")]
    UnexpectedToken(Token),
    #[error("unexpected token: {unexpected:#?}, expected tokens: {expected_tokens:#?}")]
    UnexpectedTokenExpectedTokens {
        unexpected: Token,
        expected_tokens: Vec<TokenType>,
    },
    #[error("unexpected end of input")]
    UnexpectedEndOfInput,
    #[error("unexpected end of input, expected tokens: {expected_tokens:#?}")]
    UnexpectedEndOfInputExpectedTokens { expected_tokens: Vec<TokenType> },
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
macro_rules! impl_From_for_ParserState {
    ($t:ty, $v:ident, $e:expr) => {
        impl From<$t> for ParserState {
            fn from($v: $t) -> Self {
                $e
            }
        }
    };
}
impl_From_for_ParserState!(
    SymbolOrQualifiedAccess,
    v,
    ParserState::SymbolOrQualifiedAccess(v)
);
impl_From_for_ParserState!(Collection, v, ParserState::Collection(v));
impl_From_for_ParserState!(Module, v, ParserState::Module(v));
impl_From_for_ParserState!(Definition, v, ParserState::Definition(v));

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
            values.push(symbol(".").into());
        }
        values
    }

    fn handle_token(&mut self, token: Token) -> Result<ParserStateResult, ParserError> {
        match token {
            Token::Symbol(symbol) => {
                if self.trailing_dot {
                    // symbols "." symbol -> another qualified access component
                    self.push(symbol);
                    Ok(Default::default())
                } else {
                    // no trailing dot but we got a new symbol - > consume qualified access state
                    Ok(ParserStateResult::done()
                        .reject(symbol)
                        .with_values(self.drain_all()))
                }
            }
            Token::Dot => {
                if self.trailing_dot {
                    // symbols "." "." -> consume qualified access state
                    Ok(ParserStateResult::done()
                        .with_values(self.drain_all())
                        .reject(token))
                } else {
                    // symbols "." -> just add the dot
                    self.trailing_dot = true;
                    Ok(ParserStateResult::continued())
                }
            }
            _ => {
                // symbols "."? anything -> drain qualified access state, emit dot if needed, reject token
                Ok(ParserStateResult::done()
                    .with_values(self.drain_all())
                    .reject(token))
            }
        }
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
    fn handle_sub_state_done(
        &mut self,
        values: Vec<Value>,
    ) -> Result<Option<Vec<Value>>, ParserError> {
        self.items.extend(values);
        Ok(None)
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
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
impl Module {
    fn public_unnamed() -> Self {
        Self {
            stage: ModuleStage::ReadingPublicDefs,
            ..Default::default()
        }
    }
    fn private_unnamed() -> Self {
        Self {
            stage: ModuleStage::ReadingPrivateDefs,
            ..Default::default()
        }
    }
    fn named() -> Self {
        Self {
            stage: ModuleStage::AwaitingName,
            ..Default::default()
        }
    }

    fn drain_values(&mut self) -> Vec<Value> {
        vec![
            Value::List(std::mem::take(&mut self.private_defs)),
            Value::List(std::mem::take(&mut self.public_defs)),
            Value::List(self.name.take().map(|s| vec![s.into()]).unwrap_or_default()),
            symbol(MODULE_CREATION_BUILTIN).into(),
        ]
    }

    fn handle_token(&mut self, token: Token) -> Result<ParserStateResult, ParserError> {
        if self.stage == ModuleStage::AwaitingName {
            if let Token::Symbol(sym) = token {
                self.name = Some(sym);
                self.stage = ModuleStage::ReadingPublicDefs;
                Ok(ParserStateResult::continued())
            } else {
                Err(ParserError::UnexpectedTokenExpectedTokens {
                    unexpected: token,
                    expected_tokens: vec![TokenType::Symbol],
                })
            }
        } else {
            match token {
                Token::Keyword(Keyword::Const)
                | Token::Keyword(Keyword::Define)
                | Token::Keyword(Keyword::Libra)
                | Token::Keyword(Keyword::Inline) => {
                    Ok(ParserStateResult::forward_to(Module::public_unnamed()))
                }
                Token::Keyword(Keyword::Hide) => {
                    Ok(ParserStateResult::forward_to(Module::private_unnamed()))
                }
                Token::Keyword(Keyword::Module) => {
                    Ok(ParserStateResult::forward_to(Module::named()))
                }
                Token::Dot | Token::Keyword(Keyword::End) => {
                    Ok(ParserStateResult::done().with_values(self.drain_values()))
                }
                Token::Keyword(Keyword::In) | Token::Keyword(Keyword::Public) => {
                    self.stage = ModuleStage::ReadingPublicDefs;
                    Ok(ParserStateResult::continued())
                }
                Token::Keyword(Keyword::Private) => {
                    self.stage = ModuleStage::ReadingPrivateDefs;
                    Ok(ParserStateResult::continued())
                }
                Token::SemiColon => Ok(ParserStateResult::continued()),
                Token::Symbol(s) => Ok(ParserStateResult::forward_to(Definition::named(s))),
                _ => Err(ParserError::UnexpectedToken(token)),
            }
        }
    }
    fn handle_sub_state_done(
        &mut self,
        values: Vec<Value>,
    ) -> Result<Option<Vec<Value>>, ParserError> {
        match self.stage {
            ModuleStage::AwaitingName => unreachable!(),
            ModuleStage::ReadingPrivateDefs => &mut self.private_defs,
            ModuleStage::ReadingPublicDefs => &mut self.public_defs,
        }
        .extend(values);
        Ok(None)
    }
    fn finish(self) -> Result<Vec<Value>, ParserError> {
        Err(ParserError::UnexpectedEndOfInputExpectedTokens {
            expected_tokens: if self.stage == ModuleStage::AwaitingName {
                vec![TokenType::Symbol]
            } else {
                vec![
                    TokenType::Symbol,
                    TokenType::Dot,
                    TokenType::Keyword(Keyword::End),
                ]
            },
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Definition {
    name: Symbol,
    stage: DefinitionStage,
    terms: Vec<Value>,
}
#[derive(Debug, PartialEq, Clone, Copy)]
enum DefinitionStage {
    AwaitingEquals,
    ReadingTerms,
}
impl Definition {
    fn named(name: impl Into<Symbol>) -> Self {
        Self {
            name: name.into(),
            stage: DefinitionStage::AwaitingEquals,
            terms: Default::default(),
        }
    }

    fn drain_values(&mut self) -> Vec<Value> {
        vec![
            Value::List(std::mem::take(&mut self.terms)),
            Value::List(vec![self.name.clone().into()]),
            symbol("==").into(),
        ]
    }

    fn handle_token(&mut self, token: Token) -> Result<ParserStateResult, ParserError> {
        match (self.stage, token) {
            (DefinitionStage::AwaitingEquals, Token::Symbol(s)) if s == symbol("==") => {
                self.stage = DefinitionStage::ReadingTerms;
                Ok(ParserStateResult::continued())
            }
            (
                DefinitionStage::ReadingTerms,
                token @ (Token::Dot | Token::SemiColon | Token::Keyword(Keyword::End)),
            ) => Ok(ParserStateResult::done()
                .reject(token)
                .with_values(self.drain_values())),
            (DefinitionStage::ReadingTerms, token) => {
                Ok(ParserStateResult::forward_to(ParserState::Empty).reject(token))
            }
            (_, token) => Err(ParserError::UnexpectedToken(token)),
        }
    }
    fn handle_sub_state_done(
        &mut self,
        values: Vec<Value>,
    ) -> Result<Option<Vec<Value>>, ParserError> {
        match self.stage {
            DefinitionStage::AwaitingEquals => unreachable!(),
            DefinitionStage::ReadingTerms => {
                self.terms.extend(values);
                Ok(None)
            }
        }
    }
    fn finish(self) -> Result<Vec<Value>, ParserError> {
        Err(match self.stage {
            DefinitionStage::AwaitingEquals => ParserError::UnexpectedEndOfInputExpectedTokens {
                expected_tokens: vec![
                    TokenType::Dot,
                    TokenType::SemiColon,
                    TokenType::Keyword(Keyword::End),
                ],
            },
            DefinitionStage::ReadingTerms => ParserError::UnexpectedEndOfInput,
        })
    }
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
                Token::Keyword(Keyword::Const)
                | Token::Keyword(Keyword::Define)
                | Token::Keyword(Keyword::Libra)
                | Token::Keyword(Keyword::Inline) => {
                    Ok(ParserStateResult::forward_to(Module::public_unnamed()))
                }
                Token::Keyword(Keyword::Hide) => {
                    Ok(ParserStateResult::forward_to(Module::private_unnamed()))
                }
                Token::Keyword(Keyword::Module) => {
                    Ok(ParserStateResult::forward_to(Module::named()))
                }
                Token::Symbol(symbol) => Ok(ParserStateResult::forward_to(
                    SymbolOrQualifiedAccess::new(symbol),
                )),
                Token::Literal(l) => Ok(ParserStateResult::done().with_value(l)),
                Token::Dot => Ok(ParserStateResult::done().with_value(symbol("."))),
                Token::SemiColon => {
                    // ; - shouldn't really happen (not present outside of definitions/modules)
                    Ok(ParserStateResult::done().with_value(symbol(";")))
                }
                Token::ListStart => Ok(ParserStateResult::forward_to(Collection::new_list())),
                Token::SetStart => Ok(ParserStateResult::forward_to(Collection::new_set())),
                _ => Err(ParserError::UnexpectedToken(token)),
            },
            Self::SymbolOrQualifiedAccess(sym_or_qa) => sym_or_qa.handle_token(token),
            Self::Collection(col) => {
                if token == col.expected_end() {
                    Ok(ParserStateResult::done().with_value(col.drain()))
                } else {
                    Ok(ParserStateResult::forward_to(Self::Empty).reject(token))
                }
            }
            Self::Module(module) => module.handle_token(token),
            Self::Definition(def) => def.handle_token(token),
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
            Self::Collection(col) => col.handle_sub_state_done(values),
            Self::Module(module) => module.handle_sub_state_done(values),
            Self::Definition(def) => def.handle_sub_state_done(values),
        }
    }

    fn finish(self) -> Result<Vec<Value>, ParserError> {
        match self {
            Self::Empty => Ok(vec![]),
            Self::SymbolOrQualifiedAccess(mut sym_or_qa) => Ok(sym_or_qa.drain_all()),
            Self::Collection(col) => Err(ParserError::UnexpectedEndOfInputExpectedTokens {
                expected_tokens: vec![col.expected_end().type_()],
            }),
            Self::Module(module) => module.finish(),
            Self::Definition(def) => def.finish(),
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

pub fn parse(tokens: impl IntoIterator<Item = Token>) -> Result<Vec<Value>, ParserError> {
    let mut results = Vec::new();
    let mut parser = Parser::default();
    for token in tokens {
        results.extend(parser.handle_token(token)?);
    }
    results.extend(parser.finish()?);
    Ok(results)
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Literal, lex};

    use super::*;

    macro_rules! p {
        ($tokens:expr, $($expected:expr),*) => {
            let results = parse($tokens).unwrap();
            assert_eq!(results, vec![$($expected),*]);
        };
    }

    fn tokens(s: &str) -> Vec<Token> {
        lex(s).unwrap()
    }

    fn value_sym(s: &str) -> Value {
        Value::Symbol(symbol(s))
    }

    fn value_qualified_access(s: &str) -> Value {
        Value::QualifiedAccess(s.split('.').map(symbol).collect())
    }

    fn value_lit(l: impl Into<Literal>) -> Value {
        l.into().into()
    }

    fn module_symbol() -> Value {
        symbol(MODULE_CREATION_BUILTIN).into()
    }

    #[test]
    fn parses_simple_symbol() {
        p!(tokens("foo"), value_sym("foo"));
    }

    #[test]
    fn parses_simple_qualified_access() {
        p!(tokens("foo.bar"), value_qualified_access("foo.bar"));
    }

    #[test]
    fn parses_simple_qualified_access_with_trailing_dot() {
        p!(
            tokens("foo.bar."),
            value_qualified_access("foo.bar"),
            value_sym(".")
        );
    }

    #[test]
    fn parses_simple_list() {
        p!(tokens("[1 2 3]"), Value::list3(1, 2, 3));
    }

    #[test]
    fn parses_simple_set() {
        p!(
            tokens("{1 2 3}"),
            Value::Set(vec![value_lit(1), value_lit(2), value_lit(3)])
        );
    }

    #[test]
    fn parses_simple_list_with_trailing_dot() {
        p!(tokens("[1 2 3]."), Value::list3(1, 2, 3), value_sym("."));
    }

    #[test]
    fn parses_nested_lists() {
        p!(
            tokens("[1 [2 3] 4]"),
            Value::list3(1, Value::list2(2, 3), 4,)
        );
        p!(
            tokens("[1 [2 [3]] 4] [] [5 6 7]"),
            Value::list3(1, Value::list2(2, Value::list1(3)), 4,),
            Value::list0(),
            Value::list3(5, 6, 7)
        );
    }

    #[test]
    fn parses_empty_modules() {
        p!(
            tokens("MODULE foo END"),
            Value::list0(),
            Value::list0(),
            Value::list1(symbol("foo")),
            module_symbol()
        );
        p!(
            tokens("MODULE foo."),
            Value::list0(),
            Value::list0(),
            Value::list1(symbol("foo")),
            module_symbol()
        );

        p!(
            tokens("LIBRA END"),
            Value::list0(),
            Value::list0(),
            Value::list0(),
            module_symbol()
        );
        p!(
            tokens("LIBRA."),
            Value::list0(),
            Value::list0(),
            Value::list0(),
            module_symbol()
        );

        p!(
            tokens("DEFINE END"),
            Value::list0(),
            Value::list0(),
            Value::list0(),
            module_symbol()
        );
        p!(
            tokens("DEFINE."),
            Value::list0(),
            Value::list0(),
            Value::list0(),
            module_symbol()
        );

        p!(
            tokens("HIDE IN END"),
            Value::list0(),
            Value::list0(),
            Value::list0(),
            module_symbol()
        );
        p!(
            tokens("HIDE IN."),
            Value::list0(),
            Value::list0(),
            Value::list0(),
            module_symbol()
        );

        p!(
            tokens("CONST END"),
            Value::list0(),
            Value::list0(),
            Value::list0(),
            module_symbol()
        );
        p!(
            tokens("CONST."),
            Value::list0(),
            Value::list0(),
            Value::list0(),
            module_symbol()
        );

        p!(
            tokens("INLINE END"),
            Value::list0(),
            Value::list0(),
            Value::list0(),
            module_symbol()
        );
        p!(
            tokens("INLINE."),
            Value::list0(),
            Value::list0(),
            Value::list0(),
            module_symbol()
        );
    }

    #[test]
    fn parses_simple_modules() {
        p!(
            tokens("DEFINE whatever == something END"),
            Value::list0(),
            Value::List(vec![
                Value::list1(symbol("something")),
                Value::list1(symbol("whatever")),
                value_sym("==")
            ]),
            Value::list0(),
            module_symbol()
        );

        p!(
            tokens("MODULE foo PRIVATE whatever == something END"),
            Value::list3(
                Value::list1(symbol("something")),
                Value::list1(symbol("whatever")),
                value_sym("==")
            ),
            Value::list0(),
            Value::list1(symbol("foo")),
            module_symbol()
        );
    }

    #[test]
    fn parses_nested_modules() {
        p!(
            tokens(
                r#"
                MODULE math
                    MODULE ops
                    PUBLIC
                        double == 2 *;
                        mul == *
                    END
                PUBLIC
                    square == dup ops.mul
                END
            "#
            ),
            Value::list0(),
            Value::List(vec![
                Value::list0(),
                Value::List(vec![
                    Value::list2(2, symbol("*")),
                    Value::list1(symbol("double")),
                    symbol("==").into(),
                    Value::list1(symbol("*")),
                    Value::list1(symbol("mul")),
                    symbol("==").into(),
                ]),
                Value::list1(symbol("ops")),
                module_symbol(),
                Value::list2(symbol("dup"), value_qualified_access("ops.mul")),
                Value::list1(symbol("square")),
                symbol("==").into(),
            ]),
            Value::list1(symbol("math")),
            module_symbol()
        );
    }
}
