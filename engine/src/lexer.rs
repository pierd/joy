/// Lexer
///
/// My opinionated lexer of Joy source code.
use thiserror::Error;

use crate::either::Either;
use crate::symbol::Symbol;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Keyword(Keyword),
    Symbol(Symbol),
    Literal(Literal),
    Dot,
    SemiColon,
    ListStart,
    ListEnd,
    SetStart,
    SetEnd,
}
impl From<Keyword> for Token {
    fn from(k: Keyword) -> Token {
        Token::Keyword(k)
    }
}
impl From<Symbol> for Token {
    fn from(s: Symbol) -> Token {
        Token::Symbol(s)
    }
}
impl From<Literal> for Token {
    fn from(l: Literal) -> Self {
        Token::Literal(l)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    // TODO: add missing keywords (https://wodan58.github.io/j09imp.html#TOC-4)
    Const,
    Define,
    Libra,
    Hide,
    In,
    End,
    Module,
    Private,
    Public,
    Inline,
}
impl Keyword {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "CONST" => Some(Keyword::Const),
            "DEFINE" => Some(Keyword::Define),
            "LIBRA" => Some(Keyword::Libra),
            "HIDE" => Some(Keyword::Hide),
            "IN" => Some(Keyword::In),
            "END" => Some(Keyword::End),
            "MODULE" => Some(Keyword::Module),
            "PRIVATE" => Some(Keyword::Private),
            "PUBLIC" => Some(Keyword::Public),
            "INLINE" => Some(Keyword::Inline),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Char(char),
    String(String),
    Float(f64),
    Integer(isize),
}
impl From<char> for Literal {
    fn from(c: char) -> Self {
        Literal::Char(c)
    }
}
impl From<String> for Literal {
    fn from(s: String) -> Self {
        Literal::String(s)
    }
}
impl From<f64> for Literal {
    fn from(f: f64) -> Self {
        Literal::Float(f)
    }
}
impl From<isize> for Literal {
    fn from(i: isize) -> Self {
        Literal::Integer(i)
    }
}
impl Literal {
    fn integer(negative: bool, value: isize) -> Self {
        Literal::Integer(value * if negative { -1 } else { 1 })
    }

    fn float(negative: bool, integer: isize, fractional: isize, fractional_len: usize) -> Self {
        Literal::Float(
            (integer as f64 + fractional as f64 / 10f64.powi(fractional_len as i32))
                * if negative { -1.0 } else { 1.0 },
        )
    }
}

#[derive(Debug, Error, PartialEq, Clone)]
pub enum LexerError {
    #[error("invalid character code: {0}")]
    InvalidCharacterCode(u32),
    #[error("unknown error: {0}")]
    Unknown(String),
}

struct EscapedCharResult {
    character: char,
    remaining_char: Option<char>,
}
impl From<char> for EscapedCharResult {
    fn from(c: char) -> Self {
        EscapedCharResult {
            character: c,
            remaining_char: None,
        }
    }
}
impl From<(char, char)> for EscapedCharResult {
    fn from((c, remaining_char): (char, char)) -> Self {
        EscapedCharResult {
            character: c,
            remaining_char: Some(remaining_char),
        }
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
enum EscapedCharState {
    #[default]
    Empty,
    Octal {
        val: u32,
        len: usize,
    },
    Hex {
        val: u32,
        len: usize,
    },
    Unicode {
        val: u32,
        len: usize,
    },
}
impl EscapedCharState {
    fn to_char(val: u32) -> Result<char, LexerError> {
        char::from_u32(val).ok_or(LexerError::InvalidCharacterCode(val))
    }

    fn handle_char(self, c: char) -> Result<Either<Self, EscapedCharResult>, LexerError> {
        match self {
            Self::Empty => match c {
                '0'..='7' => Ok(Either::Left(Self::Octal {
                    val: c.to_digit(8).expect("invalid octal"),
                    len: 1,
                })),
                'x' => Ok(Either::Left(Self::Hex { val: 0, len: 0 })),
                'u' => Ok(Either::Left(Self::Unicode { val: 0, len: 0 })),
                '\'' | '"' | '\\' => Ok(Either::Right(c.into())),
                'n' => Ok(Either::Right('\n'.into())),
                'r' => Ok(Either::Right('\r'.into())),
                't' => Ok(Either::Right('\t'.into())),
                _ => Err(LexerError::Unknown(format!("Unexpected character: {}", c))),
            },
            Self::Octal { val, len } => match c {
                '0'..='7' => {
                    let val = val * 8 + c.to_digit(8).expect("invalid octal");
                    let len = len + 1;
                    if len == 3 {
                        Ok(Either::Right(Self::to_char(val)?.into()))
                    } else {
                        Ok(Either::Left(Self::Octal { val, len }))
                    }
                }
                _ => Ok(Either::Right((Self::to_char(val)?, c).into())),
            },
            Self::Hex { val, len } => match c {
                '0'..='9' | 'A'..='F' | 'a'..='f' => {
                    let val = val * 16 + c.to_digit(16).expect("invalid hex");
                    let len = len + 1;
                    if len == 2 {
                        Ok(Either::Right(Self::to_char(val)?.into()))
                    } else {
                        Ok(Either::Left(Self::Hex { val, len }))
                    }
                }
                _ => Err(LexerError::Unknown(format!("Unexpected character: {}", c))),
            },
            Self::Unicode { val, len } => match c {
                '0'..='9' | 'A'..='F' | 'a'..='f' => {
                    let val = val * 16 + c.to_digit(16).expect("invalid hex");
                    let len = len + 1;
                    if len == 4 {
                        Ok(Either::Right(Self::to_char(val)?.into()))
                    } else {
                        Ok(Either::Left(Self::Unicode { val, len }))
                    }
                }
                _ => Err(LexerError::Unknown(format!("Unexpected character: {}", c))),
            },
        }
    }

    fn finish(self) -> Result<char, LexerError> {
        if let Self::Octal { val, .. } = self {
            Ok(Self::to_char(val)?)
        } else {
            Err(LexerError::Unknown(
                "Invalid escaped char state".to_string(),
            ))
        }
    }
}

fn parse_restricted_char_token(c: char) -> Option<Token> {
    match c {
        '.' => Some(Token::Dot),
        ';' => Some(Token::SemiColon),
        '[' => Some(Token::ListStart),
        ']' => Some(Token::ListEnd),
        '{' => Some(Token::SetStart),
        '}' => Some(Token::SetEnd),
        _ => None,
    }
}

#[derive(Debug, Default, Clone)]
enum LexerState {
    #[default]
    Empty,

    /// found '#', now skipping until '\n'
    InLineComment,

    /// found '(', now expecting '*' to start a comment
    FoundOpenParen,
    /// in a comment, accumulating until '*' is found
    InBlockComment,
    /// found '*' while in a comment, now expecting ')' to end a comment
    FoundCloseStarMaybe,

    InSymbolOrKeyword(String),
    /// found '\'', now expecting a character for the literal
    Quote,
    /// found '\' after a quote, now accumulating the escaped character
    QuoteEscapedChar(EscapedCharState),
    /// found '"', string literal, accumulating until '"'
    InString(String),
    /// found '\' in a string, now accumulating the escaped character
    InStringWithEscapedChar(String, EscapedCharState),

    /// found '-', now expecting a number for the literal or a symbol
    FoundMinus,
    InNumber {
        negative: bool,
        value: isize,
    },
    /// found '.' while in a number, either float or ended with an integer
    InNumberFoundDot {
        negative: bool,
        value: isize,
    },
    InFloat {
        negative: bool,
        integer: isize,
        fractional: isize,
        fractional_len: usize,
    },
}
impl LexerState {
    fn handle_char(self, c: char) -> Result<(Self, [Option<Token>; 2]), LexerError> {
        match self {
            Self::Empty => {
                if c.is_whitespace() {
                    Ok((self, [None, None]))
                } else if let Some(token) = parse_restricted_char_token(c) {
                    Ok((Self::Empty, [Some(token), None]))
                } else if c == '#' {
                    Ok((Self::InLineComment, [None, None]))
                } else if c == '(' {
                    Ok((Self::FoundOpenParen, [None, None]))
                } else if c == '\'' {
                    Ok((Self::Quote, [None, None]))
                } else if c == '"' {
                    Ok((Self::InString(String::new()), [None, None]))
                } else if c == '-' {
                    Ok((Self::FoundMinus, [None, None]))
                } else if let Some(digit) = c.to_digit(10) {
                    Ok((
                        Self::InNumber {
                            negative: false,
                            value: digit as isize,
                        },
                        [None, None],
                    ))
                } else {
                    Ok((Self::InSymbolOrKeyword(c.to_string()), [None, None]))
                }
            }
            Self::InLineComment => Ok((
                if c == '\n' {
                    Self::Empty
                } else {
                    Self::InLineComment
                },
                [None, None],
            )),
            Self::FoundOpenParen => Ok(if c == '*' {
                (Self::InBlockComment, [None, None])
            } else if c.is_whitespace() {
                (
                    Self::Empty,
                    [Some(Token::Symbol(Symbol::intern("("))), None],
                )
            } else if let Some(token) = parse_restricted_char_token(c) {
                (Self::Empty, [Some(token), None])
            } else {
                let mut s = "(".to_string();
                s.push(c);
                (Self::InSymbolOrKeyword(s), [None, None])
            }),
            Self::InBlockComment => {
                Ok((
                    if c == '*' {
                        // found '*', so we're now expecting ')' to end the comment
                        Self::FoundCloseStarMaybe
                    } else {
                        // still in the comment
                        self
                    },
                    [None, None],
                ))
            }
            Self::FoundCloseStarMaybe => {
                Ok((
                    if c == ')' {
                        // comment ended
                        Self::Empty
                    } else {
                        // no close paren after '*', so we're still in the comment
                        Self::InBlockComment
                    },
                    [None, None],
                ))
            }
            Self::InSymbolOrKeyword(mut s) => {
                if c.is_whitespace() || c == '#' || parse_restricted_char_token(c).is_some() {
                    Ok((
                        if c == '#' {
                            Self::InLineComment
                        } else {
                            Self::Empty
                        },
                        [
                            Some(if let Some(keyword) = Keyword::from_str(&s) {
                                Token::Keyword(keyword)
                            } else {
                                Token::Symbol(Symbol::intern(&s))
                            }),
                            parse_restricted_char_token(c),
                        ],
                    ))
                } else {
                    s.push(c);
                    Ok((Self::InSymbolOrKeyword(s), [None, None]))
                }
            }
            Self::Quote => {
                if c == '\\' {
                    Ok((
                        Self::QuoteEscapedChar(EscapedCharState::default()),
                        [None, None],
                    ))
                } else {
                    Ok((Self::Empty, [Some(Token::Literal(Literal::Char(c))), None]))
                }
            }
            Self::QuoteEscapedChar(state) => match state.handle_char(c)? {
                Either::Left(state) => Ok((Self::QuoteEscapedChar(state), [None, None])),
                Either::Right(EscapedCharResult {
                    character,
                    remaining_char,
                }) => {
                    let (state, last_token) = {
                        if let Some(remaining_char) = remaining_char {
                            let (state, [token0, token1]) =
                                Self::Empty.handle_char(remaining_char)?;
                            assert_eq!(token1, None);
                            (state, token0)
                        } else {
                            (Self::Empty, None)
                        }
                    };
                    Ok((
                        state,
                        [Some(Token::Literal(Literal::Char(character))), last_token],
                    ))
                }
            },
            Self::InString(mut s) => Ok(if c == '"' {
                (
                    Self::Empty,
                    [Some(Token::Literal(Literal::String(s))), None],
                )
            } else if c == '\\' {
                (
                    Self::InStringWithEscapedChar(s, EscapedCharState::default()),
                    [None, None],
                )
            } else {
                s.push(c);
                (Self::InString(s), [None, None])
            }),
            Self::InStringWithEscapedChar(mut s, state) => match state.handle_char(c)? {
                Either::Left(state) => Ok((Self::InStringWithEscapedChar(s, state), [None, None])),
                Either::Right(EscapedCharResult {
                    character,
                    remaining_char,
                }) => {
                    s.push(character);
                    let new_state = Self::InString(s);
                    if let Some(remaining_char) = remaining_char {
                        new_state.handle_char(remaining_char)
                    } else {
                        Ok((new_state, [None, None]))
                    }
                }
            },
            Self::FoundMinus => {
                if let Some(digit) = c.to_digit(10) {
                    Ok((
                        Self::InNumber {
                            negative: true,
                            value: digit as isize,
                        },
                        [None, None],
                    ))
                } else {
                    Self::InSymbolOrKeyword("-".to_string()).handle_char(c)
                }
            }
            Self::InNumber { negative, value } => {
                if let Some(digit) = c.to_digit(10) {
                    Ok((
                        Self::InNumber {
                            negative,
                            value: value * 10 + digit as isize,
                        },
                        [None, None],
                    ))
                } else if c == '.' {
                    Ok((Self::InNumberFoundDot { negative, value }, [None, None]))
                } else if c.is_whitespace() || c == '#' || parse_restricted_char_token(c).is_some()
                {
                    Ok((
                        if c == '#' {
                            Self::InLineComment
                        } else {
                            Self::Empty
                        },
                        [
                            Some(Token::Literal(Literal::integer(negative, value))),
                            parse_restricted_char_token(c),
                        ],
                    ))
                } else {
                    Err(LexerError::Unknown("Expected digit or '.'".to_string()))
                }
            }
            Self::InNumberFoundDot { negative, value } => {
                if let Some(digit) = c.to_digit(10) {
                    Ok((
                        Self::InFloat {
                            negative,
                            integer: value,
                            fractional: digit as isize,
                            fractional_len: 1,
                        },
                        [None, None],
                    ))
                } else if c.is_whitespace() || c == '#' {
                    Ok((
                        if c == '#' {
                            Self::InLineComment
                        } else {
                            Self::Empty
                        },
                        [
                            Some(Token::Literal(Literal::integer(negative, value))),
                            Some(Token::Dot),
                        ],
                    ))
                } else {
                    Err(LexerError::Unknown("Unexpected character".to_string()))
                }
            }
            Self::InFloat {
                negative,
                integer,
                fractional,
                fractional_len,
            } => {
                if let Some(digit) = c.to_digit(10) {
                    Ok((
                        Self::InFloat {
                            negative,
                            integer,
                            fractional: fractional * 10 + digit as isize,
                            fractional_len: fractional_len + 1,
                        },
                        [None, None],
                    ))
                } else if c.is_whitespace() || c == '#' || parse_restricted_char_token(c).is_some()
                {
                    Ok((
                        if c == '#' {
                            Self::InLineComment
                        } else {
                            Self::Empty
                        },
                        [
                            Some(Token::Literal(Literal::float(
                                negative,
                                integer,
                                fractional,
                                fractional_len,
                            ))),
                            parse_restricted_char_token(c),
                        ],
                    ))
                } else {
                    Err(LexerError::Unknown("Unexpected character".to_string()))
                }
            }
        }
    }

    fn finish(self) -> Result<[Option<Token>; 2], LexerError> {
        match self {
            Self::Empty => Ok([None, None]),
            Self::InLineComment => Ok([None, None]),
            Self::FoundOpenParen => Ok([Some(Token::Symbol(Symbol::intern("("))), None]),
            Self::InBlockComment => Err(LexerError::Unknown("InBlockComment".to_string())),
            Self::FoundCloseStarMaybe => {
                Err(LexerError::Unknown("FoundCloseStarMaybe".to_string()))
            }
            Self::InSymbolOrKeyword(s) => Self::finish_symbol_or_keyword(s),
            Self::Quote => Err(LexerError::Unknown("Quote".to_string())),
            Self::QuoteEscapedChar(state) => {
                Ok([Some(Token::Literal(Literal::Char(state.finish()?))), None])
            }
            Self::InString(_) => Err(LexerError::Unknown("InString".to_string())),
            Self::InStringWithEscapedChar(_, _) => {
                Err(LexerError::Unknown("InStringWithEscapedChar".to_string()))
            }
            Self::FoundMinus => Self::finish_symbol_or_keyword("-".to_string()),
            Self::InNumber { negative, value } => Ok([
                Some(Token::Literal(Literal::integer(negative, value))),
                None,
            ]),
            Self::InNumberFoundDot { negative, value } => Ok([
                Some(Token::Literal(Literal::integer(negative, value))),
                Some(Token::Dot),
            ]),
            Self::InFloat {
                negative,
                integer,
                fractional,
                fractional_len,
            } => Ok([
                Some(Token::Literal(Literal::float(
                    negative,
                    integer,
                    fractional,
                    fractional_len,
                ))),
                None,
            ]),
        }
    }

    fn finish_symbol_or_keyword(s: String) -> Result<[Option<Token>; 2], LexerError> {
        Ok(if let Some(keyword) = Keyword::from_str(&s) {
            [Some(Token::Keyword(keyword)), None]
        } else {
            [Some(Token::Symbol(Symbol::intern(&s))), None]
        })
    }
}

pub fn lex(input: impl AsRef<str>) -> Result<Vec<Token>, LexerError> {
    let mut tokens = Vec::new();
    let mut state = LexerState::default();
    for c in input.as_ref().chars() {
        let (new_state, new_tokens) = state.handle_char(c)?;
        state = new_state;
        tokens.extend(new_tokens.into_iter().filter_map(|token| token));
    }
    let new_tokens = state.finish()?;
    tokens.extend(new_tokens.into_iter().filter_map(|token| token));
    Ok(tokens)
}

struct LexerIterator<I: Iterator<Item = char>> {
    iter: I,
    state: Option<LexerState>,
    pending_token: Option<Token>,
}
impl<I: Iterator<Item = char>> LexerIterator<I> {
    fn new(iter: I) -> Self {
        Self {
            iter,
            state: Some(LexerState::default()),
            pending_token: None,
        }
    }
}
impl<I: Iterator<Item = char>> Iterator for LexerIterator<I> {
    type Item = Result<Token, LexerError>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.pending_token.take() {
            return Some(Ok(token));
        }
        let mut new_tokens = [None, None];
        while new_tokens.iter().all(|token| token.is_none()) {
            let state = self.state.take()?;
            new_tokens = if let Some(c) = self.iter.next() {
                let output = match state.handle_char(c) {
                    Ok(output) => output,
                    Err(e) => return Some(Err(e)),
                };
                self.state = Some(output.0);
                output.1
            } else {
                match state.finish() {
                    Ok(output) => output,
                    Err(e) => return Some(Err(e)),
                }
            };
        }
        let [token0, token1] = new_tokens;
        self.pending_token = token1;
        token0.map(Ok)
    }
}

pub trait LexableIterator: Sized + Iterator<Item = char> {
    fn lex(self) -> LexerIterator<Self> {
        LexerIterator::new(self)
    }
}
impl<I: Iterator<Item = char>> LexableIterator for I {}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! l {
        ($s:expr, $($expected:expr),*) => {
            assert_eq!(lex($s), Ok(vec![$($expected),*]));
        };
    }

    fn sym(s: &str) -> Token {
        Token::Symbol(Symbol::intern(s))
    }

    fn lit(l: impl Into<Literal>) -> Token {
        Token::Literal(l.into())
    }

    #[test]
    fn it_lexes_simple_keyword() {
        l!("CONST", Keyword::Const.into());
        l!("DEFINE", Keyword::Define.into());
        l!("LIBRA", Keyword::Libra.into());
        l!("HIDE", Keyword::Hide.into());
        l!("IN", Keyword::In.into());
        l!("END", Keyword::End.into());
        l!("MODULE", Keyword::Module.into());
        l!("PRIVATE", Keyword::Private.into());
        l!("PUBLIC", Keyword::Public.into());
        l!("INLINE", Keyword::Inline.into());
    }

    #[test]
    fn it_lexes_simple_symbol() {
        l!("foo", sym("foo"));
        l!("bar", sym("bar"));
        l!("baz", sym("baz"));
        l!("foo bar", sym("foo"), sym("bar"));
        l!("foo bar baz", sym("foo"), sym("bar"), sym("baz"));
    }

    #[test]
    fn it_lexes_symbols_and_keywords() {
        l!(
            "DEFINE foo bar baz HIDE foo IN baz END",
            Keyword::Define.into(),
            sym("foo"),
            sym("bar"),
            sym("baz"),
            Keyword::Hide.into(),
            sym("foo"),
            Keyword::In.into(),
            sym("baz"),
            Keyword::End.into()
        );
    }

    #[test]
    fn it_lexes_comments() {
        l!("( foo )", sym("("), sym("foo"), sym(")"));
        l!("(* foo *) bar", sym("bar"));
        l!(
            "( foo * bar * baz )",
            sym("("),
            sym("foo"),
            sym("*"),
            sym("bar"),
            sym("*"),
            sym("baz"),
            sym(")")
        );
        l!("(**) foo (* *)", sym("foo"));
        l!("# foo\nbar", sym("bar"));
        l!("xxx# foo\nbar", sym("xxx"), sym("bar"));
        l!("12# comment", lit(12));
        l!("12.5# comment", lit(12.5));
        l!("12.# comment", lit(12), Token::Dot);
    }

    #[test]
    fn it_lexes_quotes() {
        l!("'a", lit('a'));
        l!("'b test", lit('b'), sym("test"));
        l!("'c", lit('c'));
        l!("''", lit('\''));
        l!("'\\t", lit('\t'));
        l!("'\\n", lit('\n'));
        l!("'\\r", lit('\r'));
        l!("'\\0", lit('\0'));
        l!("'\\17", lit('\x0f'));
        l!("'\\017", lit('\x0f'));
        l!("'\\x0f", lit('\x0f'));
        l!("'\\u000f", lit('\x0f'));
    }

    #[test]
    fn it_lexes_strings() {
        l!("\"foo\"", lit("foo".to_string()));
        l!("\"bar baz\"", lit("bar baz".to_string()));
        l!("\"baz\"", lit("baz".to_string()));
        l!("\"\"", lit("".to_string()));
        l!("\"\\17\"", lit("\x0f".to_string()));
        l!("\"\\\"\\t\\017\"", lit("\"\t\x0f".to_string()));
    }

    #[test]
    fn it_lexes_single_char_tokens() {
        l!(".", Token::Dot);
        l!(";", Token::SemiColon);
        l!("[", Token::ListStart);
        l!("]", Token::ListEnd);
        l!("{", Token::SetStart);
        l!("}", Token::SetEnd);

        l!(".(", Token::Dot, Symbol::intern("(").into());
        l!(
            ".;[]{}",
            Token::Dot,
            Token::SemiColon,
            Token::ListStart,
            Token::ListEnd,
            Token::SetStart,
            Token::SetEnd
        );
    }

    #[test]
    fn it_lexes_numbers() {
        l!("1", lit(1));
        l!("-1", lit(-1));
        l!("1.", lit(1), Token::Dot);
        l!("1. 5", lit(1), Token::Dot, lit(5));
        l!("1.5", lit(1.5));
        l!("-1.5", lit(-1.5));
        l!("1.25", lit(1.25));
        l!("1.5.", lit(1.5), Token::Dot);
        l!("{1.5}", Token::SetStart, lit(1.5), Token::SetEnd);
        l!(
            "1;2;3",
            lit(1),
            Token::SemiColon,
            lit(2),
            Token::SemiColon,
            lit(3)
        );
        l!("1 2 3", lit(1), lit(2), lit(3));
    }

    #[test]
    fn it_lexes_simple_list() {
        l!("[1]", Token::ListStart, lit(1), Token::ListEnd);
        l!(
            "[1 2 3]",
            Token::ListStart,
            lit(1),
            lit(2),
            lit(3),
            Token::ListEnd
        );
    }

    #[test]
    fn it_lexes_malformed_list() {
        l!("1]", lit(1), Token::ListEnd);
        l!("[1", Token::ListStart, lit(1));
    }

    #[rustfmt::skip]
    #[test]
    fn it_lexes_tricky_cases() {
        l!(
            "DEFINE dup == 2 *.",
            Keyword::Define.into(), sym("dup"), sym("=="), lit(2), sym("*"), Token::Dot
        );
        l!(
            r#"
            CONST
            Ctrl-Q == 17;
            Ctrl-S == 19.
            "#,
            Keyword::Const.into(),
            sym("Ctrl-Q"), sym("=="), lit(17), Token::SemiColon,
            sym("Ctrl-S"), sym("=="), lit(19), Token::Dot
        )
    }

    #[rustfmt::skip]
    #[test]
    fn it_lexes_example_library() {
        l!(
            r#"
            LIBRA (* queue *)

            HIDE
            error   == "non_empty queue needed for " putchars putchars newline abort;
            prepare == [null] [swap reverse] [] ifte
            IN
            q-new   == [] [];
            q-add   == swap [swons] dip;
            q-addl  == swap [shunt] dip;
            q-null  == prepare dup null;
            q-front == prepare [null] ["q-front" error] [dup first] ifte;
            q-rem   == prepare [null] ["q-rem "  error] [unswons  ] ifte
            END.
            "#,
            Keyword::Libra.into(),
            Keyword::Hide.into(),
            sym("error"), sym("=="), lit("non_empty queue needed for ".to_string()), sym("putchars"), sym("putchars"), sym("newline"), sym("abort"), Token::SemiColon,
            sym("prepare"), sym("=="), Token::ListStart, sym("null"), Token::ListEnd, Token::ListStart, sym("swap"), sym("reverse"), Token::ListEnd, Token::ListStart, Token::ListEnd, sym("ifte"),
            Keyword::In.into(),
            sym("q-new"), sym("=="), Token::ListStart, Token::ListEnd, Token::ListStart, Token::ListEnd, Token::SemiColon,
            sym("q-add"), sym("=="), sym("swap"), Token::ListStart, sym("swons"), Token::ListEnd, sym("dip"), Token::SemiColon,
            sym("q-addl"), sym("=="), sym("swap"), Token::ListStart, sym("shunt"), Token::ListEnd, sym("dip"), Token::SemiColon,
            sym("q-null"), sym("=="), sym("prepare"), sym("dup"), sym("null"), Token::SemiColon,
            sym("q-front"), sym("=="), sym("prepare"), Token::ListStart, sym("null"), Token::ListEnd, Token::ListStart, lit("q-front".to_string()), sym("error"), Token::ListEnd, Token::ListStart, sym("dup"), sym("first"), Token::ListEnd, sym("ifte"), Token::SemiColon,
            sym("q-rem"), sym("=="), sym("prepare"), Token::ListStart, sym("null"), Token::ListEnd, Token::ListStart, lit("q-rem ".to_string()), sym("error"), Token::ListEnd, Token::ListStart, sym("unswons"), Token::ListEnd, sym("ifte"),
            Keyword::End.into(), Token::Dot
        )
    }

    #[rustfmt::skip]
    #[test]
    fn it_lexes_example_library_with_iterator() {
        let input =
            r#"
            LIBRA (* queue *)

            HIDE
            error   == "non_empty queue needed for " putchars putchars newline abort;
            prepare == [null] [swap reverse] [] ifte
            IN
            q-new   == [] [];
            q-add   == swap [swons] dip;
            q-addl  == swap [shunt] dip;
            q-null  == prepare dup null;
            q-front == prepare [null] ["q-front" error] [dup first] ifte;
            q-rem   == prepare [null] ["q-rem "  error] [unswons  ] ifte
            END.
            "#;
        let expected = vec![
            Keyword::Libra.into(),
            Keyword::Hide.into(),
            sym("error"), sym("=="), lit("non_empty queue needed for ".to_string()), sym("putchars"), sym("putchars"), sym("newline"), sym("abort"), Token::SemiColon,
            sym("prepare"), sym("=="), Token::ListStart, sym("null"), Token::ListEnd, Token::ListStart, sym("swap"), sym("reverse"), Token::ListEnd, Token::ListStart, Token::ListEnd, sym("ifte"),
            Keyword::In.into(),
            sym("q-new"), sym("=="), Token::ListStart, Token::ListEnd, Token::ListStart, Token::ListEnd, Token::SemiColon,
            sym("q-add"), sym("=="), sym("swap"), Token::ListStart, sym("swons"), Token::ListEnd, sym("dip"), Token::SemiColon,
            sym("q-addl"), sym("=="), sym("swap"), Token::ListStart, sym("shunt"), Token::ListEnd, sym("dip"), Token::SemiColon,
            sym("q-null"), sym("=="), sym("prepare"), sym("dup"), sym("null"), Token::SemiColon,
            sym("q-front"), sym("=="), sym("prepare"), Token::ListStart, sym("null"), Token::ListEnd, Token::ListStart, lit("q-front".to_string()), sym("error"), Token::ListEnd, Token::ListStart, sym("dup"), sym("first"), Token::ListEnd, sym("ifte"), Token::SemiColon,
            sym("q-rem"), sym("=="), sym("prepare"), Token::ListStart, sym("null"), Token::ListEnd, Token::ListStart, lit("q-rem ".to_string()), sym("error"), Token::ListEnd, Token::ListStart, sym("unswons"), Token::ListEnd, sym("ifte"),
            Keyword::End.into(), Token::Dot
        ];
        let actual = input.chars().lex().collect::<Result<Vec<Token>, LexerError>>().unwrap();
        assert_eq!(actual, expected);
    }
}
