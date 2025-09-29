use thiserror::Error;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Dot,
    SemiColon,
    Keyword(Keyword),
    Symbol(String),
    Literal(Literal),
    ListStart,
    ListEnd,
    SetStart,
    SetEnd,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Define,
    Libra,
    Hide,
    In,
    End,
}

impl Into<Token> for Keyword {
    fn into(self) -> Token {
        Token::Keyword(self)
    }
}

impl Keyword {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "DEFINE" => Some(Keyword::Define),
            "LIBRA" => Some(Keyword::Libra),
            "HIDE" => Some(Keyword::Hide),
            "IN" => Some(Keyword::In),
            "END" => Some(Keyword::End),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Char(char),
    String(String),
    Float(f64),
    Integer(i64),
}

impl Into<Token> for Literal {
    fn into(self) -> Token {
        Token::Literal(self)
    }
}

impl Into<Literal> for char {
    fn into(self) -> Literal {
        Literal::Char(self)
    }
}

impl Into<Literal> for String {
    fn into(self) -> Literal {
        Literal::String(self)
    }
}

impl Into<Literal> for f64 {
    fn into(self) -> Literal {
        Literal::Float(self)
    }
}

impl Into<Literal> for i64 {
    fn into(self) -> Literal {
        Literal::Integer(self)
    }
}

impl Literal {
    fn integer(negative: bool, value: i64) -> Self {
        Literal::Integer(value * if negative { -1 } else { 1 })
    }

    fn float(negative: bool, integer: i64, fractional: i64, fractional_len: usize) -> Self {
        Literal::Float((integer as f64 + fractional as f64 / 10f64.powi(fractional_len as i32)) * if negative { -1.0 } else { 1.0 })
    }
}

#[derive(Debug, Error, PartialEq, Clone)]
pub enum LexerError {
    #[error("unknown error: {0}")]
    Unknown(String),
}

#[derive(Debug, Default, Clone)]
enum LexerState {
    #[default]
    Empty,

    /// found '(', now expecting '*' to start a comment
    FoundOpenParen,
    /// in a comment, accumulating until '*' is found
    InComment,
    /// found '*' while in a comment, now expecting ')' to end a comment
    FoundCloseStarMaybe,
    
    InSymbolOrKeyword(String),
    /// found '\'' found, now expecting a character for the literal
    Quote,
    /// found '"', string literal, accumulating until '"'
    InString(String),

    /// found '-', now expecting a number for the literal or a symbol
    FoundMinus,
    InNumber { negative: bool, value: i64 },
    /// found '.' while in a number, either float or ended with an integer
    InNumberFoundDot { negative: bool, value: i64 },
    InFloat { negative: bool, integer: i64, fractional: i64, fractional_len: usize },
}

fn one_char_token(c: char) -> Option<Token> {
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

impl LexerState {
    fn handle_char(self, c: char) -> Result<(Self, [Option<Token>; 2]), LexerError> {
        match self {
            LexerState::Empty => {
                if c.is_whitespace() {
                    Ok((self, [None, None]))
                } else if let Some(token) = one_char_token(c) {
                    Ok((LexerState::Empty, [Some(token), None]))
                } else if c == '(' {
                    Ok((LexerState::FoundOpenParen, [None, None]))
                } else if c == '\'' {
                    Ok((LexerState::Quote, [None, None]))
                } else if c == '"' {
                    Ok((LexerState::InString(String::new()), [None, None]))
                } else if c == '-' {
                    Ok((LexerState::FoundMinus, [None, None]))
                } else if let Some(digit) = c.to_digit(10) {
                    Ok((LexerState::InNumber { negative: false, value: digit as i64 }, [None, None]))
                } else {
                    Ok((LexerState::InSymbolOrKeyword(c.to_string()), [None, None]))
                }
            },
            LexerState::FoundOpenParen => {
                Ok(
                    if c == '*' {
                        (LexerState::InComment, [None, None])
                    } else if c.is_whitespace() {
                        (LexerState::Empty, [Some(Token::Symbol("(".to_string())), None])
                    } else if let Some(token) = one_char_token(c) {
                        (LexerState::Empty, [Some(token), None])
                    } else {
                        let mut s = "(".to_string();
                        s.push(c);
                        (LexerState::InSymbolOrKeyword(s), [None, None])
                    }
                )
            },
            LexerState::InComment => {
                Ok((
                    if c == '*' {
                        // found '*', so we're now expecting ')' to end the comment
                        LexerState::FoundCloseStarMaybe
                    } else {
                        // still in the comment
                        self
                    },
                    [None, None]
                ))
            },
            LexerState::FoundCloseStarMaybe => {
                Ok((
                    if c == ')' {
                        // comment ended
                        LexerState::Empty
                    } else {
                        // no close paren after '*', so we're still in the comment
                        LexerState::InComment
                    },
                    [None, None]
                ))
            },
            LexerState::InSymbolOrKeyword(s) => {
                Self::continue_symbol_or_keyword(s, c)
            },
            LexerState::Quote => {
                Ok((LexerState::Empty, [Some(Token::Literal(Literal::Char(c))), None]))
            },
            LexerState::InString(mut s) => {
                Ok(
                    if c == '"' {
                        (LexerState::Empty, [Some(Token::Literal(Literal::String(s))), None])
                    } else {
                        s.push(c);
                        (LexerState::InString(s), [None, None])
                    }
                )
            },
            LexerState::FoundMinus => {
                if let Some(digit) = c.to_digit(10) {
                    Ok((Self::InNumber { negative: true, value: digit as i64 }, [None, None]))
                } else {
                    Self::continue_symbol_or_keyword("-".to_string(), c)
                }
            },
            LexerState::InNumber { negative, value } => {
                if let Some(digit) = c.to_digit(10) {
                    Ok((Self::InNumber { negative, value: value * 10 + digit as i64 }, [None, None]))
                } else if c == '.' {
                    Ok((Self::InNumberFoundDot { negative, value }, [None, None]))
                } else if let Some(token) = one_char_token(c) {
                    Ok((
                        Self::Empty,
                        [
                            Some(Token::Literal(Literal::integer(negative, value))),
                            Some(token),
                        ]
                    ))
                } else {
                    Err(LexerError::Unknown("Expected digit or '.'".to_string()))
                }
            },
            LexerState::InNumberFoundDot { negative, value } => {
                if let Some(digit) = c.to_digit(10) {
                    Ok((Self::InFloat { negative, integer: value, fractional: digit as i64, fractional_len: 1 }, [None, None]))
                } else if c.is_whitespace() {
                    Ok((
                        Self::Empty,
                        [
                            Some(Token::Literal(Literal::integer(negative, value))),
                            Some(Token::Dot),
                        ]
                    ))
                } else {
                    Err(LexerError::Unknown("Unexpected character".to_string()))
                }
            },
            LexerState::InFloat { negative, integer, fractional, fractional_len } => {
                if let Some(digit) = c.to_digit(10) {
                    Ok((Self::InFloat { negative, integer, fractional: fractional * 10 + digit as i64, fractional_len: fractional_len + 1 }, [None, None]))
                } else if c.is_whitespace() || one_char_token(c).is_some() {
                    Ok((
                        Self::Empty,
                        [
                            Some(Token::Literal(Literal::float(negative, integer, fractional, fractional_len))),
                            one_char_token(c),
                        ]
                    ))
                } else {
                    Err(LexerError::Unknown("Unexpected character".to_string()))
                }
            },
        }
    }

    fn continue_symbol_or_keyword(mut s: String, c: char) -> Result<(Self, [Option<Token>; 2]), LexerError> {
        if c.is_whitespace() || one_char_token(c).is_some() {
            Ok((
                Self::Empty,
                [
                    Some(
                        if let Some(keyword) = Keyword::from_str(&s) {
                            Token::Keyword(keyword)
                        } else {
                            Token::Symbol(s)
                        }
                    ),
                    one_char_token(c)
                ]
            ))
        } else {
            s.push(c);
            Ok((Self::InSymbolOrKeyword(s), [None, None]))
        }
    }

    fn finish(self) -> Result<[Option<Token>; 2], LexerError> {
        match self {
            Self::Empty => {
                Ok([None, None])
            },
            Self::FoundOpenParen => {
                Ok([Some(Token::Symbol("(".to_string())), None])
            },
            Self::InComment => {
                Err(LexerError::Unknown("InComment".to_string()))
            },
            Self::FoundCloseStarMaybe => {
                Err(LexerError::Unknown("FoundCloseStarMaybe".to_string()))
            },
            Self::InSymbolOrKeyword(s) => {
                Self::finish_symbol_or_keyword(s)
            },
            Self::Quote => {
                Err(LexerError::Unknown("Quote".to_string()))
            },
            Self::InString(s) => {
                Err(LexerError::Unknown("InString".to_string()))
            },
            Self::FoundMinus => {
                Self::finish_symbol_or_keyword("-".to_string())
            },
            Self::InNumber { negative, value } => {
                Ok([Some(Token::Literal(Literal::integer(negative, value))), None])
            },
            Self::InNumberFoundDot { negative, value } => {
                Ok([Some(Token::Literal(Literal::integer(negative, value))), Some(Token::Dot)])
            },
            Self::InFloat { negative, integer, fractional, fractional_len } => {
                Ok([Some(Token::Literal(Literal::float(negative, integer, fractional, fractional_len))), None])
            },
        }
    }

    fn finish_symbol_or_keyword(s: String) -> Result<[Option<Token>; 2], LexerError> {
        Ok(if let Some(keyword) = Keyword::from_str(&s) {
            [Some(Token::Keyword(keyword)), None]
        } else {
            [Some(Token::Symbol(s)), None]
        })
    }
}

pub fn lex(input: &str) -> Result<Vec<Token>, LexerError> {
    let mut tokens = Vec::new();
    let mut state = LexerState::default();
    for c in input.chars() {
        let (new_state, new_tokens) = state.handle_char(c)?;
        state = new_state;
        tokens.extend(new_tokens.into_iter().filter_map(|token| token));
    }
    let new_tokens = state.finish()?;
    tokens.extend(new_tokens.into_iter().filter_map(|token| token));
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! l {
        ($s:expr, $($expected:expr),*) => {
            assert_eq!(lex($s), Ok(vec![$($expected),*]));
        };
    }

    fn sym(s: &str) -> Token {
        Token::Symbol(s.to_string())
    }

    fn lit(l: impl Into<Literal>) -> Token {
        Token::Literal(l.into())
    }

    #[test]
    fn it_lexes_simple_keyword() {
        l!("DEFINE", Keyword::Define.into());
        l!("LIBRA", Keyword::Libra.into());
        l!("HIDE", Keyword::Hide.into());
        l!("IN", Keyword::In.into());
        l!("END", Keyword::End.into());
    }

    #[test]
    fn it_lexes_simple_symbol() {
        l!("foo", sym("foo"));
        l!("bar", sym("bar"));
        l!("baz", sym("baz"));
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
        l!("( foo * bar * baz )", sym("("), sym("foo"), sym("*"), sym("bar"), sym("*"), sym("baz"), sym(")"));
        l!("(**) foo (* *)", sym("foo"));
    }

    #[test]
    fn it_lexes_quotes() {
        l!("'a", lit('a'));
        l!("'b test", lit('b'), sym("test"));
        l!("'c", lit('c'));
        l!("''", lit('\''));
    }

    #[test]
    fn it_lexes_strings() {
        l!("\"foo\"", lit("foo".to_string()));
        l!("\"bar baz\"", lit("bar baz".to_string()));
        l!("\"baz\"", lit("baz".to_string()));
        l!("\"\"", lit("".to_string()));
    }

    #[test]
    fn it_lexes_single_char_tokens() {
        l!(".", Token::Dot);
        l!(";", Token::SemiColon);
        l!("[", Token::ListStart);
        l!("]", Token::ListEnd);
        l!("{", Token::SetStart);
        l!("}", Token::SetEnd);

        l!(".(", Token::Dot, Token::Symbol("(".to_string()));
        l!(".;[]{}", Token::Dot, Token::SemiColon, Token::ListStart, Token::ListEnd, Token::SetStart, Token::SetEnd);
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
    }
}
