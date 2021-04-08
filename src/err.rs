use nom::error::{ContextError, ErrorKind, FromExternalError, ParseError};

#[derive(Debug)]
pub enum Err<I> {
    Unimp,
    Int(I, String),
    Nom(I, ErrorKind),
}

impl<I> ParseError<I> for Err<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        Err::Nom(input, kind)
    }
    fn append(_input: I, _kind: ErrorKind, other: Self) -> Self {
        other
    }
    // TODO
    // or
    // from_char
}

impl<I> FromExternalError<I, std::num::ParseIntError> for Err<I> {
    fn from_external_error(input: I, _kind: ErrorKind, e: std::num::ParseIntError) -> Self {
        Err::Int(input, format!("{}", e))
    }
}

impl<I> ContextError<I> for Err<I> {
    fn add_context(_input: I, ctx: &'static str, other: Self) -> Self {
        println!("add_context: {}", ctx);
        other
    }
}
