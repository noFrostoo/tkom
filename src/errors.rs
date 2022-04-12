use crate::types::{Token, Position, Number};
use std::error::Error;

pub enum ReadError{
    ReadingError(Box<dyn Error>),
}

pub enum LexerErrorKind {
    UnexpectedCharacter { actual: char, expected: String, position: Position },
    ReadError(Box<dyn Error>), //TODO Add bad number
    BadNumber(Number, u32) // second one is bad zero count
}

pub enum ErrorKind {
    Lexical(LexerErrorKind, Token, Position, String),
    Semantical(Token, Position, String),
    Syntactic(Token, Position, String),
    RunTime
}