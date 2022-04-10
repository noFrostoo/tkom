use crate::types::{Token, Position};
use std::error::Error;

pub enum ReadError{
    Eof,
    NoData,
    ReadingError(Box<dyn Error>),
}

pub enum LexerErrorKind {
    EndOfFile,
    UnexpectedCharacter,
    ReadError(Box<dyn Error>)
}

pub enum ErrorKind {
    Lexical(LexerErrorKind, Token, Position, String),
    Semantical(Token, Position, String),
    Syntactic(Token, Position, String),
    RunTime
}