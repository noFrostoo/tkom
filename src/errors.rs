use crate::types::{Token, Position, Number};
use std::{process::exit, io::Error};

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnexpectedCharacter { actual: char, expected: String, position: Position },
    BadNumber { number: Number, zero_count: u32, position: Position }, // second one is bad zero count
    UnexpectedEOF{position: Position},
    MalformedUtf8{position: usize, bad_utf: usize},
    IoError(String)
}

// pub enum Error {
//     Lexical(ErrorKind, Token, Position, String),
//     Semantical(Token, Position, String),
//     Syntactic(Token, Position, String),
//     RunTime
// }

pub struct ErrorHandler;

impl ErrorHandler {
    pub fn report_error(err: ErrorKind) {
        Self::handle_error(err)
    }

    pub fn fatal(err: ErrorKind) -> ! { 
        Self::handle_error(err);
        exit(1);
    }

    pub fn handle_result<T>(res: Result<T, ErrorKind>) -> Result<T, ErrorKind> {
        match res {
            Ok(v) => Ok(v),
            Err(err) => { Self::handle_error(err.clone()); Err(err)},
        }
    }

    fn handle_error(err: ErrorKind) {
        let msg = match err {
            ErrorKind::UnexpectedCharacter { actual, expected, position } => {
                format!("Unexpected character at line: {} column: {}, expected: {}, got: {}, coding: {} ", position.line, position.column, expected, actual, actual as usize)
            },
            ErrorKind::BadNumber{number, zero_count, position } => {
                if zero_count > 0 {
                    format!("Number can't start with a zero, at line {}, column {}", position.line, position.column)    
                } else {
                    format!("Bad number literal {} at line {}, column {}", number.format(), position.line, position.column)
                }
            },
            ErrorKind::UnexpectedEOF { position } => {
                format!("Unexpected eof at line: {} column: {}",  position.line, position.column)
            },
            ErrorKind::MalformedUtf8 { position, bad_utf } => {
                format!("MalformedUtf8 at {}, got: {}", position, bad_utf)
            },
            ErrorKind::IoError(msg) => {
                format!("IO error: {}", msg)
            },
        };
        eprintln!("{}", msg);
    }
}

