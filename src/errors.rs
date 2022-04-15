use rust_decimal::Decimal;

use crate::types::{Position};
use std::{process::exit};

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnexpectedCharacter { actual: char, expected: String, position: Position },
    BadNumber { number: Decimal, zero_count: u32, position: Position }, // second one is bad zero count
    UnexpectedEOF{position: Position},
    MalformedUtf8{position: usize, bad_utf: usize},
    IoError(String),
    MaxIdentLenExceeded{position: Position},
    NoToken
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

    pub fn handle_result_option<T>(res: Option<Result<T, ErrorKind>>) -> Result<T, ErrorKind> {
        match res {
            Some(v) => match v {
                Ok(token) => Ok(token),
                Err(err) => { Self::handle_error(err.clone()); Err(err)},
            },
            
            None => Err(ErrorKind::NoToken),
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
                    format!("Bad number literal {} at line {}, column {}", number.to_string(), position.line, position.column)
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
            ErrorKind::MaxIdentLenExceeded { position } => {
                format!("Max ident length exceeded at line: {} column: {}",  position.line, position.column)
            },
            ErrorKind::NoToken => {
                format!("No token created")
            },
        };
        eprintln!("{}", msg);
    }
}

