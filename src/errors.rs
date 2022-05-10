use crate::types::{Position, TokenKind};
use std::{io::Error, process::exit};

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnexpectedCharacter {
        actual: char,
        expected: String,
        position: Position,
    },
    UnexpectedEOF {
        position: Position,
    },
    MalformedUtf8 {
        position: usize,
        bad_utf: usize,
    },
    IoError(String),
    MaxLenExceeded {
        position: Position,
        max_len: usize,
    },
    FractionTooBig {
        position: Position,
    },
    NumberTooBig {
        position: Position,
    },
    NoToken,
    SyntaxError {
        position: Position,
        expected_kind: TokenKind,
        got: TokenKind,
    },
    ObjectExpected {
        position: Position,
        got: TokenKind,
    },
}

pub struct ErrorHandler;

impl ErrorHandler {
    pub fn report_error(err: ErrorKind) {
        Self::handle_error(err)
    }

    pub fn fatal(err: ErrorKind) -> ! {
        Self::handle_error(err);
        exit(1);
    }

    pub fn io_error(err: Error) -> ! {
        eprintln!("Error opening the file: {}", err);
        exit(2);
    }

    fn handle_error(err: ErrorKind) {
        let msg = match err {
            ErrorKind::UnexpectedCharacter {
                actual,
                expected,
                position,
            } => {
                format!("Unexpected character at line: {} column: {}, expected: {}, got: {}, coding: {} ", position.line, position.column, expected, actual, actual as usize)
            }
            ErrorKind::UnexpectedEOF { position } => {
                format!(
                    "Unexpected eof at line: {} column: {}",
                    position.line, position.column
                )
            }
            ErrorKind::MalformedUtf8 { position, bad_utf } => {
                format!("MalformedUtf8 at {}, got: {}", position, bad_utf)
            }
            ErrorKind::IoError(msg) => {
                format!("IO error: {}", msg)
            }
            ErrorKind::MaxLenExceeded { position, max_len } => {
                format!(
                    "Max length (max length: {}) exceeded at line: {} column: {}",
                    max_len, position.line, position.column
                )
            }
            ErrorKind::NoToken => {
                format!("No token created")
            }
            ErrorKind::FractionTooBig { position } => {
                format!(
                    "Fraction too bit at line: {} column: {}",
                    position.line, position.column
                )
            }
            ErrorKind::NumberTooBig { position } => {
                format!(
                    "Number too bit at line: {} column: {}",
                    position.line, position.column
                )
            }
            ErrorKind::SyntaxError {
                position,
                expected_kind,
                got,
            } => {
                format!(
                    "Syntax error expected: {:?}, got: {:?} at: {} column: {}",
                    expected_kind, got, position.line, position.column
                )
            }
            ErrorKind::ObjectExpected { position, got } => {
                format!(
                    "Object expected:  got: {:?} at: {} column: {}",
                    got, position.line, position.column
                )
            }
        };
        eprintln!("{}", msg);
    }
}
