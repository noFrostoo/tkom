use crate::types::Position;
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
    MaxIdentLenExceeded {
        position: Position,
    },
    FractionTooBig {
        position: Position,
    },
    NumberTooBig {
        position: Position,
    },
    NoToken,
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
            ErrorKind::MaxIdentLenExceeded { position } => {
                format!(
                    "Max ident length exceeded at line: {} column: {}",
                    position.line, position.column
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
        };
        eprintln!("{}", msg);
    }
}
