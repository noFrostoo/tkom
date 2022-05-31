use crate::{
    types::*,
};
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
    ExpressionExpected {
        position: Position,
        got: TokenKind,
    },
    ConditionExpected {
        position: Position,
    },
    IncompleteExpression {
        position: Position,
        expression_type: Expression,
    },
    DuplicateFunction {
        name: String,
    },
    DuplicateParameters {
        name: String,
    },
    BlockExpected {},
    NoFunctions,
    NoField{},
    CompareDifferentTypes,
    NumberOverflow,
    NotAllowedOperation,
    ObjectExpected,
    UnknownFunction,
    NotCallable,
    AccessOnFunction,
    AccessOnNumber,
    AccessOnString,
    AccessOnBool,
    UnexpectedExpression,
    AccessNone,
    NotFoundScope,
    NotDefined,
    NotAssignable,
    BadType,
    NotIterable,
}

pub struct ErrorHandler;

impl ErrorHandler {
    pub fn report_error(err: ErrorKind) {
        Self::handle_error(err)
    }

    pub fn fatal(err: ErrorKind) -> ! {
        if cfg!(test) {
            panic!("{}", ErrorHandler::error_msg(err))
        } else {
            Self::handle_error(err);
            exit(1);
        }
    }

    pub fn io_error(err: Error) -> ! {
        eprintln!("Error opening the file: {}", err);
        exit(2);
    }

    fn handle_error(err: ErrorKind) {
        eprintln!("{}", ErrorHandler::error_msg(err));
    }

    fn error_msg(err: ErrorKind) -> String {
        match err {
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
            ErrorKind::ExpressionExpected { position, got } => {
                format!(
                    "Expression expected:  got: {:?} at: {} column: {}",
                    got, position.line, position.column
                )
            }
            ErrorKind::ConditionExpected { position } => {
                format!(
                    "No condition: at: {} column: {}",
                    position.line, position.column
                )
            }
            ErrorKind::IncompleteExpression {
                position,
                expression_type,
            } => {
                format!(
                    "Incomplete expression: {:?} at: {} column: {}",
                    expression_type, position.line, position.column
                )
            }
            ErrorKind::NoFunctions {} => {
                format!("No functions in file")
            }
            ErrorKind::DuplicateFunction { name } => todo!(),
            ErrorKind::DuplicateParameters { name } => todo!(),
            ErrorKind::BlockExpected {} => todo!(),
            ErrorKind::NoField {  } => todo!(),
            ErrorKind::CompareDifferentTypes => todo!(),
            ErrorKind::NumberOverflow => todo!(),
            ErrorKind::NotAllowedOperation => todo!(),
            ErrorKind::ObjectExpected => todo!(),
            ErrorKind::UnknownFunction => todo!(),
            ErrorKind::NotCallable => todo!(),
            ErrorKind::AccessOnFunction => todo!(),
            ErrorKind::AccessOnNumber => todo!(),
            ErrorKind::AccessOnString => todo!(),
            ErrorKind::AccessOnBool => todo!(),
            ErrorKind::UnexpectedExpression => todo!(),
            ErrorKind::AccessNone => todo!(),
            ErrorKind::NotFoundScope => todo!(),
            ErrorKind::NotDefined => todo!(),
            ErrorKind::NotAssignable => todo!(),
            ErrorKind::BadType => todo!(),
            ErrorKind::NotIterable => todo!(),
        }
    }
}
