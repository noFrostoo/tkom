use rust_decimal::Decimal;

use crate::{types::*, executor::Value};
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
    // NoField{ object: ObjectRef, field: String },
    // CompareDifferentTypes{ left: Value, right:Value},
    // NumberOverflow{number: Decimal},
    NoField{},
    CompareDifferentTypes,
    NumberOverflow,
    NotAllowedOperation,
    ObjectExpected,
    UnknownFunction,
    // NotCallable{value: Value},
    IllegalAccess{on:Value, want:String},
    NotCallable,
    UnexpectedExpression,
    AccessNone,
    NotFoundScope,
    NotDefined,
    NotAssignable,
    BadType,
    NotIterable,
    BadInputNumber {
        err_msg: String,
    },
    MismatchedArgumentsLen {
        expected: usize,
        got: usize,
        function: String,
    },
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
            ErrorKind::DuplicateFunction { name } => format!("error1"),
            ErrorKind::DuplicateParameters { name } => format!("error2"),
            ErrorKind::BlockExpected {} => format!("error3"),
            ErrorKind::NoField {} => format!("error4"),
            ErrorKind::CompareDifferentTypes => format!("error5"),
            ErrorKind::NumberOverflow => format!("error6"),
            ErrorKind::NotAllowedOperation => format!("error7"),
            ErrorKind::ObjectExpected => format!("error8"),
            ErrorKind::UnknownFunction => format!("error9"),
            ErrorKind::NotCallable => format!("error10"),
            ErrorKind::IllegalAccess { on, want } => format!("error11"),
            ErrorKind::UnexpectedExpression => format!("error15"),
            ErrorKind::AccessNone => format!("error16"),
            ErrorKind::NotFoundScope => format!("error17"),
            ErrorKind::NotDefined => format!("error18"),
            ErrorKind::NotAssignable => format!("error19"),
            ErrorKind::BadType => format!("error20"),
            ErrorKind::NotIterable => format!("error21"),
            ErrorKind::BadInputNumber { err_msg } => todo!(),
            ErrorKind::MismatchedArgumentsLen {
                expected,
                got,
                function,
            } => todo!(),
        }
    }
}
