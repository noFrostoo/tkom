use rust_decimal::Decimal;

use crate::{types::*, executor::{Value, ObjectRef, Object}};
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
    NoField{ object: ObjectRef, field: String, pos: Position },
    CompareDifferentTypes{ left: Value, right:Value, pos: Position},
    NumberOverflow{number: Decimal, other: Decimal, operation: String, pos: Position},
    NotAllowedOperation{value:Value, other:Value, name:String, pos: Position},
    ObjectExpected{what: String, pos: Position},
    UnknownFunction{name: String, pos: Position},
    // NotCallable{value: Value},
    IllegalAccess{on:Value, want:String},
    NotCallable{value:Value,  pos: Position},
    UnexpectedExpression,
    BadAssign{pos: Position}, 
    MismatchedTypes{left:Value, right:Value, pos: Position},
    NotFoundScope{pos: Position},
    NotDefined{name: String, pos: Position},
    NotAssignable{name: String, pos: Position},
    BadType{value:Value, expected:Value, pos: Position},
    NotIterable{value: Value, pos: Position},
    BadInputNumber {
        err_msg: String,
    },
    MismatchedArgumentsLen {
        expected: usize,
        got: usize,
        function: String,
    },
    NotUpdatable{name: String, value: Value},
    BadConvert{number: String}
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
            ErrorKind::DuplicateFunction { name } => {
                format!("Duplicate function name: {}", name)
            },
            ErrorKind::DuplicateParameters { name } => {
                format!("Duplicate parameter {}", name)
            },
            ErrorKind::BlockExpected {} => format!("error3"),
            ErrorKind::NoField{ object, field, pos } => {
                format!("No field: {} found on object {:?}  at: {} column: {}", field, object, pos.line, pos.column)
            },
            ErrorKind::CompareDifferentTypes { left, right, pos } => {
                format!("Comparing different types {:?}, {:?} at: {} column: {}", left, right, pos.line, pos.column)
            },
            ErrorKind::NumberOverflow{ number, other, operation, pos } => {
                format!("Number overflow: {} operation: {}, other: {} at: {} column: {}", number, operation, other, pos.line, pos.column)
            },
            ErrorKind::NotAllowedOperation { value, other, name, pos } => {
                format!("Not allowed operation: {} for {:?} with {:?} at: {} column: {}", name, value, other,pos.line, pos.column)
            },
            ErrorKind::ObjectExpected { what, pos } => {
                format!("Object excepted: {} at: {} column: {}", what, pos.line, pos.column)
            },
            ErrorKind::UnknownFunction { name, pos } => {
                format!("Unknown Function: {} at at: {} column: {}",
                name, pos.line, pos.column)
            },
            ErrorKind::NotCallable { value, pos } => {
                format!("Not callable: {:?} at: {} column: {}", value, pos.line, pos.column)
            },
            ErrorKind::IllegalAccess { on, want } => {
                format!("Illegal access: {:?}, want {:?}", on, want)
            },
            ErrorKind::UnexpectedExpression => format!("error15"),
            ErrorKind::NotFoundScope{ pos } => {
                format!("Not found cope at: {} column: {}", pos.line, pos.column)
            },
            ErrorKind::NotDefined{ name, pos } => {
                format!("Not defined {} at: {} column: {}",  name, pos.line, pos.column)
            },
            ErrorKind::NotAssignable{ name, pos } => {
                format!("Not assignable : {} at: {} column: {}", name, pos.line, pos.column)
            },
            ErrorKind::BadType { value, expected, pos } => {
                format!("Bad type, got: {:?} expected: {:?}, at: {} column: {}", value,expected,pos.line, pos.column)
            },
            ErrorKind::NotIterable{ value, pos } => {
                format!("Not iterable: {:?}, at: {} column: {}", value,pos.line, pos.column)
            },
            ErrorKind::BadInputNumber { err_msg } => {
                format!("Bad input number: {}", err_msg)
            },
            ErrorKind::MismatchedArgumentsLen {
                expected,
                got,
                function,
            } => {
                format!("Mismatched arguments len got: {}, excepted: {}, at: {}", got, expected, function)
            },
            ErrorKind::NotUpdatable { name, value } => {
                format!("Not updatable: {} at value: {:?}", name, value)
            },
            ErrorKind::BadAssign { pos } => {
                format!("Bad assign at: {} column: {} ", pos.line, pos.column)
            },
            ErrorKind::MismatchedTypes { left, right, pos } => {
                format!("Mismatched types got: {:?}, excepted: {:?}, at: {} column: {}", left, right, pos.line, pos.column)
            },
            ErrorKind::BadConvert { number } => {
                format!("Bad convert: {}", number)
            },
            
        }
    }
}
