use errors::{ErrorHandler};
use file_handler::FileSource;
use lexer::{Lexer};

pub mod file_handler;
mod types;
mod lexer;
mod errors;

fn main() {
    let fs = match FileSource::new(String::from("sourceFile.ss")) {
        Ok(f) => f,
        Err(err) => ErrorHandler::io_error(err),
    };
    let lex = Lexer::new(Box::new(fs));
    for result in lex {
        match result {
            Ok(token) => print!("{}", token),
            Err(_) => eprint!("Error"),
        }
    }
}
