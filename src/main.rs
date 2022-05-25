use errors::ErrorHandler;
use file_handler::FileSource;
use lexer::Lexer;
use parser::Parser;

mod errors;
pub mod file_handler;
mod lexer;
mod parser;
mod types;

fn main() {
    let fs = match FileSource::new(String::from("testFile.ss")) {
        Ok(f) => f,
        Err(err) => ErrorHandler::io_error(err),
    };
    let lex = Lexer::new(Box::new(fs));
    let mut parser = Parser::new(Box::new(lex), false);
    let program = parser.parse();
    print!("{:?}", program)
}
