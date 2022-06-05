use errors::ErrorHandler;
use executor::Executor;
use file_handler::FileSource;
use lexer::Lexer;
use parser::Parser;
use visitor::Visitor;

mod errors;
mod executor;
pub mod file_handler;
mod lexer;
mod parser;
mod types;
mod visitor;

fn main() {
    let program;
    {
        let fs = match FileSource::new(String::from("basic.ss")) {
            Ok(f) => f,
            Err(err) => ErrorHandler::io_error(err),
        };
        let lex = Lexer::new(Box::new(fs));
        let mut parser = Parser::new(Box::new(lex), false);
        program = parser.parse();
    }
    print!("{:?}", program);
    let mut executor = Executor::new();
    executor.visit_program(&program);
}
