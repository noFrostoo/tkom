use errors::ErrorHandler;
use executor::Executor;
use file_handler::FileSource;
use lexer::Lexer;
use parser::Parser as ProgramParser;
use visitor::Visitor;

mod errors;
mod executor;
pub mod file_handler;
mod lexer;
mod parser;
mod types;
mod visitor;

use clap::Parser;

#[derive(Parser)]
#[clap(name = "TKOM")]
#[clap(author = "Daniel Lipniacki")]
#[clap(version = "1.0")]
struct Cli {
    //File to interpreter
    #[clap(short, long)]
    file: String,

    //Allow for recoverable errors to happen
    #[clap(long)]
    fail_fast: bool,

    #[clap(short, long, default_value_t = 12)]
    stack_trace_len: usize,
}

fn main() {
    let args = Cli::parse();

    let program;
    {
        let fs = match FileSource::new(args.file) {
            Ok(f) => f,
            Err(err) => ErrorHandler::io_error(err),
        };
        let lex = Lexer::new(Box::new(fs));
        let mut parser = ProgramParser::new(Box::new(lex), args.fail_fast);
        program = parser.parse();
    }

    let mut executor = Executor::new(args.stack_trace_len);
    executor.visit_program(&program);
}
