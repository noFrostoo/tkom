use crate::file_handler::Source;

pub mod file_handler;
mod types;
mod lexer;
mod errors;

fn main() {
    let ch = 13 as char;
    println!("{}", ch.len_utf8());
}
