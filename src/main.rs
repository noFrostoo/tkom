use errors::ErrorKind;

pub mod file_handler;
mod types;
mod lexer;
mod errors;

fn dummy() -> Result<bool, ErrorKind> {
    Ok(false)
}
fn main() {
    while let Ok(eee) = dummy() {
        println!("{}", eee);
    }
}
