use errors::LexerErrorKind;

pub mod file_handler;
mod types;
mod lexer;
mod errors;

fn dummy() -> Result<bool, LexerErrorKind> {
    Ok(false)
}
fn main() {
    while let Ok(eee) = dummy() {
        println!("{}", eee);
    }
}
