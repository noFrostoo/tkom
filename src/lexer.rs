use std::collections::{VecDeque};

use crate::{types::{Token, TokenKind, Position}, file_handler::Source, errors::{LexerErrorKind, ReadError}};

const ETX: char = 3 as char;

pub trait TLexer {
    fn get_next_token(&mut self) -> Result<Token, LexerErrorKind>;
    fn get_current_token(&self) -> Token;
}


struct Lexer {
    pos: Position,
    current_char: char,
    token: Token,
    source: Box<dyn Source> 
}

impl TLexer for Lexer {
    fn get_next_token(&mut self) -> Result<Token, LexerErrorKind> {
        match self.skip_whitespace() {
            Some(e) => return Err(e),
            None => {},
        }

        match self.check_new_line() {
            Ok(_) => {},
            Err(e) => return Err(e),
        }

        if self.current_char == ETX{
            return Err(LexerErrorKind::EndOfFile);
        }
        
        return self.try_tokenize_alphanumerical().
            or_else(|_| self.try_build_operator()).
            or_else(|_| self.try_build_parentheses_or_comma()).
            or_else(|_| self.try_build_comment())
    }

    fn get_current_token(&self) -> Token {
        self.token.clone()
    }
}

impl Lexer {

pub fn new(source: Box<dyn Source>) -> Lexer {
    Lexer{
        source: source,
        pos: Position::zero(),
        current_char: '\0',
        token: Token::new(0, Position::zero(), TokenKind::Unknown)
    }
}

fn skip_whitespace(&mut self) -> Option<LexerErrorKind> {
    loop {
        let ch = match self.get_next_char(){
            Ok(c) => c,
            Err(e) => return Some(e),
        };

        if ch.is_whitespace() {
            continue;
        }
        break None;
    }
}

fn get_next_char(&mut self) -> Result<char, LexerErrorKind> {
    match self.source.get_next_char() {
        Ok(read_char) => {
            self.current_char = read_char;
            self.pos.new_char(read_char.len_utf8() as u64);
            Ok(read_char)
        },
        Err(e) => {
            match e {
                /*
                !!this is retarded, one error type ??
                */
                ReadError::NoData => { self.current_char = 3 as char ;Err(LexerErrorKind::EndOfFile)}
                ReadError::Eof => { self.current_char = 3 as char ;Err(LexerErrorKind::EndOfFile)},
                ReadError::ReadingError(e ) => Err(LexerErrorKind::ReadError(e)),
            }
        }
    }
}


fn try_tokenize_alphanumerical(&mut self) -> Result<Token, LexerErrorKind> {
    if !self.current_char.is_alphanumeric() {
        return Err(LexerErrorKind::UnexpectedCharacter);
    }

    let mut value: VecDeque<char> = VecDeque::from_iter([self.current_char]); 
    
    loop {
        let ch = match self.get_next_char(){
            Ok(c) => c,
            Err(e) => return Err(e),
        };

        if ch.is_whitespace() {
            break; 
        }

        if !ch.is_alphanumeric() {
            break ;
        }

        value.push_back(ch);
    }

    let word = String::from_iter(value);

    match word.as_str() {
        "while" => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::While)),
        "return" => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Return)),
        "if" => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::If)),
        "else" =>Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Else)),
        /*
        ! Keep it here ?
        */
        "or" => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Or)),
        "and" =>Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::And)),
        _ => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Identifier(word)))
    }
    
}

fn try_build_operator(&mut self) -> Result<Token, LexerErrorKind>{
    match self.current_char {
        '>' => {
            let ch = match self.get_next_char(){
                Ok(c) => c,
                Err(e) => return Err(e),
            };

            if ch == '=' {
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::GraterEqualThen))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::GraterThen))
        },
        '<' => {
            let ch = match self.get_next_char(){
                Ok(c) => c,
                Err(e) => return Err(e),
            };

            if ch == '=' {
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::LessEqualThen))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::LessThen))
        },
        '=' => {
            let ch = match self.get_next_char(){
                Ok(c) => c,
                Err(e) => return Err(e),
            };

            if ch == '=' {
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Equal))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Assignment))
        },
        '+' => {
            let ch = match self.get_next_char(){
                Ok(c) => c,
                Err(e) => return Err(e),
            };

            if ch == '=' {
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::AddAssignment))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Addition))
        },
        '-' => {
            let ch = match self.get_next_char(){
                Ok(c) => c,
                Err(e) => return Err(e),
            };

            if ch == '=' {
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::SubtractAssignment))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Subtraction))
        },
        '/' => {
            let ch = match self.get_next_char(){
                Ok(c) => c,
                Err(e) => return Err(e),
            };

            if ch == '=' {
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::DivisionAssignment))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Division))
        },
        '*' => {
            let ch = match self.get_next_char(){
                Ok(c) => c,
                Err(e) => return Err(e),
            };

            if ch == '=' {
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::MultiplicationAssignment))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Multiplication))
        },
        '%' => {
            let ch = match self.get_next_char(){
                Ok(c) => c,
                Err(e) => return Err(e),
            };

            if ch == '=' {
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::ModuloAssignment))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Modulo))
        },
        '|' => {
            let ch = match self.get_next_char(){
                Ok(c) => c,
                Err(e) => return Err(e),
            };

            /*
            ! Report error
            */
            if ch != '|' {
                return Err(LexerErrorKind::UnexpectedCharacter)
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Or))
        },
        '&' => {
            let ch = match self.get_next_char(){
                Ok(c) => c,
                Err(e) => return Err(e),
            };

            /*
            ! Report error
            */
            if ch != '&' {
                return Err(LexerErrorKind::UnexpectedCharacter)
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::And))
        },
        '!' => {
            let ch = match self.get_next_char(){
                Ok(c) => c,
                Err(e) => return Err(e),
            };

            if ch == '=' {
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::NotEqual))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Not))
        },
        _ => Err(LexerErrorKind::UnexpectedCharacter),
    }
}

fn try_build_parentheses_or_comma(&mut self) -> Result<Token, LexerErrorKind> {
    match self.current_char {
        '{' => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::LeftBracket)),
        '}' => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::RightBracket)),
        '(' => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::LeftParentheses)),
        ')' => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::RightParentheses)),
        '.' => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Comma)),
        _ => Err(LexerErrorKind::UnexpectedCharacter),
    }
}

fn try_build_comment(&mut self) -> Result<Token, LexerErrorKind> {
    if self.current_char != '#' {
        return  Err(LexerErrorKind::UnexpectedCharacter);
    }

    let mut value: VecDeque<char> = VecDeque::new(); 

    loop {
        let ch = match self.get_next_char(){
            Ok(c) => c,
            Err(e) => match e {
                LexerErrorKind::EndOfFile => {break;},
                LexerErrorKind::UnexpectedCharacter => return Err(LexerErrorKind::UnexpectedCharacter),
                LexerErrorKind::ReadError(e) => return Err(LexerErrorKind::ReadError(e)),
            },
        };

        match self.check_new_line() {
            Ok(new_line) => {
                if new_line {
                    break;
                }
            },
            Err(e) => match e {
                LexerErrorKind::EndOfFile => {break;},
                LexerErrorKind::UnexpectedCharacter => return Err(LexerErrorKind::UnexpectedCharacter),
                LexerErrorKind::ReadError(e) => return Err(LexerErrorKind::ReadError(e)),
            },
        }

        value.push_back(ch);
    }

    let word = String::from_iter(value);

    Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Comment(word)))
}

fn check_new_line(&mut self) -> Result<bool, LexerErrorKind> {
    if self.current_char == 10 as char{
        let ch = match self.get_next_char(){
            Ok(c) => c,
            Err(e) => return Err(e),
        };

        if ch == 13 as char {
            match self.get_next_char(){
                Ok(_) => {},
                Err(e) => return Err(e),
            };
            self.pos.new_line(2);
            return Ok(true)
        }

        self.pos.new_line(1);
        return Ok(true)
    }

    if self.current_char == 13 as char {
        let ch = match self.get_next_char(){
            Ok(c) => c,
            Err(e) => return Err(e),
        };

        if ch == 10 as char {
            match self.get_next_char(){
                Ok(_) => {},
                Err(e) => return Err(e),
            };
            self.pos.new_line(2);
            return Ok(true)
        }

        self.pos.new_line(1);
        return Ok(true)
    }

    if self.current_char == 30 as char {
        match self.get_next_char(){
            Ok(_) => {},
            Err(e) => return Err(e),
        };
        self.pos.new_line(1);
        return Ok(true)
    }

    Ok(false)
}

fn try_build_number(&mut self) -> Result<Token, LexerErrorKind> {

}

fn try_build_string(&mut self) -> Result<Token, LexerErrorKind> {
    
}

}

#[cfg(test)]
mod test {
    macro_rules! lexer_tokenize_tokens {
        (FAIL: $name:ident, $text:expr) => {
            #[test]
            fn $name() {
                let text: &str = $text;
    
                let test_source = file_handler::TestSource::new(String::from(text), 0);
                let mut lexer = Lexer::new(Box::new(test_source));
                assert!(lexer.get_next_token().is_err())
            }
        };
        ($name:ident, $text:expr, $token:expr) => {
            #[test]
            fn $name() {
                let text: &str = $text;
                let should_be: TokenKind = $token;
    
                let test_source = file_handler::TestSource::new(String::from(text), 0);
                let mut lexer = Lexer::new(Box::new(test_source));
                match lexer.get_next_token() {
                    Ok(ch) => assert_eq!(ch.kind, should_be),
                    Err(_) => panic!()
                }
            }
        };
    }

    use crate::{file_handler, types::TokenKind};

    use super::{Lexer, TLexer};

    lexer_tokenize_tokens!(ident_tokenize_test, "    aaaaa    ", TokenKind::Identifier(String::from("aaaaa")));
    lexer_tokenize_tokens!(while_tokenize_test, "   while   ", TokenKind::While);
    lexer_tokenize_tokens!(if_tokenize_test, "   return   ", TokenKind::Return);
    lexer_tokenize_tokens!(return_tokenize_test, "   if   ", TokenKind::If);
    lexer_tokenize_tokens!(else_tokenize_test, "   else   ", TokenKind::Else);
    lexer_tokenize_tokens!(plus_tokenize_test, "   +   ", TokenKind::Addition);
    lexer_tokenize_tokens!(subtract_tokenize_test, "   -   ", TokenKind::Subtraction);
    lexer_tokenize_tokens!(assignment_tokenize_test, "   =   ", TokenKind::Assignment);
    lexer_tokenize_tokens!(division_assignment_tokenize_test, "   /=   ", TokenKind::DivisionAssignment);
    lexer_tokenize_tokens!(add_assignment_tokenize_test, "   +=   ", TokenKind::AddAssignment);
    lexer_tokenize_tokens!(subtract_assignment_tokenize_test, "   -=   ", TokenKind::SubtractAssignment);
    lexer_tokenize_tokens!(modulo_assignment_tokenize_test, "   %=   ", TokenKind::ModuloAssignment);
    lexer_tokenize_tokens!(multiplication_assignment_tokenize_test, "   *=   ", TokenKind::MultiplicationAssignment);
    lexer_tokenize_tokens!(division_tokenize_test, "   /   ", TokenKind::Division);
    lexer_tokenize_tokens!(multiplication_tokenize_test, "   *   ", TokenKind::Multiplication);
    lexer_tokenize_tokens!(modulo_tokenize_test, "   %   ", TokenKind::Modulo);
    lexer_tokenize_tokens!(not_tokenize_test, "   !   ", TokenKind::Not);
    lexer_tokenize_tokens!(not_equal_tokenize_test, "   !=   ", TokenKind::NotEqual);
    lexer_tokenize_tokens!(open_bracket_tokenize_test, "   {   ", TokenKind::LeftBracket);
    lexer_tokenize_tokens!(close_bracket_tokenize_test, "   }   ", TokenKind::RightBracket);
    lexer_tokenize_tokens!(open_parentheses_tokenize_test, "   (   ", TokenKind::LeftParentheses);
    lexer_tokenize_tokens!(close_parentheses_tokenize_test, "   )   ", TokenKind::RightParentheses);
    lexer_tokenize_tokens!(comma_tokenize_test, "   .   ", TokenKind::Comma);
    lexer_tokenize_tokens!(comment_tokenize_test, "    #aaaaa    ", TokenKind::Comment(String::from("aaaaa    ")));
}

