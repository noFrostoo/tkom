use std::collections::{VecDeque};

use crate::{types::{Token, TokenKind, Position, Number}, file_handler::Source, errors::{LexerErrorKind, ReadError}};

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

        match self.skip_new_lines() {
            Some(e) => return Err(e),
            None => {},
        }

        if self.current_char == ETX{
            return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::EndOfFile));
        }
        
        return self.try_tokenize_ident_or_keyword().
            or_else(|_| self.try_build_operator()).
            or_else(|_| self.try_build_parentheses_or_comma()).
            or_else(|_| self.try_build_comment()).
            or_else(|_| self.try_build_string()).
            or_else(|_| self.try_build_number())
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
        current_char: ' ',
        token: Token::new(0, Position::zero(), TokenKind::Unknown)
    }
}

fn skip_whitespace(&mut self) -> Option<LexerErrorKind> {
    while self.current_char.is_whitespace() {
        match self.get_next_char(){
            Ok(_) => (),
            Err(e) => return Some(e),
        };
    }

    None
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
                ReadError::ReadingError(e ) => Err(LexerErrorKind::ReadError(e)),
            }
        }
    }
}


fn try_tokenize_ident_or_keyword(&mut self) -> Result<Token, LexerErrorKind> {
    if !self.current_char.is_alphabetic() {
        return Err(LexerErrorKind::UnexpectedCharacter { actual: self.current_char.clone(), expected: String::from("a,b,.."), position: self.pos.clone() });
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
                match self.get_next_char(){
                    Ok(c) => c,
                    Err(e) => return Err(e),
                };
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
                match self.get_next_char(){
                    Ok(c) => c,
                    Err(e) => return Err(e),
                };
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
                match self.get_next_char(){
                    Ok(c) => c,
                    Err(e) => return Err(e),
                };
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
                match self.get_next_char(){
                    Ok(c) => c,
                    Err(e) => return Err(e),
                };
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
                match self.get_next_char(){
                    Ok(c) => c,
                    Err(e) => return Err(e),
                };
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
                match self.get_next_char(){
                    Ok(c) => c,
                    Err(e) => return Err(e),
                };
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
                match self.get_next_char(){
                    Ok(c) => c,
                    Err(e) => return Err(e),
                };
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
                match self.get_next_char(){
                    Ok(c) => c,
                    Err(e) => return Err(e),
                };
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
                match self.get_next_char(){
                    Ok(c) => c,
                    Err(e) => return Err(e),
                };
                return Err(LexerErrorKind::UnexpectedCharacter { actual: ch, expected: String::from("|"), position: self.pos.clone() })
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
                match self.get_next_char(){
                    Ok(c) => c,
                    Err(e) => return Err(e),
                };
                return Err(LexerErrorKind::UnexpectedCharacter { actual: ch, expected: String::from("&"), position: self.pos.clone() })
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::And))
        },
        '!' => {
            let ch = match self.get_next_char(){
                Ok(c) => c,
                Err(e) => return Err(e),
            };

            if ch == '=' {
                match self.get_next_char(){
                    Ok(c) => c,
                    Err(e) => return Err(e),
                };
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::NotEqual))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Not))
        },
        _ => Err(LexerErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("<, >, |, &, =, !, +, -, /, %"), position: self.pos.clone() }),
    }
}

/*
! Refactor name
*/
fn try_build_parentheses_or_comma(&mut self) -> Result<Token, LexerErrorKind> {
    let res: Result<Token, LexerErrorKind>;
    
    match self.current_char {
        '{' => res = Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::LeftBracket)),
        '}' => res = Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::RightBracket)),
        '(' => res = Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::LeftParentheses)),
        ')' => res = Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::RightParentheses)),
        '.' => res = Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Comma)),
        ';' => res = Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Semicolon)),
        _ => res = Err(LexerErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("{, }, (, ), ."), position: self.pos.clone() }),
    }

    if res.is_ok() {
        match self.get_next_char(){
            Ok(_) => (),
            Err(e) => match e {
                e => return Err(e),
            },
        };
    }

    res
}

fn try_build_comment(&mut self) -> Result<Token, LexerErrorKind> {
    if self.current_char != '#' {
        return  Err(LexerErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("#"), position: self.pos.clone() });
    }

    let mut value: VecDeque<char> = VecDeque::new(); 

    loop {
        let ch = match self.get_next_char(){
            Ok(c) => c,
            Err(e) => match e {
                e => return Err(e),
            },
        };

        if ch == ETX {
            break;
        }

        match self.check_new_line() {
            Ok(new_line) => {
                if new_line {
                    break;
                }
            },
            Err(e) => match e {
                e => return Err(e),
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

fn skip_new_lines(&mut self) -> Option<LexerErrorKind>{
    loop {
        match self.check_new_line() {
            Ok(new_line) => {
                if !new_line {
                    return None;
                }
            },
            Err(e) => match e {
                e => return Some(e),
            },
        }
        
    }
}

fn try_build_number(&mut self) -> Result<Token, LexerErrorKind> {
    if !self.current_char.is_digit(10) {
        return Err(LexerErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("number"), position: self.pos.clone() });
    }

    let mut int_part: u64 = 0;
    let mut frac_part: u64 = 0;
    let mut frac_part_len: u64 = 0;
    let mut zero_count: u32 = 0; //? is this good ?? 

    while self.current_char == '0' {
        zero_count += 1;
        match self.get_next_char(){
            Ok(_) => {},
            Err(e) => match e {
                e => return Err(e),
            },
        };
    }

    if self.current_char != '.' && !self.current_char.is_digit(10) {
        return  Err(LexerErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("number"), position: self.pos.clone() });
    }

    if zero_count == 0 && self.current_char != '.' {
        int_part += self.current_char as u64 - '0' as u64 ;

        let mut ch = match self.get_next_char(){
            Ok(c) => c,
            Err(e) => match e {
                e => return Err(e),
            },
        };
        
        while ch.is_digit(10) {
            int_part = int_part * 10 + ch as u64 - '0' as u64;
            ch = match self.get_next_char(){
                Ok(c) => c,
                Err(e) => match e {
                    e => return Err(e),
                },
            };
        }
    }

    if self.current_char == '.' {
        zero_count = 0;
        let mut ch = match self.get_next_char(){
            Ok(c) => c,
            Err(e) => match e {
                e => return Err(e),
            },
        };

        if ch == ETX {
            return Err(LexerErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("a number"), position: self.pos.clone() })
        }

        while ch.is_digit(10) {
            frac_part = frac_part * 10 + ch as u64 - '0' as u64;
            frac_part_len += 1;
            ch = match self.get_next_char(){
                Ok(c) => c,
                Err(e) => match e {
                    e => return Err(e),
                },
            };
        }
    } else if !self.current_char.is_whitespace() && !self.current_char.is_control() {
        return Err(LexerErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("a number"), position: self.pos.clone() });
    }

    if zero_count > 0 {
        return Err(LexerErrorKind::BadNumber(Number::new(int_part, frac_part, frac_part_len), zero_count));
    }
        

    Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Number(Number::new(int_part, frac_part, frac_part_len))))
}

fn try_build_string(&mut self) -> Result<Token, LexerErrorKind> {
    if self.current_char != '"' && self.current_char != '\'' {
        return Err(LexerErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("\", '"), position: self.pos.clone() });
    }

    let mut value: VecDeque<char> = VecDeque::new(); 
    let mut closing_quote = false;

    // ? is this correct ?
    while !self.current_char.is_control() {
        let ch = match self.get_next_char(){
            Ok(c) => c,
            Err(e) => match e {
                e => return Err(e),
            },
        };

        if ch == '"' || ch == '\'' {
            closing_quote = true;
            break;
        }

        /*
        ? allow for escape chars? what disallow in string 
        */

        match self.check_new_line() {
            Ok(new_line) => {
                if new_line {
                    return Err(LexerErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("\", '"), position: self.pos.clone() });
                }
            },
            Err(e) => match e {
                e => return Err(e),
            },
        }

        value.push_back(ch);
    }

    if !closing_quote {
        return Err(LexerErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("\", '"), position: self.pos.clone() });
    }

    let word = String::from_iter(value);

    match self.get_next_char(){
        Ok(_) => {},
        Err(e) => match e {
            e => return Err(e),
        }
    }

    Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::QuotedString(word)))
}

}

#[cfg(test)]
mod test {
    macro_rules! tokenize_token {
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

    macro_rules! tokenize_multiple_tokens {
        (FAIL: $name:ident, $text:expr, $( $token:expr ),+) => {
            #[test]
            fn $name() {
                let text: &str = $text;
                let test_source = file_handler::TestSource::new(String::from(text), 0);
                let mut lexer = Lexer::new(Box::new(test_source));

                $(
                    if $token == "fail" {
                        assert!(lexer.get_next_token().is_err())
                    } else {
                        let should_be: TokenKind = $token;
                        match lexer.get_next_token() {
                            Ok(ch) => assert_eq!(ch.kind, should_be),
                            Err(_) => panic!()
                        }
                    }
                )+
            }
        };
        ($name:ident, $text:expr, $( $token:expr ),+) => {
            #[test]
            fn $name() {
                let text: &str = $text;
                let test_source = file_handler::TestSource::new(String::from(text), 0);
                let mut lexer = Lexer::new(Box::new(test_source));

                $(
                    let should_be: TokenKind = $token;
                    match lexer.get_next_token() {
                        Ok(ch) => assert_eq!(ch.kind, should_be),
                        Err(_) => panic!()
                    }
                )+
            }
        };
    }
    use crate::{file_handler, types::{TokenKind, Number}};

    use super::{Lexer, TLexer};

    tokenize_token!(ident_tokenize_test, "    aaaaa    ", TokenKind::Identifier(String::from("aaaaa")));
    tokenize_token!(while_tokenize_test, "   while   ", TokenKind::While);
    tokenize_token!(if_tokenize_test, "   return   ", TokenKind::Return);
    tokenize_token!(return_tokenize_test, "   if   ", TokenKind::If);
    tokenize_token!(else_tokenize_test, "   else   ", TokenKind::Else);
    tokenize_token!(plus_tokenize_test, "   +   ", TokenKind::Addition);
    tokenize_token!(subtract_tokenize_test, "   -   ", TokenKind::Subtraction);
    tokenize_token!(assignment_tokenize_test, "   =   ", TokenKind::Assignment);
    tokenize_token!(division_assignment_tokenize_test, "   /=   ", TokenKind::DivisionAssignment);
    tokenize_token!(add_assignment_tokenize_test, "   +=   ", TokenKind::AddAssignment);
    tokenize_token!(subtract_assignment_tokenize_test, "   -=   ", TokenKind::SubtractAssignment);
    tokenize_token!(modulo_assignment_tokenize_test, "   %=   ", TokenKind::ModuloAssignment);
    tokenize_token!(multiplication_assignment_tokenize_test, "   *=   ", TokenKind::MultiplicationAssignment);
    tokenize_token!(division_tokenize_test, "   /   ", TokenKind::Division);
    tokenize_token!(multiplication_tokenize_test, "   *   ", TokenKind::Multiplication);
    tokenize_token!(modulo_tokenize_test, "   %   ", TokenKind::Modulo);
    tokenize_token!(not_tokenize_test, "   !   ", TokenKind::Not);
    tokenize_token!(not_equal_tokenize_test, "   !=   ", TokenKind::NotEqual);
    tokenize_token!(open_bracket_tokenize_test, "   {   ", TokenKind::LeftBracket);
    tokenize_token!(close_bracket_tokenize_test, "   }   ", TokenKind::RightBracket);
    tokenize_token!(open_parentheses_tokenize_test, "   (   ", TokenKind::LeftParentheses);
    tokenize_token!(close_parentheses_tokenize_test, "   )   ", TokenKind::RightParentheses);
    tokenize_token!(comma_tokenize_test, "   .   ", TokenKind::Comma);
    tokenize_token!(comment_tokenize_test, "    #aaaaa    ", TokenKind::Comment(String::from("aaaaa    ")));
    tokenize_token!(comment_newline_tokenize_test, "    #aaaaa\n    ", TokenKind::Comment(String::from("aaaaa")));
    tokenize_token!(string_tokenize_test, "    \"aaaaa\"    ", TokenKind::QuotedString(String::from("aaaaa")));
    tokenize_token!(string2_tokenize_test, "    'aaaaa'    ", TokenKind::QuotedString(String::from("aaaaa")));
    tokenize_token!(FAIL: string3_tokenize_test, "    'aa\taaa'    ");
    tokenize_token!(FAIL: string4_tokenize_test, "    'aaaaa    ");
    tokenize_token!(number_tokenize_test, "    2137    ", TokenKind::Number(Number::new(2137, 0, 0)));
    tokenize_token!(number2_tokenize_test, "    2137.420    ", TokenKind::Number(Number::new(2137, 420, 3)));
    tokenize_token!(number3_tokenize_test, "    0.4    ", TokenKind::Number(Number::new(0, 4, 1)));
    tokenize_token!(number4_tokenize_test, "    0000.4    ", TokenKind::Number(Number::new(0, 4, 1)));
    tokenize_token!(FAIL: number5_tokenize_test, "    0005    ");
    tokenize_token!(FAIL: bad_number_test, "   000aaa    ");
    tokenize_token!(FAIL: bad_number2_test, "    1aaa    ");
    tokenize_multiple_tokens!(tokenize_multiple, "while\n { \n\n\n } ...!!!", 
    TokenKind::While, TokenKind::LeftBracket, TokenKind::RightBracket,
    TokenKind::Comma, TokenKind::Comma, TokenKind::Comma, 
    TokenKind::Not, TokenKind::Not, TokenKind::Not);
    tokenize_multiple_tokens!(tokenize_multiple2, "<<= ! == >=", 
    TokenKind::LessThen, TokenKind::LessEqualThen, TokenKind::Not,
    TokenKind::Equal, TokenKind::GraterEqualThen);

    tokenize_multiple_tokens!(tokenize_multiple3, "main(){ eee = Object(); aaa.Gol = \"bebe\"; } ", 
    TokenKind::Identifier(String::from("main")), TokenKind::LeftParentheses, TokenKind::RightParentheses, TokenKind::LeftBracket,
    TokenKind::Identifier(String::from("eee")), TokenKind::Assignment, TokenKind::Identifier(String::from("Object")),
    TokenKind::LeftParentheses, TokenKind::RightParentheses, TokenKind::Semicolon,
    TokenKind::Identifier(String::from("aaa")), TokenKind::Comma, TokenKind::Identifier(String::from("Gol")),
    TokenKind::Assignment, TokenKind::QuotedString(String::from("bebe")), TokenKind::Semicolon, TokenKind::RightBracket);
}

