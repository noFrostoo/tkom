use std::collections::VecDeque;

use crate::{types::{Token, TokenKind, Position, Number}, file_handler::Source, errors::{ErrorKind, ErrorHandler}};

const ETX: char = 3 as char;
const MAX_IDENT_LEN: u32 = 5000;

pub trait TLexer {
    fn get_next_token(&mut self) -> Result<Token, ErrorKind>;
    fn get_current_token(&self) -> Token;
    fn get_position(&self) -> Position;
}


struct Lexer {
    pos: Position,
    current_char: char,
    token: Token,
    source: Box<dyn Source> 
}

impl TLexer for Lexer {
    fn get_next_token(&mut self) -> Result<Token, ErrorKind> {
        self.skip_whitespace();
        // new line is new lione 

        if self.current_char == ETX{
            return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::EndOfFile));
        }
        
        let res = self.try_tokenize_ident_or_keyword().
            or_else(|_| self.try_build_operator()).
            or_else(|_| self.try_build_comment()).
            or_else(|_| self.try_build_string()).
            or_else(|_| self.try_build_number());

        ErrorHandler::handle_result(res)
    }

    fn get_current_token(&self) -> Token {
        self.token.clone()
    }

    fn get_position(&self) -> Position {
        self.pos.clone()
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

fn skip_whitespace(&mut self) {
    while self.current_char.is_whitespace() {
        self.get_next_char();
    }
}

fn get_next_char(&mut self) -> char {
    match self.source.get_next_char() {
        Ok(read_char) => {
            self.current_char = read_char;
            self.pos.new_char(self.source.current_position());
            read_char
        },
        Err(e) => {
            ErrorHandler::fatal(e);
        }
    }
}


fn try_tokenize_ident_or_keyword(&mut self) -> Result<Token, ErrorKind> {
    if !self.current_char.is_alphabetic() {
        return Err(ErrorKind::UnexpectedCharacter { actual: self.current_char.clone(), expected: String::from("a,b,.."), position: self.pos.clone() });
    }

    let mut value: VecDeque<char> = VecDeque::new();
    let mut len = 0; 
    while self.current_char.is_alphanumeric() && len <= MAX_IDENT_LEN {
        value.push_back(self.current_char);
        len += 1;
        self.get_next_char();
    }

    if len > MAX_IDENT_LEN {
        return Err(ErrorKind::MaxIdentLenExceeded{position: self.pos.clone()});
    }

    let word = String::from_iter(value);

    match word.as_str() {
        "while" => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::While)),
        "return" => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Return)),
        "if" => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::If)),
        "else" =>Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Else)),
        "or" => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Or)),
        "and" =>Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::And)),
        _ => Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Identifier(word)))
    }   
}

fn try_build_operator(&mut self) -> Result<Token, ErrorKind>{
    match self.current_char {
        '>' => {
            self.next_second_char_is('=',TokenKind::GraterEqualThen, TokenKind::GraterThen, false)
        },
        '<' => {
            self.next_second_char_is('=',TokenKind::LessEqualThen, TokenKind::LessThen, false)
        },
        '=' => {
            self.next_second_char_is('=',TokenKind::Equal, TokenKind::Assignment, false)
        },
        '+' => {
            self.next_second_char_is('=',TokenKind::AddAssignment, TokenKind::Addition, false)
        },
        '-' => {
            self.next_second_char_is('=',TokenKind::SubtractAssignment, TokenKind::Subtraction, false)
        },
        '/' => {
            self.next_second_char_is('=',TokenKind::DivisionAssignment, TokenKind::Division, false)
        },
        '*' => {
            self.next_second_char_is('=',TokenKind::MultiplicationAssignment, TokenKind::Multiplication, false)
        },
        '%' => {
            self.next_second_char_is('=',TokenKind::ModuloAssignment, TokenKind::Modulo, false)
        },
        '|' => {
            self.next_second_char_is('=',TokenKind::Or, TokenKind::Unknown, true)
        },
        '&' => {
            self.next_second_char_is('=',TokenKind::And, TokenKind::Unknown, true)
        },
        '!' => {
            self.next_second_char_is('=',TokenKind::NotEqual, TokenKind::Not, false)
        },
        '{' => {
            self.get_next_char();
            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::LeftBracket))
        },
        '}' => {
            self.get_next_char();
            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::RightBracket))
        },
        '(' => {
            self.get_next_char();
            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::LeftParentheses))
        },
        ')' => {
            self.get_next_char();
            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::RightParentheses))
        },
        '.' => {
            self.get_next_char();
            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Comma))
        },
        ';' => {
            self.get_next_char();
            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Semicolon))
        },
        _ => Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("<, >, |, &, =, !, +, -, /, %"), position: self.pos.clone() }),
    }
}

fn next_second_char_is(&mut self, ch: char, if_is: TokenKind, if_not: TokenKind, if_not_err: bool) -> Result<Token, ErrorKind> {
    self.get_next_char();

    if self.current_char == '=' {
        self.get_next_char();
        return Ok(Token::new(self.source.current_position(), self.pos.clone(), if_is))
    } else if if_not_err {
        self.get_next_char();
        return Err(ErrorKind::UnexpectedCharacter { actual: ch, expected: String::from(ch), position: self.pos.clone() })
    }

    Ok(Token::new(self.source.current_position(), self.pos.clone(), if_not))
}

fn try_build_comment(&mut self) -> Result<Token, ErrorKind> {
    if self.current_char != '#' {
        return  Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("#"), position: self.pos.clone() });
    }

    let mut value: VecDeque<char> = VecDeque::new(); 
    self.get_next_char();

    while !self.check_new_line() && self.current_char != ETX {
        value.push_back(self.current_char);
        self.get_next_char();
    }

    let word = String::from_iter(value);

    Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Comment(word)))
}

fn check_new_line(&mut self) -> bool {
    if self.current_char == 10 as char{
        let ch = self.get_next_char();

        if ch == 13 as char {
            self.get_next_char();
            self.pos.new_line(2);
            return true
        }

        self.pos.new_line(1);
        return true
    }

    if self.current_char == 13 as char {
        let ch = self.get_next_char();

        if ch == 10 as char {
            self.get_next_char();

            self.pos.new_line(2);
            return true
        }

        self.pos.new_line(1);
        return true
    }

    if self.current_char == 30 as char {
        self.get_next_char();

        self.pos.new_line(1);
        return true
    }

    false
}

fn try_build_number(&mut self) -> Result<Token, ErrorKind> {
    if !self.current_char.is_digit(10) {
        return Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("number"), position: self.pos.clone() });
    }

    let mut int_part: u64 = 0;
    let mut frac_part: u64 = 0;
    let mut frac_part_len: u64 = 0;

    if self.current_char != '0' {
        while self.current_char.is_digit(10) {
            int_part = int_part * 10 + self.current_char as u64 - '0' as u64;
            self.get_next_char();
        }
    } else {
        self.get_next_char();
    }

    if self.current_char == '.' {
        self.get_next_char();
        while self.current_char.is_digit(10) {
            frac_part = frac_part * 10 + self.current_char as u64 - '0' as u64;
            frac_part_len += 1;
            self.get_next_char();
        }
    }

    if self.current_char.is_digit(10) || self.current_char == '.' || self.current_char.is_alphabetic() {
        return Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("a number"), position: self.pos.clone() });
    }
    //? CHANGE
    Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Number(Number::new(int_part, frac_part, frac_part_len))))
}

fn try_build_string(&mut self) -> Result<Token, ErrorKind> {
    if self.current_char != '"' && self.current_char != '\'' {
        return Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("\", '"), position: self.pos.clone() });
    }

    let opening_char = self.current_char;
    let mut value: VecDeque<char> = VecDeque::new(); 

    // rust getter 
    self.get_next_char();
    while self.current_char != ETX && self.current_char != opening_char && !self.check_new_line() {
        
        if self.current_char == '\\' {
            self.get_next_char();
            match self.current_char {
                'n' => value.push_back('\n'),
                'r' => value.push_back('\r'),
                't' => value.push_back('\t'),
                '\\' => value.push_back('\\'),
                '0' => value.push_back('\0'),
                '"' => value.push_back('"'),
                '\'' => value.push_back('\''),
                _ => return Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("n,r,t,\\,0,"), position: self.pos.clone() })
            }
        } else {
            value.push_back(self.current_char);
        }

        self.get_next_char();
    }

    if self.current_char != opening_char {
        return Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from(opening_char), position: self.pos.clone() });
    } else {
        self.get_next_char();
    }

    let word = String::from_iter(value);

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
    tokenize_token!(string3_tokenize_test, "    'aa\taaa'    ", TokenKind::QuotedString(String::from("aa\taaa")));
    tokenize_token!(FAIL: string4_tokenize_test, "    'aaaaa    ");
    tokenize_token!(number_tokenize_test, "    2137    ", TokenKind::Number(Number::new(2137, 0, 0)));
    tokenize_token!(number2_tokenize_test, "    2137.420    ", TokenKind::Number(Number::new(2137, 420, 3)));
    tokenize_token!(number3_tokenize_test, "    0.4    ", TokenKind::Number(Number::new(0, 4, 1)));
    /*
    ! allow for 0000.4 ?????????
    */
    tokenize_token!(FAIL: number4_tokenize_test, "    0000.4    ");
    tokenize_token!(FAIL: number5_tokenize_test, "    0005    ");
    tokenize_token!(FAIL: bad_number_test, "   000aaa    ");
    tokenize_token!(FAIL: bad_number2_test, "   00    ");
    tokenize_token!(FAIL: bad_number3_test, "   0.0.    ");
    tokenize_token!(FAIL: bad_number4_test, "   0..4    ");
    tokenize_token!(FAIL: bad_number5_test, "    1aaa    ");
    tokenize_multiple_tokens!(tokenize_multiple, "while\n { \n\n\n } ...!!!", 
    TokenKind::While, TokenKind::LeftBracket, TokenKind::RightBracket,
    TokenKind::Comma, TokenKind::Comma, TokenKind::Comma, 
    TokenKind::Not, TokenKind::Not, TokenKind::Not);
    tokenize_multiple_tokens!(tokenize_multiple2, "<<= \n\n! \n\n== >=", 
    TokenKind::LessThen, TokenKind::LessEqualThen, TokenKind::Not,
    TokenKind::Equal, TokenKind::GraterEqualThen);

    tokenize_multiple_tokens!(tokenize_multiple3, "main()\n\n{ \n\n\n eee = Object(); \n\n  aaa.Gol = \"bebe\"; \n\n } \n\n", 
    TokenKind::Identifier(String::from("main")), TokenKind::LeftParentheses, TokenKind::RightParentheses, TokenKind::LeftBracket,
    TokenKind::Identifier(String::from("eee")), TokenKind::Assignment, TokenKind::Identifier(String::from("Object")),
    TokenKind::LeftParentheses, TokenKind::RightParentheses, TokenKind::Semicolon,
    TokenKind::Identifier(String::from("aaa")), TokenKind::Comma, TokenKind::Identifier(String::from("Gol")),
    TokenKind::Assignment, TokenKind::QuotedString(String::from("bebe")), TokenKind::Semicolon, TokenKind::RightBracket);
}
