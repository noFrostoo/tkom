use std::{collections::{VecDeque}, process::exit};

use crate::{types::{Token, TokenKind, Position, Number}, file_handler::Source, errors::{ErrorKind, ErrorHandler}};

const ETX: char = 3 as char;

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

        self.skip_new_lines();

        if self.current_char == ETX{
            return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::EndOfFile));
        }
        
        let res = self.try_tokenize_ident_or_keyword().
            or_else(|_| self.try_build_operator()).
            or_else(|_| self.try_build_parentheses_or_comma()).
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

    while !self.current_char.is_whitespace() && self.current_char.is_alphanumeric() {
        value.push_back(self.current_char);
        self.get_next_char();
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
            let ch = self.get_next_char();

            if ch == '=' {
                self.get_next_char();
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::GraterEqualThen))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::GraterThen))
        },
        '<' => {
            let ch = self.get_next_char();

            if ch == '=' {
                self.get_next_char();
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::LessEqualThen))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::LessThen))
        },
        '=' => {
            let ch = self.get_next_char();

            if ch == '=' {
                self.get_next_char();
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Equal))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Assignment))
        },
        '+' => {
            let ch = self.get_next_char();
            if ch == '=' {
                self.get_next_char();
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::AddAssignment))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Addition))
        },
        '-' => {
            let ch = self.get_next_char();

            if ch == '=' {
                self.get_next_char();
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::SubtractAssignment))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Subtraction))
        },
        '/' => {
            let ch = self.get_next_char();

            if ch == '=' {
                self.get_next_char();
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::DivisionAssignment))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Division))
        },
        '*' => {
            let ch = self.get_next_char();

            if ch == '=' {
                self.get_next_char();
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::MultiplicationAssignment))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Multiplication))
        },
        '%' => {
            let ch = self.get_next_char();

            if ch == '=' {
                self.get_next_char();
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::ModuloAssignment))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Modulo))
        },
        '|' => {
            let ch = self.get_next_char();

            /*
            ! Report error
            */
            if ch != '|' {
                self.get_next_char();
                return Err(ErrorKind::UnexpectedCharacter { actual: ch, expected: String::from("|"), position: self.pos.clone() })
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Or))
        },
        '&' => {
            let ch = self.get_next_char();

            /*
            ! Report error
            */
            if ch != '&' {
                self.get_next_char();
                return Err(ErrorKind::UnexpectedCharacter { actual: ch, expected: String::from("&"), position: self.pos.clone() })
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::And))
        },
        '!' => {
            let ch = self.get_next_char();

            if ch == '=' {
                self.get_next_char();
                return Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::NotEqual))
            }

            Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Not))
        },
        _ => Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("<, >, |, &, =, !, +, -, /, %"), position: self.pos.clone() }),
    }
}

/*
! Refactor name
*/
fn try_build_parentheses_or_comma(&mut self) -> Result<Token, ErrorKind> {
    let res: Result<Token, ErrorKind>;
    
    match self.current_char {
        '{' => res = Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::LeftBracket)),
        '}' => res = Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::RightBracket)),
        '(' => res = Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::LeftParentheses)),
        ')' => res = Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::RightParentheses)),
        '.' => res = Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Comma)),
        ';' => res = Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Semicolon)),
        _ => res = Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("{, }, (, ), ."), position: self.pos.clone() }),
    }

    if res.is_ok() {
        self.get_next_char();
    }

    res
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

fn skip_new_lines(&mut self){
    while self.check_new_line() {
    }
}

fn try_build_number(&mut self) -> Result<Token, ErrorKind> {
    if !self.current_char.is_digit(10) {
        return Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("number"), position: self.pos.clone() });
    }

    let mut int_part: u64 = 0;
    let mut frac_part: u64 = 0;
    let mut frac_part_len: u64 = 0;
    let mut zero_count: u32 = 0; //? is this good ?? 

    while self.current_char == '0' {
        zero_count += 1;
        self.get_next_char();
    }

    if self.current_char != '.' && !self.current_char.is_digit(10) {
        return  Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("number"), position: self.pos.clone() });
    }

    if zero_count == 0 && self.current_char != '.' {
        int_part += self.current_char as u64 - '0' as u64 ;

        let mut ch = self.get_next_char();
        
        while ch.is_digit(10) {
            int_part = int_part * 10 + ch as u64 - '0' as u64;
            ch = self.get_next_char();
        }
    }

    if self.current_char == '.' {
        zero_count = 0;
        let mut ch = self.get_next_char();

        if ch == ETX {
            return Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("a number"), position: self.pos.clone() })
        }

        while ch.is_digit(10) {
            frac_part = frac_part * 10 + ch as u64 - '0' as u64;
            frac_part_len += 1;
            ch = self.get_next_char();
        }
    } else if !self.current_char.is_whitespace() && !self.current_char.is_control() {
        return Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("a number"), position: self.pos.clone() });
    }

    if zero_count > 0 {
        return Err(ErrorKind::BadNumber { number: Number::new(int_part, frac_part, frac_part_len), zero_count: zero_count, position: self.pos.clone() });
    }
        

    Ok(Token::new(self.source.current_position(), self.pos.clone(), TokenKind::Number(Number::new(int_part, frac_part, frac_part_len))))
}

fn try_build_string(&mut self) -> Result<Token, ErrorKind> {
    if self.current_char != '"' && self.current_char != '\'' {
        return Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("\", '"), position: self.pos.clone() });
    }

    let mut value: VecDeque<char> = VecDeque::new(); 
    let mut closing_quote = false;

    // ? is this correct ?
    while !self.current_char.is_control() {
        let ch = self.get_next_char();

        if ch == '"' || ch == '\'' {
            closing_quote = true;
            break;
        }

        /*
        ? allow for escape chars? what disallow in string 
        */
        if ch == ETX {
            return Err(ErrorKind::UnexpectedEOF{position: self.pos.clone()});
        }

        if self.check_new_line() {
            return Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("\", '"), position: self.pos.clone() });
        }

        value.push_back(ch);
    }

    if !closing_quote {
        return Err(ErrorKind::UnexpectedCharacter { actual: self.current_char, expected: String::from("\", '"), position: self.pos.clone() });
    }

    let word = String::from_iter(value);

    self.get_next_char();

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
