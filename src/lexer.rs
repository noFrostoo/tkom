use std::collections::VecDeque;

use rust_decimal::Decimal;

use crate::{
    errors::{ErrorHandler, ErrorKind},
    file_handler::Source,
    types::{Position, Token, TokenKind},
};

const ETX: char = 3 as char;
const MAX_IDENT_LEN: u32 = 5000;
const LF: char = 10 as char;
const CR: char = 13 as char;
const RS: char = 30 as char;

pub trait TLexer {
    fn get_next_token(&mut self) -> Option<Result<Token, ErrorKind>>;
    fn get_current_token(&self) -> Token;
    fn get_position(&self) -> Position;
}

pub struct Lexer {
    pos: Position,
    current_char: char,
    token: Token,
    source: Box<dyn Source>,
}

impl TLexer for Lexer {
    fn get_next_token(&mut self) -> Option<Result<Token, ErrorKind>> {
        self.skip_whitespace();

        if self.current_char == ETX {
            return None;
        }

        let res = self
            .try_tokenize_ident_or_keyword()
            .or_else(|| self.try_build_operator())
            .or_else(|| self.try_build_comment())
            .or_else(|| self.try_build_string())
            .or_else(|| self.try_build_number());

        match res {
            Some(v) => match v {
                Ok(token) => {
                    self.token = token.clone();
                    Some(Ok(token))
                }
                Err(err) => {
                    ErrorHandler::report_error(err.clone());
                    return Some(Err(err));
                }
            },
            None => {
                let err = ErrorKind::UnexpectedCharacter {
                    actual: self.current_char,
                    expected: String::from(""),
                    position: self.pos.clone(),
                };
                self.get_next_char();
                ErrorHandler::report_error(err.clone());
                Some(Err(err))
            }
        }
    }

    fn get_current_token(&self) -> Token {
        self.token.clone()
    }

    fn get_position(&self) -> Position {
        self.pos.clone()
    }
}

impl Iterator for Lexer {
    type Item = Result<Token, ErrorKind>;

    fn next(&mut self) -> Option<Self::Item> {
        self.get_next_token()
    }
}

impl Lexer {
    pub fn new(source: Box<dyn Source>) -> Lexer {
        Lexer {
            source: source,
            pos: Position::zero(),
            current_char: ' ',
            token: Token::new(0, Position::zero(), TokenKind::Unknown),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.current_char.is_whitespace() {
            if !self.check_new_line() {
                self.get_next_char();
            }
        }
    }

    fn get_next_char(&mut self) -> char {
        match self.source.get_next_char() {
            Ok(read_char) => {
                self.current_char = read_char;
                self.pos.new_char(self.source.current_position());
                read_char
            }
            Err(e) => {
                ErrorHandler::fatal(e);
            }
        }
    }

    fn try_tokenize_ident_or_keyword(&mut self) -> Option<Result<Token, ErrorKind>> {
        if !self.current_char.is_alphabetic() {
            return None;
        }

        let mut value: VecDeque<char> = VecDeque::new();
        let mut len = 0;
        while self.current_char.is_alphanumeric() && len <= MAX_IDENT_LEN {
            value.push_back(self.current_char);
            len += 1;
            self.get_next_char();
        }

        if len > MAX_IDENT_LEN {
            return Some(Err(ErrorKind::MaxIdentLenExceeded {
                position: self.pos.clone(),
            }));
        }

        let word = String::from_iter(value);

        match word.as_str() {
            "while" => Some(Ok(Token::new(
                self.source.current_position(),
                self.pos.clone(),
                TokenKind::While,
            ))),
            "return" => Some(Ok(Token::new(
                self.source.current_position(),
                self.pos.clone(),
                TokenKind::Return,
            ))),
            "if" => Some(Ok(Token::new(
                self.source.current_position(),
                self.pos.clone(),
                TokenKind::If,
            ))),
            "else" => Some(Ok(Token::new(
                self.source.current_position(),
                self.pos.clone(),
                TokenKind::Else,
            ))),
            "or" => Some(Ok(Token::new(
                self.source.current_position(),
                self.pos.clone(),
                TokenKind::Or,
            ))),
            "and" => Some(Ok(Token::new(
                self.source.current_position(),
                self.pos.clone(),
                TokenKind::And,
            ))),
            _ => Some(Ok(Token::new(
                self.source.current_position(),
                self.pos.clone(),
                TokenKind::Identifier(word),
            ))),
        }
    }

    fn try_build_operator(&mut self) -> Option<Result<Token, ErrorKind>> {
        match self.current_char {
            '>' => self.next_second_char_is(
                '=',
                TokenKind::GraterEqualThen,
                TokenKind::GraterThen,
                false,
            ),
            '<' => {
                self.next_second_char_is('=', TokenKind::LessEqualThen, TokenKind::LessThen, false)
            }
            '=' => self.next_second_char_is('=', TokenKind::Equal, TokenKind::Assignment, false),
            '+' => {
                self.next_second_char_is('=', TokenKind::AddAssignment, TokenKind::Addition, false)
            }
            '-' => self.next_second_char_is(
                '=',
                TokenKind::SubtractAssignment,
                TokenKind::Subtraction,
                false,
            ),
            '/' => self.next_second_char_is(
                '=',
                TokenKind::DivisionAssignment,
                TokenKind::Division,
                false,
            ),
            '*' => self.next_second_char_is(
                '=',
                TokenKind::MultiplicationAssignment,
                TokenKind::Multiplication,
                false,
            ),
            '%' => {
                self.next_second_char_is('=', TokenKind::ModuloAssignment, TokenKind::Modulo, false)
            }
            '|' => self.next_second_char_is('=', TokenKind::Or, TokenKind::Unknown, true),
            '&' => self.next_second_char_is('=', TokenKind::And, TokenKind::Unknown, true),
            '!' => self.next_second_char_is('=', TokenKind::NotEqual, TokenKind::Not, false),
            '{' => {
                self.get_next_char();
                Some(Ok(Token::new(
                    self.source.current_position(),
                    self.pos.clone(),
                    TokenKind::LeftBracket,
                )))
            }
            '}' => {
                self.get_next_char();
                Some(Ok(Token::new(
                    self.source.current_position(),
                    self.pos.clone(),
                    TokenKind::RightBracket,
                )))
            }
            '(' => {
                self.get_next_char();
                Some(Ok(Token::new(
                    self.source.current_position(),
                    self.pos.clone(),
                    TokenKind::LeftParentheses,
                )))
            }
            ')' => {
                self.get_next_char();
                Some(Ok(Token::new(
                    self.source.current_position(),
                    self.pos.clone(),
                    TokenKind::RightParentheses,
                )))
            }
            '.' => {
                self.get_next_char();
                Some(Ok(Token::new(
                    self.source.current_position(),
                    self.pos.clone(),
                    TokenKind::Dot,
                )))
            }
            ',' => {
                self.get_next_char();
                Some(Ok(Token::new(
                    self.source.current_position(),
                    self.pos.clone(),
                    TokenKind::Comma,
                )))
            }
            ';' => {
                self.get_next_char();
                Some(Ok(Token::new(
                    self.source.current_position(),
                    self.pos.clone(),
                    TokenKind::Semicolon,
                )))
            }
            _ => return None,
        }
    }

    fn next_second_char_is(
        &mut self,
        ch: char,
        if_is: TokenKind,
        if_not: TokenKind,
        if_not_err: bool,
    ) -> Option<Result<Token, ErrorKind>> {
        self.get_next_char();

        if self.current_char == '=' {
            self.get_next_char();
            return Some(Ok(Token::new(
                self.source.current_position(),
                self.pos.clone(),
                if_is,
            )));
        } else if if_not_err {
            self.get_next_char();
            return Some(Err(ErrorKind::UnexpectedCharacter {
                actual: ch,
                expected: String::from(ch),
                position: self.pos.clone(),
            }));
        }

        Some(Ok(Token::new(
            self.source.current_position(),
            self.pos.clone(),
            if_not,
        )))
    }

    fn try_build_comment(&mut self) -> Option<Result<Token, ErrorKind>> {
        if self.current_char != '#' {
            return None;
        }

        let mut value: VecDeque<char> = VecDeque::new();
        self.get_next_char();

        while !self.check_new_line() && self.current_char != ETX {
            value.push_back(self.current_char);
            self.get_next_char();
        }

        let word = String::from_iter(value);

        Some(Ok(Token::new(
            self.source.current_position(),
            self.pos.clone(),
            TokenKind::Comment(word),
        )))
    }

    fn check_new_line(&mut self) -> bool {
        if self.current_char == LF as char {
            let ch = self.get_next_char();

            if ch == CR as char {
                self.get_next_char();
                self.pos.new_line(2);
                return true;
            }

            self.pos.new_line(1);
            return true;
        }

        if self.current_char == LF as char {
            let ch = self.get_next_char();

            if ch == CR as char {
                self.get_next_char();

                self.pos.new_line(2);
                return true;
            }

            self.pos.new_line(1);
            return true;
        }

        if self.current_char == RS as char {
            self.get_next_char();

            self.pos.new_line(1);
            return true;
        }

        false
    }

    fn try_build_number(&mut self) -> Option<Result<Token, ErrorKind>> {
        if !self.current_char.is_digit(10) {
            return None;
        }

        let mut num: i64 = 0;
        let mut frac_part_len: u32 = 0;

        if self.current_char != '0' {
            while self.current_char.is_digit(10) {
                num = num * 10 + self.current_char as i64 - '0' as i64;
                self.get_next_char();
            }
        } else {
            self.get_next_char();
        }

        if self.current_char == '.' {
            self.get_next_char();
            while self.current_char.is_digit(10) {
                num = num * 10 + self.current_char as i64 - '0' as i64;
                frac_part_len += 1;
                self.get_next_char();
            }
        }

        if self.current_char.is_digit(10)
            || self.current_char == '.'
            || self.current_char.is_alphabetic()
        {
            return Some(Err(ErrorKind::UnexpectedCharacter {
                actual: self.current_char,
                expected: String::from("a number"),
                position: self.pos.clone(),
            }));
        }
        Some(Ok(Token::new(
            self.source.current_position(),
            self.pos.clone(),
            TokenKind::Number(Decimal::new(num, frac_part_len)),
        )))
    }

    fn try_build_string(&mut self) -> Option<Result<Token, ErrorKind>> {
        if self.current_char != '"' && self.current_char != '\'' {
            return None;
        }

        let opening_char = self.current_char;
        let mut value: VecDeque<char> = VecDeque::new();

        self.get_next_char();
        while self.current_char != ETX && self.current_char != opening_char {
            value.push_back(self.current_char);
            self.get_next_char();
        }

        if self.current_char != opening_char {
            return Some(Err(ErrorKind::UnexpectedCharacter {
                actual: self.current_char,
                expected: String::from(opening_char),
                position: self.pos.clone(),
            }));
        } else {
            self.get_next_char();
        }

        let word = String::from_iter(value);

        Some(Ok(Token::new(
            self.source.current_position(),
            self.pos.clone(),
            TokenKind::QuotedString(word),
        )))
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
                assert!(lexer.get_next_token().unwrap().is_err())
            }
        };
        ($name:ident, $text:expr, $token:expr) => {
            #[test]
            fn $name() {
                let text: &str = $text;
                let should_be: TokenKind = $token;

                let test_source = file_handler::TestSource::new(String::from(text), 0);
                let mut lexer = Lexer::new(Box::new(test_source));
                match lexer.get_next_token().unwrap() {
                    Ok(ch) => assert_eq!(ch.kind, should_be),
                    Err(_) => panic!(),
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
                        match lexer.get_next_token().unwrap() {
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
                    match lexer.get_next_token().unwrap() {
                        Ok(ch) => assert_eq!(ch.kind, should_be),
                        Err(_) => panic!()
                    }
                )+
            }
        };
    }
    use rust_decimal_macros::dec;

    use crate::{
        file_handler::{self, FileSource},
        types::TokenKind,
    };

    use super::{Lexer, TLexer};

    tokenize_token!(
        ident_tokenize_test,
        "    aaaaa    ",
        TokenKind::Identifier(String::from("aaaaa"))
    );
    tokenize_token!(while_tokenize_test, "   while   ", TokenKind::While);
    tokenize_token!(if_tokenize_test, "   return   ", TokenKind::Return);
    tokenize_token!(return_tokenize_test, "   if   ", TokenKind::If);
    tokenize_token!(else_tokenize_test, "   else   ", TokenKind::Else);
    tokenize_token!(plus_tokenize_test, "   +   ", TokenKind::Addition);
    tokenize_token!(subtract_tokenize_test, "   -   ", TokenKind::Subtraction);
    tokenize_token!(assignment_tokenize_test, "   =   ", TokenKind::Assignment);
    tokenize_token!(
        division_assignment_tokenize_test,
        "   /=   ",
        TokenKind::DivisionAssignment
    );
    tokenize_token!(
        add_assignment_tokenize_test,
        "   +=   ",
        TokenKind::AddAssignment
    );
    tokenize_token!(
        subtract_assignment_tokenize_test,
        "   -=   ",
        TokenKind::SubtractAssignment
    );
    tokenize_token!(
        modulo_assignment_tokenize_test,
        "   %=   ",
        TokenKind::ModuloAssignment
    );
    tokenize_token!(
        multiplication_assignment_tokenize_test,
        "   *=   ",
        TokenKind::MultiplicationAssignment
    );
    tokenize_token!(division_tokenize_test, "   /   ", TokenKind::Division);
    tokenize_token!(
        multiplication_tokenize_test,
        "   *   ",
        TokenKind::Multiplication
    );
    tokenize_token!(modulo_tokenize_test, "   %   ", TokenKind::Modulo);
    tokenize_token!(not_tokenize_test, "   !   ", TokenKind::Not);
    tokenize_token!(not_equal_tokenize_test, "   !=   ", TokenKind::NotEqual);
    tokenize_token!(
        open_bracket_tokenize_test,
        "   {   ",
        TokenKind::LeftBracket
    );
    tokenize_token!(
        close_bracket_tokenize_test,
        "   }   ",
        TokenKind::RightBracket
    );
    tokenize_token!(
        open_parentheses_tokenize_test,
        "   (   ",
        TokenKind::LeftParentheses
    );
    tokenize_token!(
        close_parentheses_tokenize_test,
        "   )   ",
        TokenKind::RightParentheses
    );
    tokenize_token!(comma_tokenize_test, "   ,   ", TokenKind::Comma);
    tokenize_token!(dot_tokenize_test, "   .   ", TokenKind::Dot);
    tokenize_token!(
        comment_tokenize_test,
        "    #aaaaa    ",
        TokenKind::Comment(String::from("aaaaa    "))
    );
    tokenize_token!(
        comment_newline_tokenize_test,
        "    #aaaaa\n    ",
        TokenKind::Comment(String::from("aaaaa"))
    );
    tokenize_token!(
        string_tokenize_test,
        "    \"aaaaa\"    ",
        TokenKind::QuotedString(String::from("aaaaa"))
    );
    tokenize_token!(
        string2_tokenize_test,
        "    'aaaaa'    ",
        TokenKind::QuotedString(String::from("aaaaa"))
    );
    tokenize_token!(
        string3_tokenize_test,
        "    'a\ra\\a\ta\naa'    ",
        TokenKind::QuotedString(String::from("a\ra\\a\ta\naa"))
    );
    tokenize_token!(FAIL: string4_tokenize_test, "    'aaaaa    ");
    tokenize_token!(FAIL: string5_tokenize_test, "    \"aaaaa    ");
    tokenize_token!(
        number_tokenize_test,
        "    2137    ",
        TokenKind::Number(dec!(2137))
    );
    tokenize_token!(
        number2_tokenize_test,
        "    2137.420    ",
        TokenKind::Number(dec!(2137.420))
    );
    tokenize_token!(
        number3_tokenize_test,
        "    0.4    ",
        TokenKind::Number(dec!(0.4))
    );
    tokenize_token!(FAIL: number4_tokenize_test, "    0000.4    ");
    tokenize_token!(FAIL: number5_tokenize_test, "    0005    ");
    tokenize_token!(FAIL: bad_number_test, "   000aaa    ");
    tokenize_token!(FAIL: bad_number2_test, "   00    ");
    tokenize_token!(FAIL: bad_number3_test, "   0.0.    ");
    tokenize_token!(FAIL: bad_number4_test, "   0..4    ");
    tokenize_token!(FAIL: bad_number5_test, "    1aaa    ");
    tokenize_multiple_tokens!(
        tokenize_multiple,
        "while\n { \n\n\n } ...!!!",
        TokenKind::While,
        TokenKind::LeftBracket,
        TokenKind::RightBracket,
        TokenKind::Dot,
        TokenKind::Dot,
        TokenKind::Dot,
        TokenKind::Not,
        TokenKind::Not,
        TokenKind::Not
    );
    tokenize_multiple_tokens!(
        tokenize_multiple2,
        "<<= \n\n! \n\n== >=",
        TokenKind::LessThen,
        TokenKind::LessEqualThen,
        TokenKind::Not,
        TokenKind::Equal,
        TokenKind::GraterEqualThen
    );

    tokenize_multiple_tokens!(
        tokenize_multiple3,
        "main()\n\n{ \n\n\n eee = Object(); \n\n  aaa.Gol = \"bebe\"; \n\n } \n\n",
        TokenKind::Identifier(String::from("main")),
        TokenKind::LeftParentheses,
        TokenKind::RightParentheses,
        TokenKind::LeftBracket,
        TokenKind::Identifier(String::from("eee")),
        TokenKind::Assignment,
        TokenKind::Identifier(String::from("Object")),
        TokenKind::LeftParentheses,
        TokenKind::RightParentheses,
        TokenKind::Semicolon,
        TokenKind::Identifier(String::from("aaa")),
        TokenKind::Dot,
        TokenKind::Identifier(String::from("Gol")),
        TokenKind::Assignment,
        TokenKind::QuotedString(String::from("bebe")),
        TokenKind::Semicolon,
        TokenKind::RightBracket
    );

    #[test]
    fn test_real_file() {
        let lines:[&str; 71] = [
            "TOKEN: kind: Identifier(\"Describe\"), pos: line: 1, column: 10, offset: 9 \n",
            "TOKEN: kind: LeftParentheses, pos: line: 1, column: 11, offset: 10 \n",
            "TOKEN: kind: Identifier(\"p\"), pos: line: 1, column: 12, offset: 11 \n",
            "TOKEN: kind: RightParentheses, pos: line: 1, column: 13, offset: 12 \n",
            "TOKEN: kind: LeftBracket, pos: line: 1, column: 15, offset: 14 \n",
            "TOKEN: kind: Identifier(\"print\"), pos: line: 2, column: 9, offset: 24 \n",
            "TOKEN: kind: LeftParentheses, pos: line: 2, column: 10, offset: 25 \n",
            "TOKEN: kind: Identifier(\"person\"), pos: line: 2, column: 16, offset: 31 \n",
            "TOKEN: kind: Dot, pos: line: 2, column: 17, offset: 32 \n",
            "TOKEN: kind: Identifier(\"name\"), pos: line: 2, column: 21, offset: 36 \n",
            "TOKEN: kind: Comma, pos: line: 2, column: 22, offset: 37 \n",
            "TOKEN: kind: QuotedString(\"has\"), pos: line: 2, column: 28, offset: 43 \n",
            "TOKEN: kind: RightParentheses, pos: line: 2, column: 29, offset: 44 \n",
            "TOKEN: kind: Identifier(\"for\"), pos: line: 3, column: 7, offset: 52 \n",
            "TOKEN: kind: Identifier(\"c\"), pos: line: 3, column: 9, offset: 54 \n",
            "TOKEN: kind: Identifier(\"in\"), pos: line: 3, column: 12, offset: 57 \n",
            "TOKEN: kind: Identifier(\"p\"), pos: line: 3, column: 14, offset: 59 \n",
            "TOKEN: kind: LeftBracket, pos: line: 3, column: 16, offset: 61 \n",
            "TOKEN: kind: Identifier(\"print\"), pos: line: 4, column: 13, offset: 75 \n",
            "TOKEN: kind: LeftParentheses, pos: line: 4, column: 14, offset: 76 \n",
            "TOKEN: kind: Identifier(\"c\"), pos: line: 4, column: 15, offset: 77 \n",
            "TOKEN: kind: RightParentheses, pos: line: 4, column: 16, offset: 78 \n",
            "TOKEN: kind: RightBracket, pos: line: 5, column: 5, offset: 84 \n",
            "TOKEN: kind: RightBracket, pos: line: 6, column: 1, offset: 86 \n",
            "TOKEN: kind: Identifier(\"main\"), pos: line: 8, column: 4, offset: 92 \n",
            "TOKEN: kind: LeftParentheses, pos: line: 8, column: 6, offset: 94 \n",
            "TOKEN: kind: RightParentheses, pos: line: 8, column: 7, offset: 95 \n",
            "TOKEN: kind: LeftBracket, pos: line: 8, column: 9, offset: 97 \n",
            "TOKEN: kind: Identifier(\"name\"), pos: line: 9, column: 8, offset: 106 \n",
            "TOKEN: kind: Assignment, pos: line: 9, column: 10, offset: 108 \n",
            "TOKEN: kind: Identifier(\"input\"), pos: line: 9, column: 16, offset: 114 \n",
            "TOKEN: kind: LeftParentheses, pos: line: 9, column: 17, offset: 115 \n",
            "TOKEN: kind: RightParentheses, pos: line: 9, column: 18, offset: 116 \n",
            "TOKEN: kind: Identifier(\"lastName\"), pos: line: 10, column: 12, offset: 129 \n",
            "TOKEN: kind: Assignment, pos: line: 10, column: 14, offset: 131 \n",
            "TOKEN: kind: Identifier(\"input\"), pos: line: 10, column: 20, offset: 137 \n",
            "TOKEN: kind: LeftParentheses, pos: line: 10, column: 21, offset: 138 \n",
            "TOKEN: kind: RightParentheses, pos: line: 10, column: 22, offset: 139 \n",
            "TOKEN: kind: If, pos: line: 12, column: 6, offset: 147 \n",
            "TOKEN: kind: Identifier(\"name\"), pos: line: 12, column: 11, offset: 152 \n",
            "TOKEN: kind: Equal, pos: line: 12, column: 14, offset: 155 \n",
            "TOKEN: kind: QuotedString(\"Daniel\"), pos: line: 12, column: 23, offset: 164 \n",
            "TOKEN: kind: LeftBracket, pos: line: 12, column: 25, offset: 166 \n",
            "TOKEN: kind: Identifier(\"print\"), pos: line: 13, column: 13, offset: 180 \n",
            "TOKEN: kind: LeftParentheses, pos: line: 13, column: 14, offset: 181 \n",
            "TOKEN: kind: QuotedString(\"Hello Daniel\"), pos: line: 13, column: 28, offset: 195 \n",
            "TOKEN: kind: RightParentheses, pos: line: 13, column: 29, offset: 196 \n",
            "TOKEN: kind: RightBracket, pos: line: 14, column: 5, offset: 202 \n",
            "TOKEN: kind: Else, pos: line: 14, column: 10, offset: 207 \n",
            "TOKEN: kind: LeftBracket, pos: line: 14, column: 12, offset: 209 \n", 
            "TOKEN: kind: Identifier(\"print\"), pos: line: 15, column: 13, offset: 223 \n",
            "TOKEN: kind: LeftParentheses, pos: line: 15, column: 14, offset: 224 \n",
            "TOKEN: kind: QuotedString(\"Hello\"), pos: line: 15, column: 21, offset: 231 \n",
            "TOKEN: kind: Comma, pos: line: 15, column: 22, offset: 232 \n",
            "TOKEN: kind: Identifier(\"name\"), pos: line: 15, column: 27, offset: 237 \n",
            "TOKEN: kind: RightParentheses, pos: line: 15, column: 28, offset: 238 \n",
            "TOKEN: kind: RightBracket, pos: line: 16, column: 5, offset: 244 \n",
            "TOKEN: kind: Identifier(\"person\"), pos: line: 18, column: 10, offset: 256 \n",
            "TOKEN: kind: Dot, pos: line: 18, column: 11, offset: 257 \n",
            "TOKEN: kind: Identifier(\"Describe\"), pos: line: 18, column: 19, offset: 265 \n",
            "TOKEN: kind: Assignment, pos: line: 18, column: 21, offset: 267 \n",
            "TOKEN: kind: Identifier(\"Describe\"), pos: line: 18, column: 30, offset: 276 \n",
            "TOKEN: kind: Semicolon, pos: line: 18, column: 31, offset: 277 \n",
            "TOKEN: kind: Identifier(\"person\"), pos: line: 20, column: 10, offset: 289 \n",
            "TOKEN: kind: Dot, pos: line: 20, column: 11, offset: 290 \n",
            "TOKEN: kind: Identifier(\"Describe\"), pos: line: 20, column: 19, offset: 298 \n",
            "TOKEN: kind: LeftParentheses, pos: line: 20, column: 20, offset: 299 \n",
            "TOKEN: kind: Identifier(\"person\"), pos: line: 20, column: 26, offset: 305 \n",
            "TOKEN: kind: RightParentheses, pos: line: 20, column: 27, offset: 306 \n",
            "TOKEN: kind: Semicolon, pos: line: 20, column: 28, offset: 307 \n",
            "TOKEN: kind: RightBracket, pos: line: 21, column: 1, offset: 308 \n",
        ];
        let mut i = 0;
        let fs = match FileSource::new(String::from("testFile.ss")) {
            Ok(f) => f,
            Err(err) => panic!("{}", err),
        };
        let lex = Lexer::new(Box::new(fs));
        for result in lex {
            match result {
                Ok(token) => {
                    assert_eq!(token.to_string(), lines[i]);
                    i += 1
                }
                Err(_) => eprint!("Error"),
            }
        }
    }
}
