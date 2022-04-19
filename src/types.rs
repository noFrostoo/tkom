use rust_decimal::Decimal;

#[derive(Clone, Debug, PartialEq)]
pub struct Position {
    //byte offset from begin of the file 
    pub offset: u64,
    pub line: u64,
    pub column: u64,
}

/*
!  How to track white space, create new pos and move it along
! or just create pos on token creation, and keep position in lexer
! i can keep posion in lexer and on token creation just make a copy, but that will point to end of the token not beging  
*/

//TODO: refactor s,l,c names
impl Position {
    pub fn new(offset:u64, line:u64, column:u64) -> Position {
        Position { offset: (offset), line: (line), column: (column) }
    }

    pub fn zero() -> Position {
        Position { offset: (0), line: (0), column: (0) }
    }

    pub fn move_pos(&mut self, offset:u64, line:u64, column:u64) {
        self.offset = offset;
        self.line = line;
        self.column = column;
    }

    pub fn new_char(&mut self, new_steam_pos: u64) {
        self.offset = new_steam_pos;
        self.column += 1;
    }

    pub fn new_line(&mut self, new_steam_pos: u64) {
        self.column = 0;
        self.line += 1;
        self.offset = new_steam_pos;
    }

}

#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub enum TokenKind {
    Number(Decimal),
    Identifier(String),
    QuotedString(String), //? can this work like this or do i need to have string and quotation mark as different token kinds 
    Comment(String),
    While,
    If,
    Else,
    Return,
    // brackets
    LeftBracket,
    RightBracket,
    LeftParentheses,
    RightParentheses,
    Comma,
    Dot,
    // relation operators
    GraterThen,
    LessThen,
    GraterEqualThen,
    LessEqualThen,
    Equal,
    NotEqual,
    // Logical
    And,
    Or,
    Not,
    // Arithmetic operators
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    // Assignment operators
    Assignment,
    AddAssignment,
    SubtractAssignment,
    MultiplicationAssignment,
    DivisionAssignment,
    ModuloAssignment,

    Semicolon,

    EndOfFile,
    Unknown,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    /// The token's location relative to the rest of the files being 
    /// processed in bytes.
    pub offset: u64,

    pub position: Position,
    /// What kind of token is this?
    pub kind: TokenKind,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TOKEN: kind: {:?}, pos: line: {}, column: {}, offset: {} \n", self.kind, self.position.line, self.position.column, self.offset)
    }
}

impl Token {
    /// Create a new token out of a `Span` and something which can be turned 
    /// into a `TokenKind`.
    pub fn new<K: Into<TokenKind>>(offset: u64, position: Position, kind: K) -> Token {
        let kind = kind.into();
        Token { offset, position ,kind }
    }
}



#[cfg(test)]
mod test {
    use super::Position;

    const OFFSET: u64 = 50;
    const LINE: u64 = 3;
    const COLUMN: u64 = 5;

    fn create_pos() -> Position {
        Position::new(OFFSET, LINE, COLUMN)
    }

    #[test]
    fn test_pos_new() {
        let pos = create_pos();
        assert_eq!(pos.offset, OFFSET);
        assert_eq!(pos.line, LINE);
        assert_eq!(pos.column, COLUMN);
    }

    #[test]
    fn test_pos_move() {
        let mut pos = create_pos();
        pos.move_pos(10, 1, 1);
        assert_eq!(pos.offset, 10);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.column, 1);
    }

    #[test]
    fn test_pos_new_char() {
        let mut pos = create_pos();
        pos.new_char(54);
        assert_eq!(pos.offset, 54);
        assert_eq!(pos.line, LINE);
        assert_eq!(pos.column, COLUMN+1);
    }

    #[test]

    fn test_pos_newline() {
        let mut pos = create_pos();
        pos.new_line(2);
        assert_eq!(pos.offset, 2);
        assert_eq!(pos.line, LINE+1);
        assert_eq!(pos.column, 0);
    }


}
