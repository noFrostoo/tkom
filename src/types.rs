use std::{
    collections::{HashMap, VecDeque},
    rc::Rc,
};

use rust_decimal::Decimal;

#[derive(Clone, Debug, PartialEq)]
pub struct Position {
    //byte offset from begin of the file
    pub offset: u64,
    pub line: u64,
    pub column: u64,
}

impl Position {
    pub fn new(offset: u64, line: u64, column: u64) -> Position {
        Position {
            offset: (offset),
            line: (line),
            column: (column),
        }
    }

    pub fn zero() -> Position {
        Position {
            offset: (0),
            line: (1),
            column: (1),
        }
    }

    pub fn move_pos(&mut self, offset: u64, line: u64, column: u64) {
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

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum TokenKind {
    Number(Decimal),
    Identifier(String),
    QuotedString(String),
    Comment(String),
    While,
    If,
    Else,
    Return,
    Has,
    For,
    In,
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
    Unknown,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    /// The token's location relative to the rest of the files being
    /// processed in bytes.
    pub offset: u64,
    pub position: Position,
    pub kind: TokenKind,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "TOKEN: kind: {:?}, pos: line: {}, column: {}, offset: {} \n",
            self.kind, self.position.line, self.position.column, self.offset
        )
    }
}

impl Token {
    pub fn new<K: Into<TokenKind>>(offset: u64, position: Position, kind: K) -> Token {
        let kind = kind.into();
        Token {
            offset,
            position,
            kind,
        }
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
        assert_eq!(pos.column, COLUMN + 1);
    }

    #[test]

    fn test_pos_newline() {
        let mut pos = create_pos();
        pos.new_line(2);
        assert_eq!(pos.offset, 2);
        assert_eq!(pos.line, LINE + 1);
        assert_eq!(pos.column, 0);
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Program {
    pub functions: HashMap<String, Function>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    pub name: String, //not optimized
    pub parameters: VecDeque<Parameter>,
    pub block: Block,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Parameter {
    pub name: String,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Argument {
    pub expr: Expression,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Block {
    pub statements: VecDeque<Statement>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Statement {
    Expression(Expression),
    If(If),
    For(For),
    While(While),
    Return(Return),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    OrExpression(OrExpression),
    AndExpression(AndExpression),
    EqualExpression(EqualExpression),
    RelationalExpression(RelationalExpression),
    AdditiveExpression(AdditiveExpression),
    MultiplicativeExpression(MultiplicativeExpression),
    UnaryExpression(NotExpression),
    HasExpression(HasExpression),
    StringLiteral(String),
    Number(Decimal),
    VariableExpression(VariableExpression),
    AssignmentExpression(AssignmentExpression),
}

#[derive(Clone, PartialEq, Debug)]
pub struct AssignmentExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub operator: AssignmentOperator,
    pub position: Position
}

#[derive(Clone, PartialEq, Debug)]
pub struct OrExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub position: Position
}

#[derive(Clone, PartialEq, Debug)]
pub struct AndExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub position: Position
}

#[derive(Clone, PartialEq, Debug)]
pub struct EqualExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub operator: EqualOperator,
    pub position: Position
}

#[derive(Clone, PartialEq, Debug)]
pub struct RelationalExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub operator: RelationOperator,
    pub position: Position
}

#[derive(Clone, PartialEq, Debug)]
pub struct MultiplicativeExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub operator: MultiplicationOperator,
    pub position: Position
}

#[derive(Clone, PartialEq, Debug)]
pub struct AdditiveExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub operator: AdditionOperator,
    pub position: Position
}

#[derive(Clone, PartialEq, Debug)]
pub struct NotExpression {
    pub expression: Box<Expression>,
    pub position: Position
}

#[derive(Clone, PartialEq, Debug)]
pub struct HasExpression {
    pub expression: Box<Expression>,
    pub ident: String,
    pub position: Position
}

#[derive(Clone, PartialEq, Debug)]
pub struct StringLiteral {
    pub content: String,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Number {
    pub number: Decimal,
}

#[derive(Clone, PartialEq, Debug)]
pub struct VariableExpression {
    pub path: VecDeque<FunCallOrMember>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunCallOrMember {
    pub name: String,
    pub arguments: Option<VecDeque<Argument>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct If {
    pub condition: Option<Box<Expression>>,
    pub block: Box<Block>,
    pub else_block: Option<Box<If>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct For {
    pub iterator: String,
    pub object: Box<Expression>,
    pub block: Box<Block>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct While {
    pub condition: Box<Expression>,
    pub block: Box<Block>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Return {
    pub expression: Option<Box<Expression>>,
}

pub const NOT_OPERATOR: TokenKind = TokenKind::Not;
pub const OR_OPERATOR: TokenKind = TokenKind::Or;
pub const AND_OPERATOR: TokenKind = TokenKind::And;
pub const SUBTRACT_OPERATOR: TokenKind = TokenKind::Subtraction;
pub const HAS_OPERATOR: TokenKind = TokenKind::Has;

#[derive(Clone, PartialEq, Debug)]
pub enum AssignmentOperator {
    Assignment,
    AddAssignment,
    SubtractAssignment,
    MultiplicationAssignment,
    DivisionAssignment,
    ModuloAssignment,
}

impl AssignmentOperator {
    pub fn remap(token: Token) -> Option<AssignmentOperator> {
        match token.kind {
            TokenKind::Assignment => Some(AssignmentOperator::Assignment),
            TokenKind::AddAssignment => Some(AssignmentOperator::AddAssignment),
            TokenKind::SubtractAssignment => Some(AssignmentOperator::SubtractAssignment),
            TokenKind::MultiplicationAssignment => {
                Some(AssignmentOperator::MultiplicationAssignment)
            }
            TokenKind::DivisionAssignment => Some(AssignmentOperator::DivisionAssignment),
            TokenKind::ModuloAssignment => Some(AssignmentOperator::ModuloAssignment),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum AdditionOperator {
    Add,
    Subtract,
}

impl AdditionOperator {
    pub fn remap(token: Token) -> Option<AdditionOperator> {
        match token.kind {
            TokenKind::Addition => Some(AdditionOperator::Add),
            TokenKind::Subtraction => Some(AdditionOperator::Subtract),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum MultiplicationOperator {
    Multiplication,
    Division,
    Modulo,
}

impl MultiplicationOperator {
    pub fn remap(token: Token) -> Option<MultiplicationOperator> {
        match token.kind {
            TokenKind::Multiplication => Some(MultiplicationOperator::Multiplication),
            TokenKind::Division => Some(MultiplicationOperator::Division),
            TokenKind::Modulo => Some(MultiplicationOperator::Modulo),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum RelationOperator {
    Grater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl RelationOperator {
    pub fn remap(token: Token) -> Option<RelationOperator> {
        match token.kind {
            TokenKind::GraterThen => Some(RelationOperator::Grater),
            TokenKind::LessThen => Some(RelationOperator::Less),
            TokenKind::GraterEqualThen => Some(RelationOperator::GreaterEqual),
            TokenKind::LessEqualThen => Some(RelationOperator::LessEqual),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum EqualOperator {
    Equal,
    NotEqual,
}

impl EqualOperator {
    pub fn remap(token: Token) -> Option<EqualOperator> {
        match token.kind {
            TokenKind::Equal => Some(EqualOperator::Equal),
            TokenKind::NotEqual => Some(EqualOperator::NotEqual),
            _ => None,
        }
    }
}
