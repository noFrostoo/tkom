use std::{collections::{VecDeque, HashMap}, rc::Rc};

use rust_decimal::Decimal;

use crate::{lexer::TLexer, types::{TokenKind, Token}, errors::{ErrorKind, ErrorHandler}};

const FAILFAST: bool = false;

#[derive(Clone, PartialEq, Debug)]
pub struct Program{
    functions: HashMap<String, Function>
}

impl Program {
    pub fn pretty_print(&self) {
        for func in &self.functions {
            func.1.pretty_print();
        }
    }
}



#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    name: Rc<String>, //not optimized
    parameters: VecDeque<Parameter>,
    block: Block
}

impl Function {
    pub fn pretty_print(&self) {
        println!("FUNCTION: {}", self.name);
        println!("Parameters: {}", self.name);
        for par in &self.parameters {
            par.pretty_print();
        }
        self.block.pretty_print();
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct  Parameter {
    name: String,
    pos: u8
}

impl Parameter {
    pub fn pretty_print(&self) {
        println!("* Name {}, pos: {}", self.name, self.pos);
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct  Argument {
    expr: Expression,
    pos: u8
}

#[derive(Clone, PartialEq, Debug)]
pub struct Block {
    statements: VecDeque<Statement>
}

impl Block {
    pub fn pretty_print(&self) {
        for stmt in &self.statements {
            
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Statement {
    Expression(Expression),
    If(If),
    For(For),
    While(While),
    Return(Return)
}

#[derive(Clone, PartialEq, Debug)]
pub enum  Expression {
    OrExpression(OrExpression),
    AndExpression(AndExpression),
    EqualExpression(EqualExpression),
    RelationalExpression(RelationalExpression),
    AdditiveExpression(AdditiveExpression),
    MultiplicativeExpression(MultiplicativeExpression),
    UnaryExpression(NotExpression),
    StringLiteral(String),
    Number(Decimal), 
    VariableExpression(VariableExpression),
    AssignmentExpression(AssignmentExpression),
}

#[derive(Clone, PartialEq, Debug)]
pub struct AssignmentExpression {
    left: Box<Expression>,
    right: Box<Expression>,
    operator: AssignmentOperator 
}

#[derive(Clone, PartialEq, Debug)]
pub struct OrExpression {
    left: Box<Expression>,
    right: Box<Expression> 
}

#[derive(Clone, PartialEq, Debug)]
pub struct AndExpression {
    left: Box<Expression>,
    right: Box<Expression> 
}

#[derive(Clone, PartialEq, Debug)]
pub struct EqualExpression {
    left: Box<Expression>,
    right: Box<Expression>, 
    operator: EqualOperator
}

#[derive(Clone, PartialEq, Debug)]
pub struct RelationalExpression {
    left: Box<Expression>,
    right: Box<Expression>, 
    operator: RelationOperator
}

#[derive(Clone, PartialEq, Debug)]
pub struct  MultiplicativeExpression {
    left: Box<Expression>,
    right: Box<Expression>, 
    operator: MultiplicationOperator
}

#[derive(Clone, PartialEq, Debug)]
pub struct AdditiveExpression {
    left: Box<Expression>,
    right: Box<Expression>, 
    operator: AdditionOperator
}

#[derive(Clone, PartialEq, Debug)]
pub struct NotExpression {
    expression: Box<Expression>, 
}

#[derive(Clone, PartialEq, Debug)]
pub struct  StringLiteral {
    content: String
}

#[derive(Clone, PartialEq, Debug)]
pub struct  Number {
    number: Decimal
}   

#[derive(Clone, PartialEq, Debug)]
pub struct VariableExpression {
    path: VecDeque<String>,
    has: Option<String>,
    arguments: Option<VecDeque<Argument>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct  If {
    condition: Option<Box<Expression>>,
    block:Box<Block>,
    else_block: Option<Box<If>>
}

#[derive(Clone, PartialEq, Debug)]
pub struct For {
    iterator: String, 
    object: Box<Expression>,
    block: Box<Block>
}

#[derive(Clone, PartialEq, Debug)]
pub struct  While {
    condition: Box<Expression>,
    block: Box<Block>
}

#[derive(Clone, PartialEq, Debug)]
pub struct  Return {
    expression: Option<Box<Expression>>
}

pub const NOT_OPERATOR: TokenKind = TokenKind::Not;
pub const OR_OPERATOR: TokenKind = TokenKind::Or;
pub const AND_OPERATOR: TokenKind = TokenKind::And;

#[derive(Clone, PartialEq, Debug)]
pub enum AssignmentOperator {
    Assignment,
    AddAssignment,
    SubtractAssignment,
    MultiplicationAssignment,
    DivisionAssignment,
    ModuloAssignment
}

impl AssignmentOperator {
    fn remap(token: Token) -> Option<AssignmentOperator> {
        match token.kind {
            TokenKind::Assignment => Some(AssignmentOperator::Assignment),
            TokenKind::AddAssignment => Some(AssignmentOperator::AddAssignment),
            TokenKind::SubtractAssignment => Some(AssignmentOperator::SubtractAssignment),
            TokenKind::MultiplicationAssignment => Some(AssignmentOperator::MultiplicationAssignment),
            TokenKind::DivisionAssignment => Some(AssignmentOperator::DivisionAssignment),
            TokenKind::ModuloAssignment => Some(AssignmentOperator::ModuloAssignment),
            _ => None
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum AdditionOperator {
    Add,
    Subtract
}

impl AdditionOperator {
    fn remap(token: Token) -> Option<AdditionOperator> {
        match token.kind {
            TokenKind::Addition => Some(AdditionOperator::Add),
            TokenKind::Subtraction => Some(AdditionOperator::Subtract),
            _ => None
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum MultiplicationOperator {
    Multiplication,
    Division,
    Modulo
}

impl MultiplicationOperator {
    fn remap(token: Token) -> Option<MultiplicationOperator> {
        match token.kind {
            TokenKind::Multiplication => Some(MultiplicationOperator::Multiplication),
            TokenKind::Division => Some(MultiplicationOperator::Division),
            TokenKind::Modulo => Some(MultiplicationOperator::Modulo),
            _ => None
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum RelationOperator {
    Grater,
    GreaterEqual,
    Less,
    LessEqual
}

impl RelationOperator {
    fn remap(token: Token) -> Option<RelationOperator> {
        match token.kind {
            TokenKind::GraterThen => Some(RelationOperator::Grater),
            TokenKind::LessThen => Some(RelationOperator::Less),
            TokenKind::GraterEqualThen => Some(RelationOperator::GreaterEqual),
            TokenKind::LessEqualThen => Some(RelationOperator::LessEqual),
            _ => None
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum EqualOperator {
    Equal,
    NotEqual
}

impl EqualOperator {
    fn remap(token: Token) -> Option<EqualOperator> {
        match token.kind {
            TokenKind::Equal => Some(EqualOperator::Equal),
            TokenKind::NotEqual => Some(EqualOperator::NotEqual),
            _ => None
        }
    }
}

pub struct Parser {
    lexer: Box<dyn TLexer>
}

impl Parser { 
    pub fn new(lexer: Box<dyn TLexer>) -> Parser {
        Parser{
            lexer: lexer,
        }
    }

    pub fn parse(&mut self) -> Program {
        self.next_token();
        let mut functions: HashMap<String, Function> = HashMap::new();
        while let Some(func) = self.try_parse_fun_def() {
            functions.insert(String::from(func.name.as_ref()), func.clone());
        }

        Program{functions: functions}
    }

    fn try_parse_fun_def(&mut self) -> Option<Function> {
        let name: String;
        match self.lexer.get_current_token().kind {
            TokenKind::Identifier(n) => name = n,
            _ => return None
        }

        self.next_token();

        if let TokenKind::LeftParentheses = self.lexer.get_current_token().kind { self.next_token(); } else {
            self.report_error(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::LeftParentheses, got: self.lexer.get_current_token().kind }) ;
        }

        let parameters = self.parse_parameters();

        if let TokenKind::RightParentheses = self.lexer.get_current_token().kind { self.next_token(); } else {
            self.report_error(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::RightParentheses, got: self.lexer.get_current_token().kind }) ;
        }

        let block = self.parse_block();

        Some(Function{name: Rc::new(name), parameters: parameters, block: block})
    }

    fn parse_parameters(&mut self) -> VecDeque<Parameter> {
        let mut parameters:VecDeque<Parameter> = VecDeque::new();
        let mut pos: u8 = 0;

        match self.lexer.get_current_token().kind {
            TokenKind::Identifier(p) => parameters.push_back(Parameter{name: p, pos: pos}),
            _ => return parameters
        };

        pos += 1;
        self.next_token();

        while let TokenKind::Comma = self.lexer.get_current_token().kind {
            self.next_token();

            match self.lexer.get_current_token().kind {
                TokenKind::Identifier(p) => parameters.push_back(Parameter{name: p, pos: pos}),
                _ => return parameters
            };

            pos += 1;

            self.next_token();
        }

        parameters
    }

    fn parse_block(&mut self) -> Block {
        if let TokenKind::LeftBracket = self.lexer.get_current_token().kind { self.next_token(); } else { 
            self.report_error(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::LeftBracket, got: self.lexer.get_current_token().kind }) ;
        }

        let mut statements:VecDeque<Statement> = VecDeque::new();

        while let Some(stmt) = self.try_parse_statement() {
            statements.push_back(stmt);
        }

        if let TokenKind::RightBracket = self.lexer.get_current_token().kind { self.next_token(); } else {
            self.report_error(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::RightBracket, got: self.lexer.get_current_token().kind }) ;
        }

        Block { statements: statements }
    }

    fn try_parse_statement(&mut self) -> Option<Statement> {
        if let Some(expr) = self.try_parse_expression() {

            if let TokenKind::Semicolon = self.lexer.get_current_token().kind { self.next_token(); } else {
                self.report_error(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::Semicolon, got: self.lexer.get_current_token().kind }) ;
            }

            return Some(Statement::Expression(expr));
        }

        if let Some(wh) = self.try_prase_while() {
            return Some(Statement::While(wh))
        }

        if let Some(fr) = self.try_prase_for() {
            return Some(Statement::For(fr));
        }

        if let Some(stmt) = self.try_prase_if() {
            return Some(Statement::If(stmt));
        }

        if let Some(ret) = self.try_parse_return() {

            if let TokenKind::Semicolon = self.lexer.get_current_token().kind { self.next_token(); } else {
                self.report_error(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::Semicolon, got: self.lexer.get_current_token().kind }) ;
            }

            return Some(Statement::Return(ret));
        }

        None
    }

    fn try_prase_while(&mut self) -> Option<While> {
        if let TokenKind::While = self.lexer.get_current_token().kind { self.next_token(); } else { 
            return None;
        }

        if let TokenKind::LeftParentheses = self.lexer.get_current_token().kind { self.next_token(); } else {
            self.report_error(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::LeftParentheses, got: self.lexer.get_current_token().kind }) ;
        }

        let cond = self.try_parse_or_expression()?;

        if let TokenKind::RightParentheses = self.lexer.get_current_token().kind { self.next_token(); } else {
            self.report_error(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::RightParentheses, got: self.lexer.get_current_token().kind }) ;
        }

        let block = self.parse_block();

        Some(While { condition: Box::new(cond), block: Box::new(block) })
    }

    fn try_prase_for(&mut self) -> Option<For> {
        if let TokenKind::For = self.lexer.get_current_token().kind { self.next_token(); } else { 
            return None;
        }

        let ident_iterator: String;

        if let TokenKind::Identifier(ident) = self.lexer.get_current_token().kind {
            ident_iterator = ident;
            self.next_token();
        } else {
            ErrorHandler::fatal(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::Identifier("_".to_string()), got: self.lexer.get_current_token().kind }) ;
        }

        if let TokenKind::In = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            self.report_error(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::Identifier("_".to_string()), got: self.lexer.get_current_token().kind }) ;
        }

        let object: Expression;
        match self.try_parse_expression() {
            Some(expr) => object = expr,
            None => ErrorHandler::fatal(ErrorKind::ObjectExpected{ position: self.lexer.get_current_token().position.clone(), got: self.lexer.get_current_token().kind }),
        }

        let block = self.parse_block();

        Some(For { iterator: ident_iterator, object: Box::new(object), block: Box::new(block) })
    }

    fn try_prase_if(&mut self) -> Option<If> {
        if let TokenKind::If = self.lexer.get_current_token().kind { self.next_token(); } else { 
            return None;
        }

        if let TokenKind::LeftParentheses = self.lexer.get_current_token().kind { self.next_token(); } else {
            self.report_error(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::LeftParentheses, got: self.lexer.get_current_token().kind }) ;
        }

        let cond = self.try_parse_or_expression()?;

        if let TokenKind::RightParentheses = self.lexer.get_current_token().kind { self.next_token(); } else {
            self.report_error(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::RightParentheses, got: self.lexer.get_current_token().kind }) ;
        }

        let block = self.parse_block();

        let else_block: Option<Box<If>>;
        if let TokenKind::Else = self.lexer.get_current_token().kind {
            self.next_token();
            match self.try_prase_if() {
                Some(stmt) => else_block = Some(Box::new(stmt)),
                None => {
                    else_block = Some(Box::new(If{ condition: None, block: Box::new(self.parse_block()), else_block: None }))
                },
            }
        } else { 
            else_block = None;
        }

        Some(If{ condition: Some(Box::new(cond)), block: Box::new(block), else_block: else_block })
    }

    fn try_parse_return(&mut self) -> Option<Return> {
        if let TokenKind::Return = self.lexer.get_current_token().kind { self.next_token(); } else { 
            return None;
        }

        match self.try_parse_expression() {
            Some(expr) => Some(Return { expression: Some(Box::new(expr)) }),
            None => Some(Return { expression: None }),
        }
    }

    fn try_parse_expression(&mut self) -> Option<Expression> {
        let mut left = self.try_parse_or_expression()?;

        if let Some(operator) = AssignmentOperator::remap(self.lexer.get_current_token()) {
            self.next_token();
            match self.try_parse_or_expression() {
                Some(right) => {left = Expression::AssignmentExpression(AssignmentExpression{ left: Box::new(left), right: Box::new(right), operator: operator })},
                None => todo!(),
            }
        } 

        Some(left)
    }

    fn try_parse_or_expression(&mut self) -> Option<Expression> {
        let mut left = self.try_parse_and_expression()?;
        
        let operator = self.lexer.get_current_token().kind;
        if matches!(operator, OR_OPERATOR) {
            self.next_token();
            match self.try_parse_and_expression() {
                Some(right) => {left = Expression::OrExpression(OrExpression{ left: Box::new(left), right: Box::new(right) })},
                None => todo!(),
            }
        } 

        Some(left)
    }

    fn try_parse_and_expression(&mut self) -> Option<Expression> {
        let mut left = self.try_parse_equal_expression()?;
        
        let operator = self.lexer.get_current_token().kind;
        if matches!(operator, AND_OPERATOR) {
            self.next_token();
            match self.try_parse_equal_expression() {
                Some(right) => {left = Expression::AndExpression(AndExpression{ left: Box::new(left), right: Box::new(right) })},
                None => todo!(),
            }
        } 

        Some(left)
    }

    fn try_parse_equal_expression(&mut self) ->  Option<Expression> {
        let mut left = self.try_parse_relational_expression()?;
        
        if let Some(operator) = EqualOperator::remap(self.lexer.get_current_token()) {
            self.next_token();
            match self.try_parse_relational_expression() {
                Some(right) => {left = Expression::EqualExpression(EqualExpression{ left: Box::new(left), right: Box::new(right), operator: operator })},
                None => todo!(),
            }
        } 

        Some(left)
    }

    fn try_parse_relational_expression(&mut self) ->  Option<Expression> {
        let mut left = self.try_parse_additive_expression()?;
        
        if let Some(operator) = RelationOperator::remap(self.lexer.get_current_token()) {
            self.next_token();
            match self.try_parse_additive_expression() {
                Some(right) => {left = Expression::RelationalExpression(RelationalExpression{ left: Box::new(left), right: Box::new(right), operator: operator })},
                None => todo!(),
            }
        } 

        Some(left)
    }

    fn try_parse_additive_expression(&mut self) -> Option<Expression> {
        let mut left = self.try_parse_multiplicative_expression()?;
        
        if let Some(operator) = AdditionOperator::remap(self.lexer.get_current_token()) {
            self.next_token();
            match self.try_parse_multiplicative_expression() {
                Some(right) => {left = Expression::AdditiveExpression(AdditiveExpression{ left: Box::new(left), right: Box::new(right), operator: operator })},
                None => todo!(),
            }
        } 

        Some(left)
    }

    fn try_parse_multiplicative_expression(&mut self) -> Option<Expression> {
        let mut left = self.try_parse_unary_expression()?;
        
        if let Some(operator) = MultiplicationOperator::remap(self.lexer.get_current_token()) {
            self.next_token();
            match self.try_parse_multiplicative_expression() {
                Some(right) => {left = Expression::MultiplicativeExpression(MultiplicativeExpression{ left: Box::new(left), right: Box::new(right), operator: operator })},
                None => todo!(),
            }
        } 

        Some(left)
    }

    fn try_parse_unary_expression(&mut self) -> Option<Expression> {
        if matches!(self.lexer.get_current_token().kind, NOT_OPERATOR) {
            let expression = self.try_parse_primary_expression()?;
            return Some(Expression::UnaryExpression(NotExpression{expression: Box::new(expression)}))
        }

        let expression = self.try_parse_primary_expression()?;
        Some(expression)
    }

    fn try_parse_primary_expression(&mut self) -> Option<Expression> {
        match self.lexer.get_current_token().kind {
            TokenKind::QuotedString(s) => { self.next_token(); Some(Expression::StringLiteral(s)) },
            TokenKind::Number(n) => { self.next_token(); Some(Expression::Number(n))},
            TokenKind::Identifier(_) => self.try_parse_variable_expression(),
            _ => None
        }
    }

    fn try_parse_variable_expression(&mut self) -> Option<Expression> {
        match self.lexer.get_current_token().kind {
            TokenKind::Identifier(_) => self.try_parse_ident_expression(),
            TokenKind::LeftParentheses => {
                self.next_token();
                let expr = self.try_parse_expression()?;
                if !matches!(self.lexer.get_current_token().kind, TokenKind::RightParentheses) {
                    self.report_error(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::RightParentheses, got: self.lexer.get_current_token().kind }) ;
                }
                return Some(expr);
            }
            _ => None
        }
    }

    fn try_parse_ident_expression(&mut self) -> Option<Expression> {
        let mut ident;
        if let TokenKind::Identifier(i) = self.lexer.get_current_token().kind { ident = i } else {
            return None
        }

        self.next_token();
        
        let mut path: VecDeque<String> = VecDeque::new();
        let mut arguments: Option<VecDeque<Argument>> = None;
        let mut has: Option<String>  = None;

        path.push_back(ident.clone());

        while let TokenKind::Dot = self.lexer.get_current_token().kind {
            self.next_token();
            if let TokenKind::Identifier(i) = self.lexer.get_current_token().kind { ident = i; } else {
                return None
            }
            self.next_token();
            path.push_back(ident);
        }

        if let TokenKind::LeftParentheses = self.lexer.get_current_token().kind {
            self.next_token();
            
            arguments = Some(self.parse_arguments());

            if let TokenKind::RightParentheses = self.lexer.get_current_token().kind { self.next_token(); } else {
                self.report_error(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::LeftParentheses, got: self.lexer.get_current_token().kind }) ;
            }
        }

        if let TokenKind::Has = self.lexer.get_current_token().kind {
            self.next_token();
            if let TokenKind::Identifier(i) = self.lexer.get_current_token().kind {
                has = Some(i);
            } else {
                ErrorHandler::fatal(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::LeftParentheses, got: self.lexer.get_current_token().kind })
            }
        }

        return Some(Expression::VariableExpression(VariableExpression { path: path, has: has, arguments: arguments }));
    }

    fn parse_arguments(&mut self) -> VecDeque<Argument> {
        let mut arguments: VecDeque<Argument> = VecDeque::new();
        let mut pos = 0;

        match self.try_parse_expression() {
            Some(e) => { arguments.push_back(Argument{ expr: e, pos: pos })} ,
            None => ErrorHandler::fatal(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::LeftParentheses, got: self.lexer.get_current_token().kind }),
        }

        while let TokenKind::Comma = self.lexer.get_current_token().kind { 
            self.next_token();
            match self.try_parse_expression() {
                Some(e) => { arguments.push_back(Argument{ expr: e, pos: pos })} ,
                None => ErrorHandler::fatal(ErrorKind::SyntaxError{ position: self.lexer.get_current_token().position.clone(), expected_kind: TokenKind::LeftParentheses, got: self.lexer.get_current_token().kind }),
            }
            pos += 1;
        }

        arguments
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.lexer.get_next_token() {
            Some(res) => {
                match res {
                    Ok(token) => Some(token),
                    Err(e) => todo!()
                }
            },
            None => None,
        }
    }
    
    fn report_error(&self, err: ErrorKind) {
        if FAILFAST {
            panic!("aaa")
        } else {
            ErrorHandler::report_error(err)
        }
    }
}

mod tests {


    macro_rules! parser_test {
        (FAIL: $name:ident, $text:expr) => {
            #[test]
            fn $name() {
                let text: &str = $text;
                let should_be: Program = $program;

                let test_source = file_handler::TestSource::new(String::from(text), 0);
                let mut lexer = Lexer::new(Box::new(test_source));
                let mut parser = Parser::new(Box::new(lexer));
                let program = parser.parse();
                assert_eq!(program, should_be)
            }
        };
        ($name:ident, $text:expr, $program:expr) => {
            #[test]
            fn $name() {
                let text: &str = $text;
                let should_be: Program = $program;

                let test_source = file_handler::TestSource::new(String::from(text), 0);
                let lexer = Lexer::new(Box::new(test_source));
                let mut parser = Parser::new(Box::new(lexer));
                let program = parser.parse();
                assert_eq!(program, should_be)
            }
        };
    }

    use crate::{
        file_handler::{self, FileSource},
        types::TokenKind,
        lexer::{Lexer, TLexer},
    };

    use super::*;

    parser_test!(basic_test, "main(args, argv) { }", Program{
        functions: HashMap::from([
            ("main".to_string(),  Function{name: Rc::new(String::from("main")), parameters: VecDeque::from_iter([Parameter{name: "args".to_string(), pos: 0}, Parameter{name: "argv".to_string(), pos: 1}]), block: Block { statements: VecDeque::new() }}),
            ])
    });

    parser_test!(number_and_string_parsing, "main(args) { xx + 12; xx - \"12\"; }", Program{
        functions: HashMap::from([
            ("main".to_string(),  
            Function{name: Rc::new(String::from("main")), 
            parameters: VecDeque::from_iter([
                Parameter{name: "args".to_string(), pos: 0}]), 
                block: Block { 
                    statements: VecDeque::from_iter([
                        Statement::Expression(Expression::AdditiveExpression(AdditiveExpression{
                            left: Box::new(Expression::VariableExpression(VariableExpression{
                                path: VecDeque::from_iter(["xx".to_string()]),
                                has: None,
                                arguments: None,
                            })),
                            right: Box::new(Expression::Number(Decimal::from(12))),
                            operator: AdditionOperator::Add
                        })
                    ), Statement::Expression(Expression::AdditiveExpression(AdditiveExpression{
                        left: Box::new(Expression::VariableExpression(VariableExpression{
                            path: VecDeque::from_iter(["xx".to_string()]),
                            has: None,
                            arguments: None,
                        })),
                        right: Box::new(Expression::StringLiteral("12".to_string())),
                        operator: AdditionOperator::Subtract
                    })
                    )        
            ]) }}),
            ])
    });

    parser_test!(returnca_parsing, "main(args) { return ; return xx - \"12\"; }", Program{
        functions: HashMap::from([
            ("main".to_string(),  
            Function{name: Rc::new(String::from("main")), 
            parameters: VecDeque::from_iter([
                Parameter{name: "args".to_string(), pos: 0}]), 
                block: Block { 
                    statements: VecDeque::from_iter([
                        Statement::Return(Return{
                            expression: None
                        }
                    ), Statement::Return(Return{
                        expression: Some(Box::new(Expression::AdditiveExpression(AdditiveExpression{
                        left: Box::new(Expression::VariableExpression(VariableExpression{
                            path: VecDeque::from_iter(["xx".to_string()]),
                            has: None,
                            arguments: None,
                        })),
                        right: Box::new(Expression::StringLiteral("12".to_string())),
                        operator: AdditionOperator::Subtract
                    })) )}
                    )        
            ]) }}),
            ])
    });

    parser_test!(multiple_expressions, "main(args) { xx + 12; xx - 12; xx * 12; xx / 12; xx or 12; }", Program{
        functions: HashMap::from([
            ("main".to_string(),  
            Function{name: Rc::new(String::from("main")), 
            parameters: VecDeque::from_iter([
                Parameter{name: "args".to_string(), pos: 0}]), 
                block: Block { 
                    statements: VecDeque::from_iter([
                        Statement::Expression(Expression::AdditiveExpression(AdditiveExpression{
                            left: Box::new(Expression::VariableExpression(VariableExpression{
                                path: VecDeque::from_iter(["xx".to_string()]),
                                has: None,
                                arguments: None,
                            })),
                            right: Box::new(Expression::Number(Decimal::from(12))),
                            operator: AdditionOperator::Add
                        })
                    ), Statement::Expression(Expression::AdditiveExpression(AdditiveExpression{
                        left: Box::new(Expression::VariableExpression(VariableExpression{
                            path: VecDeque::from_iter(["xx".to_string()]),
                            has: None,
                            arguments: None,
                        })),
                        right: Box::new(Expression::Number(Decimal::from(12))),
                        operator: AdditionOperator::Subtract
                    })
                    ), Statement::Expression(Expression::MultiplicativeExpression(MultiplicativeExpression{
                        left: Box::new(Expression::VariableExpression(VariableExpression{
                            path: VecDeque::from_iter(["xx".to_string()]),
                            has: None,
                            arguments: None,
                        })),
                        right: Box::new(Expression::Number(Decimal::from(12))),
                        operator: MultiplicationOperator::Multiplication
                    })
                    ), Statement::Expression(Expression::MultiplicativeExpression(MultiplicativeExpression{
                        left: Box::new(Expression::VariableExpression(VariableExpression{
                            path: VecDeque::from_iter(["xx".to_string()]),
                            has: None,
                            arguments: None,
                        })),
                        right: Box::new(Expression::Number(Decimal::from(12))),
                        operator: MultiplicationOperator::Division
                    })
                    ), Statement::Expression(Expression::OrExpression(OrExpression{
                        left: Box::new(Expression::VariableExpression(VariableExpression{
                            path: VecDeque::from_iter(["xx".to_string()]),
                            has: None,
                            arguments: None,
                        })),
                        right: Box::new(Expression::Number(Decimal::from(12))),
                    })
                    )
            
            ]) }}),
            ])
    });

    parser_test!(parsing_arguments, "main(args) { xx = 12; xx.x(ee); xx.x.ee(add(y.yy.yyy)); }", Program{
        functions: HashMap::from([
            ("main".to_string(),  
            Function{name: Rc::new(String::from("main")), 
            parameters: VecDeque::from_iter([
                Parameter{name: "args".to_string(), pos: 0}]), 
                block: Block { 
                    statements: VecDeque::from_iter([
                        Statement::Expression(Expression::AssignmentExpression(AssignmentExpression{
                            left: Box::new(Expression::VariableExpression(VariableExpression{
                                path: VecDeque::from_iter(["xx".to_string()]),
                                has: None,
                                arguments: None,                         })),
                            right: Box::new(Expression::Number(Decimal::from(12))),
                            operator: AssignmentOperator::Assignment
                        })
                    ), Statement::Expression(Expression::VariableExpression(VariableExpression{
                        path: VecDeque::from_iter(["xx".to_string(), "x".to_string()]),
                        has: None, 
                        arguments: Some(VecDeque::from_iter([
                            Argument{
                                expr: Expression::VariableExpression(VariableExpression{ path: VecDeque::from_iter(["ee".to_string()]), has: None, arguments: None }), 
                                pos: 0 }
                        ])) })
                    ), Statement::Expression(Expression::VariableExpression(VariableExpression{
                        path: VecDeque::from_iter(["xx".to_string(), "x".to_string(), "ee".to_string()]),
                        has: None, 
                        arguments: Some(VecDeque::from_iter([
                            Argument{
                                expr: Expression::VariableExpression(VariableExpression{ path: VecDeque::from_iter(["add".to_string()]), has: None, arguments: Some(VecDeque::from_iter([
                                    Argument{
                                        expr: Expression::VariableExpression(VariableExpression{ path: VecDeque::from_iter(["y".to_string(), "yy".to_string(), "yyy".to_string()]), has: None, arguments: None }), 
                                        pos: 0 }
                                ])) }), 
                                pos: 0 }
                        ])) })
            )
            ]) }}),
            ])
    });

    parser_test!(while_parsing, "main(args) { while (xx != 1) { xx + 12; } }", Program{
        functions: HashMap::from([
            ("main".to_string(),  
            Function{name: Rc::new(String::from("main")), 
            parameters: VecDeque::from_iter([
                Parameter{name: "args".to_string(), pos: 0}]), 
                block: Block { statements: VecDeque::from_iter([
                    Statement::While(While{ 
                        condition: Box::new(Expression::EqualExpression(EqualExpression{ 
                            left: Box::new(Expression::VariableExpression(VariableExpression{
                                        path: VecDeque::from_iter(["xx".to_string()]),
                                        has: None,
                                        arguments: None,
                                    })), 
                            right: Box::new(Expression::Number(Decimal::from(1))),
                            operator: EqualOperator::NotEqual 
                        })), block: Box::new(Block { 
                            statements: VecDeque::from_iter([
                                Statement::Expression(Expression::AdditiveExpression(AdditiveExpression{
                                    left: Box::new(Expression::VariableExpression(VariableExpression{
                                        path: VecDeque::from_iter(["xx".to_string()]),
                                        has: None,
                                        arguments: None,
                                    })),
                                    right: Box::new(Expression::Number(Decimal::from(12))),
                                    operator: AdditionOperator::Add
                                })
                            )        
                        ]) })
                    })
                ]) }
                 }),
            ])
    });

    parser_test!(for_parsing, "main(args) { for xx in eee { xx + 12; } }", Program{
        functions: HashMap::from([
            ("main".to_string(),  
            Function{name: Rc::new(String::from("main")), 
            parameters: VecDeque::from_iter([
                Parameter{name: "args".to_string(), pos: 0}]), 
                block: Block { statements: VecDeque::from_iter([
                    Statement::For(For{ 
                            object: Box::new(Expression::VariableExpression(VariableExpression{
                                        path: VecDeque::from_iter(["eee".to_string()]),
                                        has: None,
                                        arguments: None,
                                    })),
                            iterator: "xx".to_string(),
                            block: Box::new(Block { 
                            statements: VecDeque::from_iter([
                                Statement::Expression(Expression::AdditiveExpression(AdditiveExpression{
                                    left: Box::new(Expression::VariableExpression(VariableExpression{
                                        path: VecDeque::from_iter(["xx".to_string()]),
                                        has: None,
                                        arguments: None,
                                    })),
                                    right: Box::new(Expression::Number(Decimal::from(12))),
                                    operator: AdditionOperator::Add
                                })
                            )       
                        ]) })
                    })
                ]) }
                 }),
            ])
    });

    parser_test!(if_parsing, "main(args) { if (xx != 1) { xx + 12; } }", Program{
        functions: HashMap::from([
            ("main".to_string(),  
            Function{name: Rc::new(String::from("main")), 
            parameters: VecDeque::from_iter([
                Parameter{name: "args".to_string(), pos: 0}]), 
                block: Block { statements: VecDeque::from_iter([
                    Statement::If(If{ 
                        condition: Some(Box::new(Expression::EqualExpression(EqualExpression{ 
                            left: Box::new(Expression::VariableExpression(VariableExpression{
                                        path: VecDeque::from_iter(["xx".to_string()]),
                                        has: None,
                                        arguments: None,
                                    })), 
                            right: Box::new(Expression::Number(Decimal::from(1))),
                            operator: EqualOperator::NotEqual 
                        }))),
                        else_block: None, 
                        block: Box::new(Block { 
                            statements: VecDeque::from_iter([
                                Statement::Expression(Expression::AdditiveExpression(AdditiveExpression{
                                    left: Box::new(Expression::VariableExpression(VariableExpression{
                                        path: VecDeque::from_iter(["xx".to_string()]),
                                        has: None,
                                        arguments: None,
                                    })),
                                    right: Box::new(Expression::Number(Decimal::from(12))),
                                    operator: AdditionOperator::Add
                                })
                            )      
                        ]) })
                    })
                ]) }
                 }),
            ])
    });

    parser_test!(if_else_parsing, "main(args) {
         if (xx != 1) {
              xx + 12; 
        } else { xx + 12; } 
       }", Program{
        functions: HashMap::from([
            ("main".to_string(),  
            Function{name: Rc::new(String::from("main")), 
            parameters: VecDeque::from_iter([
                Parameter{name: "args".to_string(), pos: 0}]), 
                block: Block { statements: VecDeque::from_iter([
                    Statement::If(If{ 
                        condition: Some(Box::new(Expression::EqualExpression(EqualExpression{ 
                            left: Box::new(Expression::VariableExpression(VariableExpression{
                                        path: VecDeque::from_iter(["xx".to_string()]),
                                        has: None,
                                        arguments: None,
                                    })), 
                            right: Box::new(Expression::Number(Decimal::from(1))),
                            operator: EqualOperator::NotEqual 
                        }))),
                        else_block: Some(Box::new(If{ 
                            condition: None,
                            else_block: None, 
                            block: Box::new(Block { 
                                statements: VecDeque::from_iter([
                                    Statement::Expression(Expression::AdditiveExpression(AdditiveExpression{
                                        left: Box::new(Expression::VariableExpression(VariableExpression{
                                            path: VecDeque::from_iter(["xx".to_string()]),
                                            has: None,
                                            arguments: None,
                                        })),
                                        right: Box::new(Expression::Number(Decimal::from(12))),
                                        operator: AdditionOperator::Add
                                    })
                                )      
                            ]) })
                        })), 
                        block: Box::new(Block { 
                            statements: VecDeque::from_iter([
                                Statement::Expression(Expression::AdditiveExpression(AdditiveExpression{
                                    left: Box::new(Expression::VariableExpression(VariableExpression{
                                        path: VecDeque::from_iter(["xx".to_string()]),
                                        has: None,
                                        arguments: None,
                                    })),
                                    right: Box::new(Expression::Number(Decimal::from(12))),
                                    operator: AdditionOperator::Add
                                })
                            )      
                        ]) })
                    })
                ]) }
                 }),
            ])
    });
    
    parser_test!(if_else_if_else_parsing, "main(args) { if (xx != 1) { xx + 12; } else if (xx == 1) { xx + 13; } else { xx + 11; }  }", Program{
        functions: HashMap::from([
            ("main".to_string(),  
            Function{name: Rc::new(String::from("main")), 
            parameters: VecDeque::from_iter([
                Parameter{name: "args".to_string(), pos: 0}]), 
                block: Block { statements: VecDeque::from_iter([
                    Statement::If(If{ 
                        condition: Some(Box::new(Expression::EqualExpression(EqualExpression{ 
                            left: Box::new(Expression::VariableExpression(VariableExpression{
                                        path: VecDeque::from_iter(["xx".to_string()]),
                                        has: None,
                                        arguments: None,
                                    })), 
                            right: Box::new(Expression::Number(Decimal::from(1))),
                            operator: EqualOperator::NotEqual 
                        }))),
                        else_block: Some(Box::new(If{ 
                            condition: Some(Box::new(Expression::EqualExpression(EqualExpression{ 
                                left: Box::new(Expression::VariableExpression(VariableExpression{
                                            path: VecDeque::from_iter(["xx".to_string()]),
                                            has: None,
                                            arguments: None,
                                        })), 
                                right: Box::new(Expression::Number(Decimal::from(1))),
                                operator: EqualOperator::Equal 
                            }))),
                            else_block: Some(Box::new(If{ 
                                condition: None,
                                else_block: None, 
                                block: Box::new(Block { 
                                    statements: VecDeque::from_iter([
                                        Statement::Expression(Expression::AdditiveExpression(AdditiveExpression{
                                            left: Box::new(Expression::VariableExpression(VariableExpression{
                                                path: VecDeque::from_iter(["xx".to_string()]),
                                                has: None,
                                                arguments: None,
                                            })),
                                            right: Box::new(Expression::Number(Decimal::from(11))),
                                            operator: AdditionOperator::Add
                                        })
                                    )      
                                ]) })
                            })), 
                            block: Box::new(Block { 
                                statements: VecDeque::from_iter([
                                    Statement::Expression(Expression::AdditiveExpression(AdditiveExpression{
                                        left: Box::new(Expression::VariableExpression(VariableExpression{
                                            path: VecDeque::from_iter(["xx".to_string()]),
                                            has: None,
                                            arguments: None,
                                        })),
                                        right: Box::new(Expression::Number(Decimal::from(13))),
                                        operator: AdditionOperator::Add
                                    })
                                )      
                            ]) })
                        })), 
                        block: Box::new(Block { 
                            statements: VecDeque::from_iter([
                                Statement::Expression(Expression::AdditiveExpression(AdditiveExpression{
                                    left: Box::new(Expression::VariableExpression(VariableExpression{
                                        path: VecDeque::from_iter(["xx".to_string()]),
                                        has: None,
                                        arguments: None,
                                    })),
                                    right: Box::new(Expression::Number(Decimal::from(12))),
                                    operator: AdditionOperator::Add
                                })
                            )      
                        ]) })
                    })
                ]) }
                 }),
            ])
    });
}
