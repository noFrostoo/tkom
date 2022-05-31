use std::{
    collections::{HashMap, VecDeque},
    rc::Rc,
};

use rust_decimal::Decimal;

use crate::{
    errors::{ErrorHandler, ErrorKind},
    lexer::TLexer,
    types::*,
};


pub struct Parser {
    lexer: Box<dyn TLexer>,
    fail_fast: bool,
}

impl Parser {
    pub fn new(lexer: Box<dyn TLexer>, fail_fast: bool) -> Parser {
        Parser {
            lexer: lexer,
            fail_fast: fail_fast,
        }
    }

    pub fn parse(&mut self) -> Program {
        self.next_token();
        let mut functions: HashMap<String, Function> = HashMap::new();
        while let Some(func) = self.try_parse_fun_def() {
            match functions.get(&func.name.to_string()) {
                Some(_) => ErrorHandler::fatal(ErrorKind::DuplicateFunction {
                    name: func.name.to_string(),
                }),
                None => {
                    functions.insert(String::from(func.name.as_ref()), func.clone());
                }
            }
        }

        if functions.len() == 0 {
            self.report_error(ErrorKind::NoFunctions);
        }

        Program {
            functions: functions,
        }
    }

    fn try_parse_fun_def(&mut self) -> Option<Function> {
        let name: String;
        match self.lexer.get_current_token().kind {
            TokenKind::Identifier(n) => name = n,
            _ => return None,
        }

        self.next_token();

        if let TokenKind::LeftParentheses = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            self.report_error(ErrorKind::SyntaxError {
                position: self.lexer.get_current_token().position.clone(),
                expected_kind: TokenKind::LeftParentheses,
                got: self.lexer.get_current_token().kind,
            });
        }

        let parameters = self.parse_parameters();

        if let TokenKind::RightParentheses = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            self.report_error(ErrorKind::SyntaxError {
                position: self.lexer.get_current_token().position.clone(),
                expected_kind: TokenKind::RightParentheses,
                got: self.lexer.get_current_token().kind,
            });
        }

        let block;
        match self.try_parse_block() {
            Some(b) => block = b,
            None => ErrorHandler::fatal(ErrorKind::BlockExpected {}),
        }

        Some(Function {
            name: Rc::new(name),
            parameters: parameters,
            block: block,
        })
    }

    fn parse_parameters(&mut self) -> VecDeque<Parameter> {
        let mut parameters: VecDeque<Parameter> = VecDeque::new();

        match self.lexer.get_current_token().kind {
            TokenKind::Identifier(p) => parameters.push_back(Parameter { name: p }),
            _ => return parameters,
        };

        self.next_token();

        while let TokenKind::Comma = self.lexer.get_current_token().kind {
            self.next_token();

            match self.lexer.get_current_token().kind {
                TokenKind::Identifier(p) => {
                    let parameter = Parameter { name: p.clone() };
                    for par in parameters.clone() {
                        if parameter == par {
                            ErrorHandler::fatal(ErrorKind::DuplicateParameters { name: p })
                        }
                    }

                    parameters.push_back(parameter);
                }
                _ => self.report_error(ErrorKind::SyntaxError {
                    position: self.lexer.get_position(),
                    expected_kind: TokenKind::Identifier("_".to_string()),
                    got: self.lexer.get_current_token().kind,
                }),
            };

            self.next_token();
        }

        parameters
    }

    fn try_parse_block(&mut self) -> Option<Block> {
        if let TokenKind::LeftBracket = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            return None;
        }

        let mut statements: VecDeque<Statement> = VecDeque::new();

        while let Some(stmt) = self.try_parse_statement() {
            statements.push_back(stmt);
        }

        if let TokenKind::RightBracket = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            self.report_error(ErrorKind::SyntaxError {
                position: self.lexer.get_current_token().position.clone(),
                expected_kind: TokenKind::RightBracket,
                got: self.lexer.get_current_token().kind,
            });
        }

        Some(Block {
            statements: statements,
        })
    }

    fn try_parse_statement(&mut self) -> Option<Statement> {
        if let Some(expr) = self.try_parse_expression() {
            if let TokenKind::Semicolon = self.lexer.get_current_token().kind {
                self.next_token();
            } else {
                self.report_error(ErrorKind::SyntaxError {
                    position: self.lexer.get_current_token().position.clone(),
                    expected_kind: TokenKind::Semicolon,
                    got: self.lexer.get_current_token().kind,
                });
            }

            return Some(Statement::Expression(expr));
        }

        if let Some(wh) = self.try_prase_while() {
            return Some(Statement::While(wh));
        }

        if let Some(fr) = self.try_prase_for() {
            return Some(Statement::For(fr));
        }

        if let Some(stmt) = self.try_prase_if() {
            return Some(Statement::If(stmt));
        }

        if let Some(ret) = self.try_parse_return() {
            if let TokenKind::Semicolon = self.lexer.get_current_token().kind {
                self.next_token();
            } else {
                self.report_error(ErrorKind::SyntaxError {
                    position: self.lexer.get_current_token().position.clone(),
                    expected_kind: TokenKind::Semicolon,
                    got: self.lexer.get_current_token().kind,
                });
            }

            return Some(Statement::Return(ret));
        }

        None
    }

    fn try_prase_while(&mut self) -> Option<While> {
        if let TokenKind::While = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            return None;
        }

        if let TokenKind::LeftParentheses = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            self.report_error(ErrorKind::SyntaxError {
                position: self.lexer.get_current_token().position.clone(),
                expected_kind: TokenKind::LeftParentheses,
                got: self.lexer.get_current_token().kind,
            });
        }

        let cond;
        match self.try_parse_expression() {
            Some(c) => cond = c,
            None => ErrorHandler::fatal(ErrorKind::ConditionExpected {
                position: self.lexer.get_position().clone(),
            }),
        }

        // TODO: dodaj what_building
        if let TokenKind::RightParentheses = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            self.report_error(ErrorKind::SyntaxError {
                position: self.lexer.get_current_token().position.clone(),
                expected_kind: TokenKind::RightParentheses,
                got: self.lexer.get_current_token().kind,
            });
        }

        //TODO: DODAJ licznki błedów
        //TODO: dodaj pozycje gdzie cos jest
        let block;
        match self.try_parse_block() {
            Some(b) => block = b,
            None => ErrorHandler::fatal(ErrorKind::BlockExpected {}),
        }

        Some(While {
            condition: Box::new(cond),
            block: Box::new(block),
        })
    }

    fn try_prase_for(&mut self) -> Option<For> {
        if let TokenKind::For = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            return None;
        }

        let ident_iterator: String;

        if let TokenKind::Identifier(ident) = self.lexer.get_current_token().kind {
            ident_iterator = ident;
            self.next_token();
        } else {
            ErrorHandler::fatal(ErrorKind::SyntaxError {
                position: self.lexer.get_current_token().position.clone(),
                expected_kind: TokenKind::Identifier("_".to_string()),
                got: self.lexer.get_current_token().kind,
            });
        }

        if let TokenKind::In = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            self.report_error(ErrorKind::SyntaxError {
                position: self.lexer.get_current_token().position.clone(),
                expected_kind: TokenKind::Identifier("_".to_string()),
                got: self.lexer.get_current_token().kind,
            });
        }

        let object: Expression;
        match self.try_parse_expression() {
            Some(expr) => object = expr,
            None => ErrorHandler::fatal(ErrorKind::ExpressionExpected {
                position: self.lexer.get_current_token().position.clone(),
                got: self.lexer.get_current_token().kind,
            }),
        }

        let block;
        match self.try_parse_block() {
            Some(b) => block = b,
            None => ErrorHandler::fatal(ErrorKind::BlockExpected {}),
        }

        Some(For {
            iterator: ident_iterator,
            object: Box::new(object),
            block: Box::new(block),
        })
    }

    fn try_prase_if(&mut self) -> Option<If> {
        if let TokenKind::If = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            return None;
        }

        if let TokenKind::LeftParentheses = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            self.report_error(ErrorKind::SyntaxError {
                position: self.lexer.get_current_token().position.clone(),
                expected_kind: TokenKind::LeftParentheses,
                got: self.lexer.get_current_token().kind,
            });
        }

        let cond;
        match self.try_parse_expression() {
            Some(c) => cond = c,
            None => ErrorHandler::fatal(ErrorKind::ConditionExpected {
                position: self.lexer.get_position().clone(),
            }),
        }

        if let TokenKind::RightParentheses = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            self.report_error(ErrorKind::SyntaxError {
                position: self.lexer.get_current_token().position.clone(),
                expected_kind: TokenKind::RightParentheses,
                got: self.lexer.get_current_token().kind,
            });
        }

        let block;
        match self.try_parse_block() {
            Some(b) => block = b,
            None => ErrorHandler::fatal(ErrorKind::BlockExpected {}),
        }

        //TODO; przerób na iteracje
        let else_block: Option<Box<If>>;
        if let TokenKind::Else = self.lexer.get_current_token().kind {
            self.next_token();
            match self.try_prase_if() {
                Some(stmt) => else_block = Some(Box::new(stmt)),
                None => {
                    let block;
                    match self.try_parse_block() {
                        Some(b) => block = b,
                        None => ErrorHandler::fatal(ErrorKind::BlockExpected {}),
                    }

                    else_block = Some(Box::new(If {
                        condition: None,
                        block: Box::new(block),
                        else_block: None,
                    }))
                }
            }
        } else {
            else_block = None;
        }

        Some(If {
            condition: Some(Box::new(cond)),
            block: Box::new(block),
            else_block: else_block,
        })
    }

    fn try_parse_return(&mut self) -> Option<Return> {
        if let TokenKind::Return = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            return None;
        }

        match self.try_parse_expression() {
            Some(expr) => Some(Return {
                expression: Some(Box::new(expr)),
            }),
            None => Some(Return { expression: None }),
        }
    }

    fn try_parse_expression(&mut self) -> Option<Expression> {
        let mut left = self.try_parse_and_expression()?;

        let mut operator = self.lexer.get_current_token().kind;
        while matches!(operator, OR_OPERATOR) {
            self.next_token();
            match self.try_parse_and_expression() {
                Some(right) => {
                    left = Expression::OrExpression(OrExpression {
                        left: Box::new(left),
                        right: Box::new(right),
                    })
                }
                None => ErrorHandler::fatal(ErrorKind::IncompleteExpression {
                    position: self.lexer.get_position(),
                    expression_type: left,
                }),
            }
            operator = self.lexer.get_current_token().kind;
        }

        Some(left)
    }

    fn try_parse_and_expression(&mut self) -> Option<Expression> {
        let mut left = self.try_parse_equal_expression()?;

        let mut operator = self.lexer.get_current_token().kind;
        while matches!(operator, AND_OPERATOR) {
            self.next_token();
            match self.try_parse_equal_expression() {
                Some(right) => {
                    left = Expression::AndExpression(AndExpression {
                        left: Box::new(left),
                        right: Box::new(right),
                    })
                }
                None => ErrorHandler::fatal(ErrorKind::IncompleteExpression {
                    position: self.lexer.get_position(),
                    expression_type: left,
                }),
            }
            operator = self.lexer.get_current_token().kind;
        }

        Some(left)
    }

    fn try_parse_equal_expression(&mut self) -> Option<Expression> {
        let mut left = self.try_parse_relational_expression()?;

        if let Some(operator) = EqualOperator::remap(self.lexer.get_current_token()) {
            self.next_token();
            match self.try_parse_relational_expression() {
                Some(right) => {
                    left = Expression::EqualExpression(EqualExpression {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator: operator,
                    })
                }
                None => ErrorHandler::fatal(ErrorKind::IncompleteExpression {
                    position: self.lexer.get_position(),
                    expression_type: left,
                }),
            }
        }

        Some(left)
    }

    fn try_parse_relational_expression(&mut self) -> Option<Expression> {
        let mut left = self.try_parse_additive_expression()?;

        if let Some(operator) = RelationOperator::remap(self.lexer.get_current_token()) {
            self.next_token();
            match self.try_parse_additive_expression() {
                Some(right) => {
                    left = Expression::RelationalExpression(RelationalExpression {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator: operator,
                    })
                }
                None => ErrorHandler::fatal(ErrorKind::IncompleteExpression {
                    position: self.lexer.get_position(),
                    expression_type: left,
                }),
            }
        }

        Some(left)
    }

    fn try_parse_additive_expression(&mut self) -> Option<Expression> {
        let mut left = self.try_parse_multiplicative_expression()?;

        while let Some(operator) = AdditionOperator::remap(self.lexer.get_current_token()) {
            self.next_token();
            match self.try_parse_multiplicative_expression() {
                Some(right) => {
                    left = Expression::AdditiveExpression(AdditiveExpression {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator: operator,
                    })
                }
                None => ErrorHandler::fatal(ErrorKind::IncompleteExpression {
                    position: self.lexer.get_position(),
                    expression_type: left,
                }),
            }
        }

        Some(left)
    }

    fn try_parse_multiplicative_expression(&mut self) -> Option<Expression> {
        let mut left = self.try_parse_unary_expression()?;

        while let Some(operator) = MultiplicationOperator::remap(self.lexer.get_current_token()) {
            self.next_token();
            match self.try_parse_multiplicative_expression() {
                Some(right) => {
                    left = Expression::MultiplicativeExpression(MultiplicativeExpression {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator: operator,
                    })
                }
                None => ErrorHandler::fatal(ErrorKind::IncompleteExpression {
                    position: self.lexer.get_position(),
                    expression_type: left,
                }),
            }
        }

        Some(left)
    }

    fn try_parse_unary_expression(&mut self) -> Option<Expression> {
        if matches!(self.lexer.get_current_token().kind, NOT_OPERATOR) {
            self.next_token();
            let expression = match self.try_parse_expression() {
                Some(e) => e,
                None => ErrorHandler::fatal(ErrorKind::ExpressionExpected {
                    position: self.lexer.get_position(),
                    got: self.lexer.get_current_token().kind,
                }),
            };
            return Some(Expression::UnaryExpression(NotExpression {
                expression: Box::new(expression),
            }));
        }

        if matches!(self.lexer.get_current_token().kind, SUBTRACT_OPERATOR) {
            self.next_token();
            if let TokenKind::Number(n) = self.lexer.get_current_token().kind {
                match n.checked_mul(Decimal::new(-1, 0)) {
                    Some(ns) => {
                        self.next_token();
                        return Some(Expression::Number(ns));
                    }
                    None => ErrorHandler::fatal(ErrorKind::NumberTooBig {
                        position: self.lexer.get_position(),
                    }),
                }
            } else {
                ErrorHandler::fatal(ErrorKind::IncompleteExpression {
                    position: self.lexer.get_current_token().position.clone(),
                    expression_type: Expression::Number(Decimal::new(0, 0)),
                });
            }
        }

        let expression = self.try_parse_has_expression()?;
        Some(expression)
    }

    fn try_parse_has_expression(&mut self) -> Option<Expression> {
        let mut expression = self.try_parse_primary_expression()?;

        let operator = self.lexer.get_current_token().kind;
        if matches!(operator, HAS_OPERATOR) {
            self.next_token();
            if let TokenKind::Identifier(ident) = self.lexer.get_current_token().kind {
                self.next_token();
                expression = Expression::HasExpression(HasExpression {
                    expression: Box::new(expression),
                    ident: ident,
                });
            }
        }

        Some(expression)
    }

    fn try_parse_primary_expression(&mut self) -> Option<Expression> {
        match self.lexer.get_current_token().kind {
            TokenKind::QuotedString(s) => {
                self.next_token();
                Some(Expression::StringLiteral(s))
            }
            TokenKind::Number(n) => {
                self.next_token();
                Some(Expression::Number(n))
            }

            TokenKind::Identifier(_) => self.try_parse_assignment_expression(),
            TokenKind::LeftParentheses => {
                self.next_token();
                let expr = self.try_parse_expression();
                if !matches!(
                    self.lexer.get_current_token().kind,
                    TokenKind::RightParentheses
                ) {
                    self.report_error(ErrorKind::SyntaxError {
                        position: self.lexer.get_current_token().position.clone(),
                        expected_kind: TokenKind::RightParentheses,
                        got: self.lexer.get_current_token().kind,
                    });
                } else {
                    self.next_token();
                }
                return expr;
            }
            _ => None,
        }
    }

    fn try_parse_assignment_expression(&mut self) -> Option<Expression> {
        let mut left = self.try_parse_ident_expression()?;

        if let Some(operator) = AssignmentOperator::remap(self.lexer.get_current_token()) {
            self.next_token();
            match self.try_parse_expression() {
                Some(right) => {
                    left = Expression::AssignmentExpression(AssignmentExpression {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator: operator,
                    })
                }
                None => ErrorHandler::fatal(ErrorKind::IncompleteExpression {
                    position: self.lexer.get_position(),
                    expression_type: left,
                }),
            }
        }

        Some(left)
    }

    fn try_parse_ident_expression(&mut self) -> Option<Expression> {
        let mut path: VecDeque<FunCallOrMember> = VecDeque::new();

        if let Some(f) = self.try_parse_fn_call_or_member_access() {
            path.push_back(f)
        } else {
            return None;
        }

        while let TokenKind::Dot = self.lexer.get_current_token().kind {
            self.next_token();

            if let Some(f) = self.try_parse_fn_call_or_member_access() {
                path.push_back(f)
            } else {
                ErrorHandler::fatal(ErrorKind::SyntaxError {
                    position: self.lexer.get_current_token().position.clone(),
                    expected_kind: TokenKind::RightParentheses,
                    got: self.lexer.get_current_token().kind,
                });
            }
        }

        return Some(Expression::VariableExpression(VariableExpression {
            path: path,
        }));
    }

    fn try_parse_fn_call_or_member_access(&mut self) -> Option<FunCallOrMember> {
        let ident;
        if let TokenKind::Identifier(i) = self.lexer.get_current_token().kind {
            ident = i;
            self.next_token();
        } else {
            return None;
        }

        let arguments = self.try_parse_function_call();

        Some(FunCallOrMember {
            name: ident,
            arguments,
        })
    }

    fn try_parse_function_call(&mut self) -> Option<VecDeque<Argument>> {
        if let TokenKind::LeftParentheses = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            return None;
        }

        let arguments = Some(self.parse_arguments());

        if let TokenKind::RightParentheses = self.lexer.get_current_token().kind {
            self.next_token();
        } else {
            self.report_error(ErrorKind::SyntaxError {
                position: self.lexer.get_current_token().position.clone(),
                expected_kind: TokenKind::RightParentheses,
                got: self.lexer.get_current_token().kind,
            });
        }

        return arguments;
    }

    fn parse_arguments(&mut self) -> VecDeque<Argument> {
        let mut arguments: VecDeque<Argument> = VecDeque::new();

        match self.try_parse_expression() {
            Some(e) => arguments.push_back(Argument { expr: e }),
            None => return arguments,
        }

        while let TokenKind::Comma = self.lexer.get_current_token().kind {
            self.next_token();
            match self.try_parse_expression() {
                Some(e) => arguments.push_back(Argument { expr: e }),
                None => self.report_error(ErrorKind::SyntaxError {
                    position: self.lexer.get_current_token().position.clone(),
                    expected_kind: TokenKind::LeftParentheses,
                    got: self.lexer.get_current_token().kind,
                }),
            }
        }

        arguments
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.lexer.get_next_token() {
            Some(res) => match res {
                Ok(token) => Some(token),
                Err(e) => ErrorHandler::fatal(e),
            },
            None => None,
        }
    }

    fn report_error(&self, err: ErrorKind) {
        if self.fail_fast {
            ErrorHandler::fatal(err)
        } else {
            ErrorHandler::report_error(err)
        }
    }
}

mod tests {

    macro_rules! parser_test {
        (FAIL: $name:ident, $text:expr) => {
            #[test]
            #[should_panic]
            fn $name() {
                let text: &str = $text;

                let test_source = file_handler::TestSource::new(String::from(text), 0);
                let lexer = Lexer::new(Box::new(test_source));
                let mut parser = Parser::new(Box::new(lexer), true);
                parser.parse();
            }
        };
        ($name:ident, $text:expr, $program:expr) => {
            #[test]
            fn $name() {
                let text: &str = $text;
                let should_be: Program = $program;

                let test_source = file_handler::TestSource::new(String::from(text), 0);
                let lexer = Lexer::new(Box::new(test_source));
                let mut parser = Parser::new(Box::new(lexer), false);
                let program = parser.parse();
                assert_eq!(program, should_be)
            }
        };
    }

    use crate::{
        file_handler::{self},
        lexer::Lexer,
    };

    use super::*;

    parser_test!(
        basic_test,
        "main(args, argv) { }",
        Program {
            functions: HashMap::from([(
                "main".to_string(),
                Function {
                    name: Rc::new(String::from("main")),
                    parameters: VecDeque::from_iter([
                        Parameter {
                            name: "args".to_string(),
                        },
                        Parameter {
                            name: "argv".to_string(),
                        }
                    ]),
                    block: Block {
                        statements: VecDeque::new()
                    }
                }
            ),])
        }
    );

    parser_test!(
        number_and_string_parsing,
        "main(args) { xx + 12; xx - \"12\"; }",
        Program {
            functions: HashMap::from([(
                "main".to_string(),
                Function {
                    name: Rc::new(String::from("main")),
                    parameters: VecDeque::from_iter([Parameter {
                        name: "args".to_string(),
                    }]),
                    block: Block {
                        statements: VecDeque::from_iter([
                            Statement::Expression(Expression::AdditiveExpression(
                                AdditiveExpression {
                                    left: Box::new(Expression::VariableExpression(
                                        VariableExpression {
                                            path: VecDeque::from_iter([FunCallOrMember {
                                                name: "xx".to_string(),
                                                arguments: None
                                            }]),
                                        }
                                    )),
                                    right: Box::new(Expression::Number(Decimal::from(12))),
                                    operator: AdditionOperator::Add
                                }
                            )),
                            Statement::Expression(Expression::AdditiveExpression(
                                AdditiveExpression {
                                    left: Box::new(Expression::VariableExpression(
                                        VariableExpression {
                                            path: VecDeque::from_iter([FunCallOrMember {
                                                name: "xx".to_string(),
                                                arguments: None
                                            }]),
                                        }
                                    )),
                                    right: Box::new(Expression::StringLiteral("12".to_string())),
                                    operator: AdditionOperator::Subtract
                                }
                            ))
                        ])
                    }
                }
            ),])
        }
    );

    parser_test!(
        negative_number_parsing,
        "main(args) { - 1444 }",
        Program {
            functions: HashMap::from([(
                "main".to_string(),
                Function {
                    name: Rc::new(String::from("main")),
                    parameters: VecDeque::from_iter([Parameter {
                        name: "args".to_string(),
                    }]),
                    block: Block {
                        statements: VecDeque::from_iter([Statement::Expression(
                            Expression::Number(Decimal::new(-1444, 0))
                        ),])
                    }
                }
            ),])
        }
    );

    parser_test!(
        return_parsing,
        "main(args) { return ; return xx - \"12\"; }",
        Program {
            functions: HashMap::from([(
                "main".to_string(),
                Function {
                    name: Rc::new(String::from("main")),
                    parameters: VecDeque::from_iter([Parameter {
                        name: "args".to_string(),
                    }]),
                    block: Block {
                        statements: VecDeque::from_iter([
                            Statement::Return(Return { expression: None }),
                            Statement::Return(Return {
                                expression: Some(Box::new(Expression::AdditiveExpression(
                                    AdditiveExpression {
                                        left: Box::new(Expression::VariableExpression(
                                            VariableExpression {
                                                path: VecDeque::from_iter([FunCallOrMember {
                                                    name: "xx".to_string(),
                                                    arguments: None
                                                }]),
                                            }
                                        )),
                                        right: Box::new(Expression::StringLiteral(
                                            "12".to_string()
                                        )),
                                        operator: AdditionOperator::Subtract
                                    }
                                )))
                            })
                        ])
                    }
                }
            ),])
        }
    );

    parser_test!(
        has_parsing,
        "main(args) { xx has faf ; xx.x() has XD ;}",
        Program {
            functions: HashMap::from([(
                "main".to_string(),
                Function {
                    name: Rc::new(String::from("main")),
                    parameters: VecDeque::from_iter([Parameter {
                        name: "args".to_string(),
                    }]),
                    block: Block {
                        statements: VecDeque::from_iter([
                            Statement::Expression(Expression::HasExpression(HasExpression {
                                expression: Box::new(Expression::VariableExpression(
                                    VariableExpression {
                                        path: VecDeque::from_iter([FunCallOrMember {
                                            name: "xx".to_string(),
                                            arguments: None
                                        }]),
                                    }
                                )),
                                ident: "faf".to_string()
                            })),
                            Statement::Expression(Expression::HasExpression(HasExpression {
                                expression: Box::new(Expression::VariableExpression(
                                    VariableExpression {
                                        path: VecDeque::from_iter([
                                            FunCallOrMember {
                                                name: "xx".to_string(),
                                                arguments: None
                                            },
                                            FunCallOrMember {
                                                name: "x".to_string(),
                                                arguments: Some(VecDeque::new())
                                            }
                                        ]),
                                    }
                                )),
                                ident: "XD".to_string()
                            })),
                        ])
                    }
                }
            ),])
        }
    );

    parser_test!(
        multiple_expressions,
        "main(args) { xx + 12; xx - 12; xx * 12; xx / 12; xx or 12; }",
        Program {
            functions: HashMap::from([(
                "main".to_string(),
                Function {
                    name: Rc::new(String::from("main")),
                    parameters: VecDeque::from_iter([Parameter {
                        name: "args".to_string(),
                    }]),
                    block: Block {
                        statements: VecDeque::from_iter([
                            Statement::Expression(Expression::AdditiveExpression(
                                AdditiveExpression {
                                    left: Box::new(Expression::VariableExpression(
                                        VariableExpression {
                                            path: VecDeque::from_iter([FunCallOrMember {
                                                name: "xx".to_string(),
                                                arguments: None
                                            }]),
                                        }
                                    )),
                                    right: Box::new(Expression::Number(Decimal::from(12))),
                                    operator: AdditionOperator::Add
                                }
                            )),
                            Statement::Expression(Expression::AdditiveExpression(
                                AdditiveExpression {
                                    left: Box::new(Expression::VariableExpression(
                                        VariableExpression {
                                            path: VecDeque::from_iter([FunCallOrMember {
                                                name: "xx".to_string(),
                                                arguments: None
                                            }]),
                                        }
                                    )),
                                    right: Box::new(Expression::Number(Decimal::from(12))),
                                    operator: AdditionOperator::Subtract
                                }
                            )),
                            Statement::Expression(Expression::MultiplicativeExpression(
                                MultiplicativeExpression {
                                    left: Box::new(Expression::VariableExpression(
                                        VariableExpression {
                                            path: VecDeque::from_iter([FunCallOrMember {
                                                name: "xx".to_string(),
                                                arguments: None
                                            }]),
                                        }
                                    )),
                                    right: Box::new(Expression::Number(Decimal::from(12))),
                                    operator: MultiplicationOperator::Multiplication
                                }
                            )),
                            Statement::Expression(Expression::MultiplicativeExpression(
                                MultiplicativeExpression {
                                    left: Box::new(Expression::VariableExpression(
                                        VariableExpression {
                                            path: VecDeque::from_iter([FunCallOrMember {
                                                name: "xx".to_string(),
                                                arguments: None
                                            }]),
                                        }
                                    )),
                                    right: Box::new(Expression::Number(Decimal::from(12))),
                                    operator: MultiplicationOperator::Division
                                }
                            )),
                            Statement::Expression(Expression::OrExpression(OrExpression {
                                left: Box::new(Expression::VariableExpression(
                                    VariableExpression {
                                        path: VecDeque::from_iter([FunCallOrMember {
                                            name: "xx".to_string(),
                                            arguments: None
                                        }]),
                                    }
                                )),
                                right: Box::new(Expression::Number(Decimal::from(12))),
                            }))
                        ])
                    }
                }
            ),])
        }
    );

    parser_test!(
        parsing_arguments,
        "main(args) { xx += 12; xx.x(ee); xx.x().ee(add(y.yy.yyy)); }",
        Program {
            functions: HashMap::from([(
                "main".to_string(),
                Function {
                    name: Rc::new(String::from("main")),
                    parameters: VecDeque::from_iter([Parameter {
                        name: "args".to_string(),
                    }]),
                    block: Block {
                        statements: VecDeque::from_iter([
                            Statement::Expression(Expression::AssignmentExpression(
                                AssignmentExpression {
                                    left: Box::new(Expression::VariableExpression(
                                        VariableExpression {
                                            path: VecDeque::from_iter([FunCallOrMember{ name: "xx".to_string(), arguments: None }]),
                                        }
                                    )),
                                    right: Box::new(Expression::Number(Decimal::from(12))),
                                    operator: AssignmentOperator::AddAssignment
                                }
                            )),
                            Statement::Expression(Expression::VariableExpression(
                                VariableExpression {
                                    path: VecDeque::from_iter([FunCallOrMember{ name: "xx".to_string(), arguments: None }, 
                                    FunCallOrMember{ name: "x".to_string(), arguments: Some(VecDeque::from_iter([Argument {
                                        expr: Expression::VariableExpression(VariableExpression {
                                            path: VecDeque::from_iter([FunCallOrMember{ name: "ee".to_string(), arguments: None }]),
                                        }),
                                    }])) }]),
                                }
                            )),
                            Statement::Expression(Expression::VariableExpression(
                                VariableExpression {
                                    path: VecDeque::from_iter([
                                        FunCallOrMember{ name: "xx".to_string(), arguments: None },
                                        FunCallOrMember{ name: "x".to_string(), arguments: Some(VecDeque::new()) },
                                        FunCallOrMember{ name: "ee".to_string(), arguments: Some(VecDeque::from_iter([Argument {
                                            expr: Expression::VariableExpression(VariableExpression {
                                                path: VecDeque::from_iter([FunCallOrMember{ name: "add".to_string(),
                                                arguments: Some(VecDeque::from_iter([Argument {
                                                    expr: Expression::VariableExpression(
                                                        VariableExpression {
                                                            path: VecDeque::from_iter([
                                                                FunCallOrMember{ name: "y".to_string(), arguments: None },
                                                                FunCallOrMember{ name: "yy".to_string(), arguments: None },
                                                                FunCallOrMember{ name: "yyy".to_string(), arguments: None }
                                                            ]),
                                                        }
                                                    ),
                                                }]))
                                             }]),
                                            }),
                                        }])) }
                                    ]),
                                }
                            ))
                        ])
                    }
                }
            ),])
        }
    );

    parser_test!(
        while_parsing,
        "main(args) { while (xx != 1) { xx + 12; } }",
        Program {
            functions: HashMap::from([(
                "main".to_string(),
                Function {
                    name: Rc::new(String::from("main")),
                    parameters: VecDeque::from_iter([Parameter {
                        name: "args".to_string(),
                    }]),
                    block: Block {
                        statements: VecDeque::from_iter([Statement::While(While {
                            condition: Box::new(Expression::EqualExpression(EqualExpression {
                                left: Box::new(Expression::VariableExpression(
                                    VariableExpression {
                                        path: VecDeque::from_iter([FunCallOrMember {
                                            name: "xx".to_string(),
                                            arguments: None
                                        }]),
                                    }
                                )),
                                right: Box::new(Expression::Number(Decimal::from(1))),
                                operator: EqualOperator::NotEqual
                            })),
                            block: Box::new(Block {
                                statements: VecDeque::from_iter([Statement::Expression(
                                    Expression::AdditiveExpression(AdditiveExpression {
                                        left: Box::new(Expression::VariableExpression(
                                            VariableExpression {
                                                path: VecDeque::from_iter([FunCallOrMember {
                                                    name: "xx".to_string(),
                                                    arguments: None
                                                }]),
                                            }
                                        )),
                                        right: Box::new(Expression::Number(Decimal::from(12))),
                                        operator: AdditionOperator::Add
                                    })
                                )])
                            })
                        })])
                    }
                }
            ),])
        }
    );

    parser_test!(
        for_parsing,
        "main(args) { for xx in eee { xx + 12; } }",
        Program {
            functions: HashMap::from([(
                "main".to_string(),
                Function {
                    name: Rc::new(String::from("main")),
                    parameters: VecDeque::from_iter([Parameter {
                        name: "args".to_string(),
                    }]),
                    block: Block {
                        statements: VecDeque::from_iter([Statement::For(For {
                            object: Box::new(Expression::VariableExpression(VariableExpression {
                                path: VecDeque::from_iter([FunCallOrMember {
                                    name: "eee".to_string(),
                                    arguments: None
                                }]),
                            })),
                            iterator: "xx".to_string(),
                            block: Box::new(Block {
                                statements: VecDeque::from_iter([Statement::Expression(
                                    Expression::AdditiveExpression(AdditiveExpression {
                                        left: Box::new(Expression::VariableExpression(
                                            VariableExpression {
                                                path: VecDeque::from_iter([FunCallOrMember {
                                                    name: "xx".to_string(),
                                                    arguments: None
                                                }]),
                                            }
                                        )),
                                        right: Box::new(Expression::Number(Decimal::from(12))),
                                        operator: AdditionOperator::Add
                                    })
                                )])
                            })
                        })])
                    }
                }
            ),])
        }
    );

    parser_test!(
        if_parsing,
        "main(args) { if (xx != 1) { xx + 12; } }",
        Program {
            functions: HashMap::from([(
                "main".to_string(),
                Function {
                    name: Rc::new(String::from("main")),
                    parameters: VecDeque::from_iter([Parameter {
                        name: "args".to_string(),
                    }]),
                    block: Block {
                        statements: VecDeque::from_iter([Statement::If(If {
                            condition: Some(Box::new(Expression::EqualExpression(
                                EqualExpression {
                                    left: Box::new(Expression::VariableExpression(
                                        VariableExpression {
                                            path: VecDeque::from_iter([FunCallOrMember {
                                                name: "xx".to_string(),
                                                arguments: None
                                            }]),
                                        }
                                    )),
                                    right: Box::new(Expression::Number(Decimal::from(1))),
                                    operator: EqualOperator::NotEqual
                                }
                            ))),
                            else_block: None,
                            block: Box::new(Block {
                                statements: VecDeque::from_iter([Statement::Expression(
                                    Expression::AdditiveExpression(AdditiveExpression {
                                        left: Box::new(Expression::VariableExpression(
                                            VariableExpression {
                                                path: VecDeque::from_iter([FunCallOrMember {
                                                    name: "xx".to_string(),
                                                    arguments: None
                                                }]),
                                            }
                                        )),
                                        right: Box::new(Expression::Number(Decimal::from(12))),
                                        operator: AdditionOperator::Add
                                    })
                                )])
                            })
                        })])
                    }
                }
            ),])
        }
    );

    parser_test!(
        if_else_parsing,
        "main(args) {
         if (xx != 1) {
              xx + 12; 
        } else { xx + 12; } 
       }",
        Program {
            functions: HashMap::from([(
                "main".to_string(),
                Function {
                    name: Rc::new(String::from("main")),
                    parameters: VecDeque::from_iter([Parameter {
                        name: "args".to_string(),
                    }]),
                    block: Block {
                        statements: VecDeque::from_iter([Statement::If(If {
                            condition: Some(Box::new(Expression::EqualExpression(
                                EqualExpression {
                                    left: Box::new(Expression::VariableExpression(
                                        VariableExpression {
                                            path: VecDeque::from_iter([FunCallOrMember {
                                                name: "xx".to_string(),
                                                arguments: None
                                            }]),
                                        }
                                    )),
                                    right: Box::new(Expression::Number(Decimal::from(1))),
                                    operator: EqualOperator::NotEqual
                                }
                            ))),
                            else_block: Some(Box::new(If {
                                condition: None,
                                else_block: None,
                                block: Box::new(Block {
                                    statements: VecDeque::from_iter([Statement::Expression(
                                        Expression::AdditiveExpression(AdditiveExpression {
                                            left: Box::new(Expression::VariableExpression(
                                                VariableExpression {
                                                    path: VecDeque::from_iter([FunCallOrMember {
                                                        name: "xx".to_string(),
                                                        arguments: None
                                                    }]),
                                                }
                                            )),
                                            right: Box::new(Expression::Number(Decimal::from(12))),
                                            operator: AdditionOperator::Add
                                        })
                                    )])
                                })
                            })),
                            block: Box::new(Block {
                                statements: VecDeque::from_iter([Statement::Expression(
                                    Expression::AdditiveExpression(AdditiveExpression {
                                        left: Box::new(Expression::VariableExpression(
                                            VariableExpression {
                                                path: VecDeque::from_iter([FunCallOrMember {
                                                    name: "xx".to_string(),
                                                    arguments: None
                                                }]),
                                            }
                                        )),
                                        right: Box::new(Expression::Number(Decimal::from(12))),
                                        operator: AdditionOperator::Add
                                    })
                                )])
                            })
                        })])
                    }
                }
            ),])
        }
    );

    parser_test!(if_else_if_else_parsing, "main(args) { if (xx != 1) { xx + 12; } else if (xx == 1) { xx + 13; } else { xx + 11; }  }", Program{
        functions: HashMap::from([
            ("main".to_string(),  
            Function{name: Rc::new(String::from("main")), 
            parameters: VecDeque::from_iter([
                Parameter{name: "args".to_string()}]), 
                block: Block { statements: VecDeque::from_iter([
                    Statement::If(If{
                        condition: Some(Box::new(Expression::EqualExpression(EqualExpression{
                            left: Box::new(Expression::VariableExpression(VariableExpression{
                                        path: VecDeque::from_iter([FunCallOrMember{ name: "xx".to_string(), arguments: None }]),
                                    })),
                            right: Box::new(Expression::Number(Decimal::from(1))),
                            operator: EqualOperator::NotEqual
                        }))),
                        else_block: Some(Box::new(If{
                            condition: Some(Box::new(Expression::EqualExpression(EqualExpression{
                                left: Box::new(Expression::VariableExpression(VariableExpression{
                                            path: VecDeque::from_iter([FunCallOrMember{ name: "xx".to_string(), arguments: None }]),
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
                                                path: VecDeque::from_iter([FunCallOrMember{ name: "xx".to_string(), arguments: None }]),
                                            })),
                                            right: Box::new(Expression::Number(Decimal::from(11))),
                                            operator: AdditionOperator::Add
                                        }))
                                ])})
                            })),
                            block: Box::new(Block{
                                statements: VecDeque::from_iter([
                                    Statement::Expression(Expression::AdditiveExpression(AdditiveExpression{
                                        left: Box::new(Expression::VariableExpression(VariableExpression{
                                            path: VecDeque::from_iter([FunCallOrMember{ name: "xx".to_string(), arguments: None }]),
                                        })),
                                        right: Box::new(Expression::Number(Decimal::from(13))),
                                        operator: AdditionOperator::Add
                                    }))
                            ])})
                        })),
                        block: Box::new(Block{
                            statements: VecDeque::from_iter([
                                Statement::Expression(Expression::AdditiveExpression(AdditiveExpression{
                                    left: Box::new(Expression::VariableExpression(VariableExpression{
                                        path: VecDeque::from_iter([FunCallOrMember{ name: "xx".to_string(), arguments: None }]),
                                    })),
                                    right: Box::new(Expression::Number(Decimal::from(12))),
                                    operator: AdditionOperator::Add
                                }))
                        ]) })
                    })
                ]) }
                 }),
            ])
    });

    parser_test!(
        multiple_level_parsing,
        "main(args) { if (xx != 1) { for ele in yy {xx + 12; while(xx != 40) { yy(xx + 12); if(yy) { } } } } }",
        Program {
            functions: HashMap::from([(
                "main".to_string(),
                Function {
                    name: Rc::new(String::from("main")),
                    parameters: VecDeque::from_iter([Parameter {
                        name: "args".to_string(),
                    }]),
                    block: Block {
                        statements: VecDeque::from_iter([Statement::If(If {
                            condition: Some(Box::new(Expression::EqualExpression(
                                EqualExpression {
                                    left: Box::new(Expression::VariableExpression(
                                        VariableExpression {
                                            path: VecDeque::from_iter([FunCallOrMember{ name: "xx".to_string(), arguments: None }]),
                                        }
                                    )),
                                    right: Box::new(Expression::Number(Decimal::from(1))),
                                    operator: EqualOperator::NotEqual
                                }
                            ))),
                            else_block: None,
                            block: Box::new(Block {
                                statements: VecDeque::from_iter([Statement::For(For {
                                    object: Box::new(Expression::VariableExpression(VariableExpression {
                                        path: VecDeque::from_iter([FunCallOrMember{ name: "yy".to_string(), arguments: None }]),
                                    })),
                                    iterator: "ele".to_string(),
                                    block: Box::new(Block {
                                        statements: VecDeque::from_iter([Statement::Expression(
                                            Expression::AdditiveExpression(AdditiveExpression {
                                                left: Box::new(Expression::VariableExpression(
                                                    VariableExpression {
                                                        path: VecDeque::from_iter([FunCallOrMember{ name: "xx".to_string(), arguments: None }]),
                                                    }
                                                )),
                                                right: Box::new(Expression::Number(Decimal::from(12))),
                                                operator: AdditionOperator::Add
                                            })
                                        ), Statement::While(While {
                                            condition: Box::new(Expression::EqualExpression(EqualExpression {
                                                left: Box::new(Expression::VariableExpression(
                                                    VariableExpression {
                                                        path: VecDeque::from_iter([FunCallOrMember{ name: "xx".to_string(), 
                                                        arguments: None
                                                    }]),
                                                    }
                                                )),
                                                right: Box::new(Expression::Number(Decimal::from(40))),
                                                operator: EqualOperator::NotEqual
                                            })),
                                            block: Box::new(Block {
                                                statements: VecDeque::from_iter([Statement::Expression(
                                                    Expression::VariableExpression(VariableExpression {
                                                        path: VecDeque::from_iter([FunCallOrMember{ name: "yy".to_string(), arguments: Some(VecDeque::from_iter([
                                                            Argument{ expr: Expression::AdditiveExpression(AdditiveExpression{
                                                                left: Box::new(Expression::VariableExpression(
                                                                    VariableExpression {
                                                                        path: VecDeque::from_iter([FunCallOrMember{ name: "xx".to_string(), arguments: None }]),
                                                                    }
                                                                )),
                                                                right: Box::new(Expression::Number(Decimal::from(12))),
                                                                operator: AdditionOperator::Add })}
                                                        ])) }]),
                                                    })
                                                ), Statement::If(If {
                                                    condition: Some(Box::new(Expression::VariableExpression(VariableExpression{
                                                        path: VecDeque::from_iter([FunCallOrMember{ name: "yy".to_string(), arguments: None }]),
                                                         }))),
                                                    else_block: None,
                                                    block: Box::new(Block {
                                                        statements: VecDeque::new()
                                                    })
                                                })
                                                ])
                                            })
                                        })
                                        ])
                                    })
                                })])
                            })
                        })])
                    }
                }
            ),])
        }
    );

    parser_test!(
        operator_order,
        "main(args) { x * (13 + y) - 10 > 5 != 6 and yy or !(4 == 5) ;}",
        Program {
            functions: HashMap::from([(
                "main".to_string(),
                Function {
                    name: Rc::new(String::from("main")),
                    parameters: VecDeque::from_iter([Parameter {
                        name: "args".to_string(),
                    }]),
                    block: Block {
                        statements: VecDeque::from_iter([
                            Statement::Expression(Expression::OrExpression(
                                OrExpression {
                                    left: Box::new(Expression::AndExpression(
                                        AndExpression {
                                            left: Box::new(Expression::EqualExpression(
                                                EqualExpression{
                                                    left: Box::new(Expression::RelationalExpression(RelationalExpression{
                                                        left: Box::new(Expression::AdditiveExpression(AdditiveExpression {
                                                            left: Box::new(Expression::MultiplicativeExpression(MultiplicativeExpression{
                                                                left: Box::new(Expression::VariableExpression(VariableExpression{
                                                                    path: VecDeque::from_iter([FunCallOrMember{ name: "x".to_string(), arguments: None }]),
                                                                     })),
                                                                right: Box::new(Expression::AdditiveExpression(AdditiveExpression{
                                                                    left: Box::new(Expression::Number(Decimal::from(13))),
                                                                    right: Box::new(Expression::VariableExpression(VariableExpression{
                                                                        path: VecDeque::from_iter([FunCallOrMember{ name: "y".to_string(), arguments: None }]),
                                                                         })),
                                                                    operator: AdditionOperator::Add
                                                                })),
                                                                operator: MultiplicationOperator::Multiplication
                                                            })),
                                                            right: Box::new(Expression::Number(Decimal::from(10))),
                                                            operator: AdditionOperator::Subtract }
                                                        )),
                                                        right: Box::new(Expression::Number(Decimal::from(5))),
                                                        operator: RelationOperator::Grater
                                                    })),
                                                    right: Box::new(Expression::Number(Decimal::from(6))),
                                                    operator: EqualOperator::NotEqual
                                                })),
                                            right: Box::new(Expression::VariableExpression(VariableExpression{
                                                path: VecDeque::from_iter([FunCallOrMember{ name: "yy".to_string(), arguments: None }]),
                                                 })) ,}
                                    )),
                                    right: Box::new(Expression::UnaryExpression(NotExpression{
                                        expression: Box::new(Expression::EqualExpression(EqualExpression{
                                            left: Box::new(Expression::Number(Decimal::from(4))),
                                            right: Box::new(Expression::Number(Decimal::from(5))),
                                            operator: EqualOperator::Equal }))
                                    })),
                                }
                            )),
                        ])
                    }
                }
            ),])
        }
    );

    parser_test!(FAIL: no_condition, "main() { while() {} }");
    parser_test!(FAIL: no_condition2, "main() { if() {} }");
    parser_test!(FAIL: no_iterator, "main() { for in yyy {} }");
    parser_test!(FAIL: no_object, "main() { for x in  {} }");
    parser_test!(FAIL: incomplete_expression, "main() { x or }");
    parser_test!(FAIL: incomplete_expression2, "main() { x and }");
    parser_test!(FAIL: incomplete_expression3, "main() { x = }");
    parser_test!(FAIL: incomplete_expression4, "main() { x + }");
    parser_test!(FAIL: incomplete_expression5, "main() { x / }");

    parser_test!(FAIL: syntax_error, "main) { x / 12; }");
    parser_test!(FAIL: syntax_error2, "main( { x / 12; }");
    parser_test!(FAIL: syntax_error3, "main(args, argv)  x / 12; }");
    parser_test!(FAIL: syntax_error7, "main(args, argv) { if(xx) { x / 12;} ");
    parser_test!(FAIL: syntax_error4, "main(args, argv) { x / 12 }");
    parser_test!(FAIL: syntax_error5, "main(args, argv) { if {x / 12;} }");
    parser_test!(FAIL: syntax_error6, "main(args, argv) { if(xx) { x / 12; }");

    parser_test!(
        FAIL: syntax_error8,
        "main(args, argv) { if(xx) { x / 12; } else  }"
    );

    parser_test!(
        FAIL: syntax_error9,
        "main(args, argv) { if(xx) { x / 12; } else if {} }"
    );
    parser_test!(FAIL: syntax_error10, "{}");
    parser_test!(FAIL: syntax_error11, "{ if(xx) { x / 12; } else if {} }");
    parser_test!(FAIL: syntax_error12, "main() { x.x. }");
    parser_test!(FAIL: syntax_error13, "main(args,) { x.(yy,yy) }");
    parser_test!(FAIL: syntax_error14, "main(args) { x.(yy,) }");
    parser_test!(FAIL: syntax_error15, "main(args) { x has   }");
    parser_test!(FAIL: syntax_error16, "main(args) { while( {}   }");
    parser_test!(FAIL: syntax_error17, "main(args) { while) {}   }");
    parser_test!(FAIL: syntax_error18, "main(args) { if( {}   }");
    parser_test!(FAIL: syntax_error19, "main(args) { if) {}   }");
    parser_test!(FAIL: syntax_error20, "main(args) { -   }");
    parser_test!(
        FAIL: syntax_error21,
        "main(args) { x.xx() } main(args) { x.xx() }"
    );
    parser_test!(FAIL: syntax_error22, "main(args, args) {x.x();}");

}
