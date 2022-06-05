use std::collections::VecDeque;

use rust_decimal::Decimal;

use crate::{executor::Value, types::*};

//TODO: executor sam sobie przechowuje to 
pub trait Visitor<T> {
    fn visit_program(&mut self, n: &Program) -> T;
    fn visit_function_call(&mut self, name: String, arguments: VecDeque<Argument>, pos: Position) -> T;
    fn visit_argument(&mut self, n: &Argument) -> T;
    fn visit_block(&mut self, n: &Block) -> T;
    fn visit_expr(&mut self, e: &Expression) -> T;
    fn visit_if(&mut self, e: &If) -> T;
    fn visit_for(&mut self, e: &For) -> T;
    fn visit_while(&mut self, e: &While) -> T;
    fn visit_return(&mut self, e: &Return) -> T;
    fn visit_or_expression(&mut self, e: &OrExpression) -> T;
    fn visit_and_expression(&mut self, e: &AndExpression) -> T;
    fn visit_equal_expression(&mut self, e: &EqualExpression) -> T;
    fn visit_relation_expression(&mut self, e: &RelationalExpression) -> T;
    fn visit_additive_expression(&mut self, e: &AdditiveExpression) -> T;
    fn visit_multiplicative_expression(&mut self, e: &MultiplicativeExpression) -> T;
    fn visit_unary_expression(&mut self, e: &NotExpression) -> T;
    fn visit_has_expression(&mut self, e: &HasExpression) -> T;
    fn visit_string_expression(&mut self, e: &String) -> T;
    fn visit_number_expression(&mut self, e: &Decimal) -> T;
    fn visit_variable_expression(&mut self, e: &VariableExpression) -> T;
    fn visit_assignment_expression(&mut self, e: &AssignmentExpression) -> T;
}