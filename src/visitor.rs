use std::collections::VecDeque;

use crate::{executor::Value, types::*};

pub trait Visitor<T> {
    fn visit_program(&mut self, n: &Program) -> T;
    fn visit_function_call(&mut self, name: String, arguments: VecDeque<Argument>) -> T;
    fn visit_argument(&mut self, n: &Argument) -> T;
    fn visit_block(&mut self, n: &Block);
    fn visit_expr(&mut self, e: &Expression) -> T;
    fn visit_if(&mut self, e: &If);
    fn visit_for(&mut self, e: &For);
    fn visit_while(&mut self, e: &While);
    fn visit_return(&mut self, e: &Return);
}
