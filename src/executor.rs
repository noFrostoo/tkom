use std::{collections::{VecDeque, HashMap}, rc::Rc, iter::zip, ops::Add, borrow::{BorrowMut, Borrow}, cell::RefCell};

use rust_decimal::{Decimal};

use crate::{types::*, visitor::Visitor, errors::{ErrorKind, ErrorHandler}};

type ObjectRef = Rc<RefCell<Object>>;

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Object(Rc<RefCell<Object>>),
    Number(Decimal),
    String(String),
    Bool(bool),
    Function(Function),
    None
}

#[derive(Clone, PartialEq, Debug)]
pub struct Object {
    fields: HashMap<String, Value>
}

impl Object {
    pub fn new() -> Self { Self { fields:HashMap::new() } }

    pub fn has(&self, name: String) -> bool {
        match self.fields.get(&name) {
            Some(_) => return true,
            None => return false,
        }
    }

    pub fn access_field(&self, name: &String) -> Option<Value> {
        match self.fields.get(name) {
            Some(v) => Some(v.clone()),
            None => None,
        }
    }

    pub fn add_field(&mut self, name: &String, value: Value) {
        self.fields.insert(name.clone(), value);
    }

    pub fn len(&self) -> usize {
        self.fields.len()
    }

}

#[derive(Clone, PartialEq, Debug)]
pub struct Scope {
    values: HashMap<String, Value>,
}

impl Scope {
    pub fn new() -> Self { Self { values: HashMap::new() } }

    pub fn insert_var(&mut self, name: &String, value: Value) {
        self.values.insert(name.clone(), value);
    }

    pub fn get_val(&self, name: &String) -> Option<&Value> {
        match self.values.get(name) {
            Some(v) => Some(v),
            None => None,
        } 
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunCallContext {
    name: String,
    scopes: VecDeque<Scope>,
    cur_return:Value,
    returnable: bool
}

impl FunCallContext {

    fn new_scope(&mut self) {
        let scope = Scope::new();
        self.scopes.push_back(scope);
    }

    fn find_var_in_scopes(&mut self, name: &String) -> Option<&Value> {
        // can unwrap here since stack can't be empty
        for scope in self.scopes.borrow_mut().into_iter().rev() {
            match scope.get_val(name) {
                Some(v) => {return Some(&v)},
                None => {},
            }
        }

        None
    }

    fn update_var_in_scopes(&mut self, name: &String, value: Value) {
        // can unwrap here since stack can't be empty
        for scope in self.scopes.borrow_mut().into_iter().rev() {
            match scope.get_val(name) {
                Some(_) => {scope.insert_var(name, value); return;},
                None => {},
            }
        }
    }

    fn dope_scope(&mut self) {
        self.scopes.remove(self.scopes.len()-1);
    }
}

/*
Sem check:
1.Check functions names 
2. 

Execution:
1. visit program 
2. load functions, checks ?
3. visit main function
4. visit statements
5. so expressions and so on so on

TODO: ADD NONE

!!! ADD NONE


*/

const main_func_name: &str = "main";

pub struct Executor {
    functions: HashMap<String, Function>,
    stack: VecDeque<FunCallContext>,
    standard_lib: HashMap<String, fn(Option<VecDeque<Argument>>)->Value>
}

impl Executor {
    pub fn new() -> Self { Self { functions: HashMap::new(), stack: VecDeque::new(), standard_lib: HashMap::new() } }

    
}

impl Visitor<Value> for Executor {
    fn visit_program(&mut self, n: &Program) {
        self.functions = n.functions.clone();
        self.visit_function_call("main".to_string(), None);

    }

    fn visit_function_call(&mut self, name: String, arguments: Option<VecDeque<Argument>>) -> Value{
        if let Some(func) = self.standard_lib.get(&name) {
            return func(arguments);
        }

        let func = self.functions.borrow_mut().get(&name);
        if let Some(f) = func {
            self.user_function_call(f, arguments);
            return Value::None;
        }

        self.error(ErrorKind::UnknownFunction);
        Value::None
    }

    fn visit_argument(&mut self, n: &Argument) -> Value {
        self.visit_expr(&n.expr)
    }

    fn visit_block(&mut self, n: &Block) {
        for stmt in n.statements.to_owned() {
            match stmt {
                Statement::Expression(e) => {self.visit_expr(&e);},
                Statement::If(i) => self.visit_if(&i),
                Statement::For(f) => self.visit_for(&f),
                Statement::While(w) => self.visit_while(&w),
                Statement::Return(_) => todo!(),
            };
        }

    }

    fn visit_expr(&mut self, e: &Expression) -> Value {
        match e {
            Expression::OrExpression(e) => Value::Bool(self.or_cond(e)),
            Expression::AndExpression(e) => Value::Bool(self.and_cond(e)),
            Expression::EqualExpression(e) => Value::Bool(self.equal_cond(e)),
            Expression::RelationalExpression(e) => Value::Bool(self.relational_cond(e)),
            Expression::AdditiveExpression(e) => self.add_expr(e),
            Expression::MultiplicativeExpression(e) => self.mul_expr(e),
            Expression::UnaryExpression(e) => self.unary_expr(e),
            Expression::HasExpression(e) => self.has_expr(e),
            Expression::StringLiteral(s) => Value::String(s.clone()),
            Expression::Number(d) => Value::Number(*d),
            Expression::VariableExpression(e) => (self.variable_expr(e).clone()),
            Expression::AssignmentExpression(e) => todo!(),
        }
    }

    fn visit_if(&mut self, e: &If) {
        self.new_scope();
        
        let condExpr = e.condition.clone();
        let mut cond = false;
        match condExpr {
            Some(c) => {
                let expr = self.visit_expr(&c);
                cond = self.value_to_bool(&expr);
            },
            None => {cond = false},
        }

        if cond {
            self.visit_block(&e.block)
        } else {
            match &e.else_block {
                Some(e) => self.visit_if(&e),
                None => {},
            }
        }
    }

    fn visit_for(&mut self, e: &For) {
        let iterator = self.visit_expr(&e.object);
        if let Value::Object(o) = iterator {
            self.new_scope();
            for elem in (*o).borrow_mut().fields {
                self.current_context().scopes.back_mut().unwrap().insert_var(&e.iterator, Value::None);
                self.visit_block(&e.block);
            }
        } else {
            self.error(ErrorKind::NotIterable);
        }
    }

    fn visit_while(&mut self, e: &While) {
        let mut expr = self.visit_expr(&e.condition);
        let mut value = self.value_to_bool(&expr);
        
        if !value{
            return;
        }

        self.new_scope();

        while value {
            expr = self.visit_expr(&e.condition);
            value = self.value_to_bool(&expr);
            self.visit_block(&e.block);
        }
    }

    fn visit_return(&mut self, r: &Return) {
        match &r.expression {
            Some(_) => todo!(),
            None => todo!(),
        }



    }
}



impl Executor {
    fn logical_cond(&mut self, left_expr: &Expression, right_expr: &Expression) -> (bool, bool) {
        let left = self.visit_expr(left_expr);
        let right = self.visit_expr(right_expr);

        return (self.value_to_bool(&left), self.value_to_bool(&right))
    }

    fn value_to_bool(&mut self, val: &Value) -> bool {
        match val {
            Value::Object(o) => (**o).borrow_mut().len() != 0,
            Value::Number(n) => n.is_sign_positive(),
            Value::String(s) => s.len() != 0,
            Value::Bool(b) => *b,
            Value::Function(_) => false,
            Value::None => false,
        }
    }

    fn or_cond(&mut self, expr: &OrExpression) -> bool {
        let val = self.logical_cond(&expr.left, &expr.right);
        
        val.0 || val.1
    }

    fn and_cond(&mut self, expr: &AndExpression) -> bool {
        let val = self.logical_cond(&expr.left, &expr.right);
        
        val.0 && val.1
    }

    fn equal_cond(&mut self, expr: &EqualExpression) -> bool {
        let values = (self.visit_expr(&expr.left), self.visit_expr(&expr.right));

        if let (Value::Object(o1), Value::Object(o2)) = values {
            match expr.operator {
                EqualOperator::Equal => todo!(),
                EqualOperator::NotEqual => todo!(),
            }
        }

        if let (Value::Number(n1), Value::Number(n2)) = values {
            match expr.operator {
                EqualOperator::Equal => return n1.eq(&n2),
                EqualOperator::NotEqual => return !n1.eq(&n2),
            }
        }

        if let (Value::String(s1), Value::String(s2)) = values {
            match expr.operator {
                EqualOperator::Equal => return s1 == s2,
                EqualOperator::NotEqual => return  s1 != s2,
            }
        }

        //TODO: WARING, Comparing different type
        false
    }

    fn equal_object(left: &Object, right: &Object, operator: &EqualOperator) -> bool{
        //TODO: try to call function less


        // match operator {
            
        // }

        // //do the deep equal
        // for elem in left.fields {
        //     match rig
        // }
        false
    }

    // this will call function less on left object 
    fn compare_object(&mut self, left: ObjectRef, right: ObjectRef, operator: &RelationOperator) -> bool {
        false
    }

    fn relational_cond(&mut self, expr: &RelationalExpression) -> bool {
        let values = (self.visit_expr(&expr.left), self.visit_expr(&expr.right));

        if let (Value::Object(o1), Value::Object(o2)) = values {
            return self.compare_object(o1, o2, &expr.operator);
        }

        if let (Value::Number(n1), Value::Number(n2)) = values {
            match expr.operator {
                RelationOperator::Grater => return n1 > n2,
                RelationOperator::GreaterEqual => return n1 >= n2,
                RelationOperator::Less => return n1 < n2,
                RelationOperator::LessEqual => return n1 <= n2,
            }
        }

        if let (Value::String(s1), Value::String(s2)) = values {
            match expr.operator {
                RelationOperator::Grater => return s1 > s1,
                RelationOperator::GreaterEqual => return s1 >= s2,
                RelationOperator::Less => return s1< s2,
                RelationOperator::LessEqual => return s1 <= s2,
            }
        }

        ErrorHandler::fatal(ErrorKind::CompareDifferentTypes);
    }

    fn add_expr(&mut self, expr: &AdditiveExpression) -> Value {
        let values = (self.visit_expr(&expr.left), self.visit_expr(&expr.right));
        
        if let (Value::Number(n1), Value::Number(n2)) = values {
            match expr.operator {
                AdditionOperator::Add => {
                    match n1.checked_add(n2) {
                        Some(d) => return Value::Number(d),
                        None => self.error(ErrorKind::NumberOverflow),
                    }
                },
                AdditionOperator::Subtract => {
                    match n1.checked_sub(n2) {
                        Some(d) => return Value::Number(d),
                        None => self.error(ErrorKind::NumberOverflow),
                    }
                },
            }
        }

        if let (Value::String(s1), Value::String(s2)) = values {
            match expr.operator {
                AdditionOperator::Add => {
                    let s = s1.clone();
                    let other = s2.clone();
                    let added = s + &other;
                    return Value::String(added); //TODO: check WTF  STRING !!!!!!!!!!!
                },
                AdditionOperator::Subtract => self.error(ErrorKind::NotAllowedOperation),
            }
        }

        if let (Value::Object(o1), Value::Object(o2)) = values {
            return Value::Object(self.add_objects(o1, o2));
        }

        self.error(ErrorKind::NotAllowedOperation);
        Value::Number(Decimal::new(0, 0))
    }

    fn mul_expr(&mut self, expr: &MultiplicativeExpression) -> Value {
        let values = (self.visit_expr(&expr.left), self.visit_expr(&expr.right));
        
        if let (Value::Number(n1), Value::Number(n2)) = values {
            match expr.operator {
                MultiplicationOperator::Multiplication => {
                    match n1.checked_mul(n2) {
                        Some(d) => return Value::Number(d),
                        None => self.error(ErrorKind::NumberOverflow),
                    }
                },
                MultiplicationOperator::Division => {
                    match n1.checked_div(n2) {
                        Some(d) => return Value::Number(d),
                        None => self.error(ErrorKind::NumberOverflow),
                    }
                },
                MultiplicationOperator::Modulo => {
                    match n1.checked_rem(n2) {
                        Some(d) => return Value::Number(d),
                        None => self.error(ErrorKind::NumberOverflow),
                    }
                },
            }
        }

        if let (Value::String(s1), Value::String(s2)) = values {
            match expr.operator {
                MultiplicationOperator::Multiplication => todo!(),
                MultiplicationOperator::Division => self.error(ErrorKind::NotAllowedOperation),
                MultiplicationOperator::Modulo => self.error(ErrorKind::NotAllowedOperation),
            }
        }

        if let (Value::Object(o1), Value::Object(o2)) = values {
            self.error(ErrorKind::NotAllowedOperation)
            //TODO: do it ?
        }

        Value::Number(Decimal::new(0, 0))
    }


    fn unary_expr(&mut self, expr: &NotExpression) -> Value {
        let left = self.visit_expr(&expr.expression);

        match left {
            Value::Object(o) => {
                //TODO: NONE CHECK
                Value::Bool(false)
            },
            Value::Number(n) => {
                if n.eq(&Decimal::new(0, 0)) {
                    return Value::Bool(true); 
                } else {
                    Value::Bool(false)
                }
            },
            Value::String(s) => {
                if s.len() == 0 {
                    return Value::Bool(true); 
                } else {
                    Value::Bool(false)
                }
            },
            Value::Bool(b) => {
                return Value::Bool(!b);
            },
            Value::Function(_) => {
                self.error(ErrorKind::NotAllowedOperation);
                Value::Bool(true)
            },
            Value::None =>{
                return Value::Bool(true);
            },
        }
    } 

    fn has_expr(&mut self, expr: &HasExpression) -> Value {
        let obj = self.visit_expr(&expr.expression);
        if let Value::Object(o) = obj {
            return Value::Bool((*o).borrow_mut().has(expr.ident.clone()));
        }  else {
            self.error(ErrorKind::ObjectExpected);
            Value::Bool(false)
        }
    }

    //TODO ? use rc here ?
    fn variable_expr(&mut self, expr: &VariableExpression) -> Value {
        let mut val = Value::None;
        for elem in expr.path.clone() {
            if let Some(arguments) = elem.arguments {
                match val {
                    Value::Object(o) => {
                        if let Some(rc) =  (*o).borrow_mut().access_field(&elem.name) {
                            if let Value::Function(f) = rc {
                                val = self.user_function_call(&f, Some(arguments));
                            } else {
                                self.error(ErrorKind::NotCallable);
                            }
                        } else {
                            self.error(ErrorKind::NoField {  })
                        }
                    },
                    Value::Number(_) => todo!(),
                    Value::String(_) => todo!(),
                    Value::Bool(_) => todo!(),
                    Value::Function(_) => {self.error(ErrorKind::AccessOnFunction)},
                    Value::None => {
                        self.error(ErrorKind::AccessNone);
                    },
                }
                //TODO: functions on build - in types ?
                /*
                    function on string: len
                */
            } else {
                match val {
                    Value::Object(o) => {
                        match (*o).borrow_mut().access_field(&elem.name) {
                            Some(v) => {val = v},
                            None => {self.error(ErrorKind::NoField {  })},
                        }
                    },
                    Value::Number(_) => {self.error(ErrorKind::AccessOnNumber)},
                    Value::String(_) => {self.error(ErrorKind::AccessOnString)},
                    Value::Bool(_) => {self.error(ErrorKind::AccessOnBool)},
                    Value::Function(_) => {self.error(ErrorKind::AccessOnFunction)},
                    Value::None => {
                        match self.current_context().find_var_in_scopes(&elem.name) {
                            Some(v) => val = v.clone(),
                            None => todo!(),
                        }

                    },
                }
            }
        }
        
        val
    }

    fn assignment_expr(&mut self, expr: &AssignmentExpression) -> Value {
        //let left;
        //let right = self.visit_expr(&expr.right);

        // if let Expression::VariableExpression(e) = *expr.left {
        //     //TODO: len 0 ?????
        //     let name = e.path[0].name;
        //     if e.path.len() == 1 {
        //         match self.current_context().find_var_in_scopes(&name) {
        //             Some(o) => {
        //                 match expr.operator {
        //                     AssignmentOperator::Assignment => {
        //                         self.current_context().update_var_in_scopes(&name, &right);
        //                     },
        //                     AssignmentOperator::AddAssignment => {self.add_assignment(&name, &left, &right)},
        //                     AssignmentOperator::SubtractAssignment => {self.subtract_assignment(&name, &left, &right)},
        //                     AssignmentOperator::MultiplicationAssignment => {self.mul_assignment(&name, &left, &right)},
        //                     AssignmentOperator::DivisionAssignment => {self.div_assignment(&name, &left, &right)},
        //                     AssignmentOperator::ModuloAssignment => {self.modulo_assignment(&name, &left, &right)},
        //                 }
        //             },
        //             None => {
        //                 if let AssignmentOperator::Assignment = expr.operator {
        //                     left = *self.get_scopes().back().unwrap().insert_var(&name, right);
        //                 } else {
        //                     self.error(ErrorKind::NotDefined);
        //                     return Value::None;
        //                 }
        //             },
        //         }
        //     } else {
        //         //len can be > 2, can do this safely
        //         let last = e.path.remove(e.path.len()-1).unwrap();
        //         if let Some(_) = last.arguments {
        //             self.error(ErrorKind::NotCallable);
        //             return Value::None;
        //         }

        //         left = *self.variable_expr(&e);
        //         if let Value::Object(o) = left {
        //             match expr.operator {
        //                 AssignmentOperator::Assignment => {
        //                     o.add_field(&last.name, right);
        //                 },
        //                 AssignmentOperator::AddAssignment => {
        //                     self.add_assignment_object(&last.name, &o, &right);
        //                 },
        //                 AssignmentOperator::SubtractAssignment => todo!(),
        //                 AssignmentOperator::MultiplicationAssignment => todo!(),
        //                 AssignmentOperator::DivisionAssignment => todo!(),
        //                 AssignmentOperator::ModuloAssignment => todo!(),
        //             }
        //         } else {
        //             self.error(ErrorKind::NotAssignable);
        //             return Value::None;    
        //         }
        //     }
        // } else {
        //     self.error(ErrorKind::UnexpectedExpression);
        //     return Value::None;
        // }
        
    
        //left
        Value::None
    }

    fn add_assignment_object(&mut self, name: &String, left: ObjectRef, right: &Value) {
        let mut obj = (*left).borrow_mut();
        match obj.access_field(name) {
            Some(v) => {
                match v {
                    Value::Object(o1) => {
                        if let Value::Object(o2) = right {
                            obj.add_field(name, Value::Object(self.add_objects(o1, Rc::clone(o2))));
                        } else {
                            self.error(ErrorKind::BadType)
                        }
                    },
                    Value::Number(n1) => {
                        if let Value::Number(n2) = right {
                            match n1.checked_add(*n2) {
                                Some(n) => obj.add_field(name, Value::Number(n)),
                                None => self.error(ErrorKind::BadType),
                            }
                        } else {
                            self.error(ErrorKind::BadType)
                        }
                    },
                    Value::String(s1) => {
                        if let Value::String(s2) = right {
                            let s = s1.clone();
                            let other = s2.clone();
                            let added = s + &other;
                            obj.add_field(name, Value::String(added));
                        } else {
                            self.error(ErrorKind::BadType)
                        }
                    },
                    Value::Bool(_) => {self.error(ErrorKind::BadType)},
                    Value::Function(_) => {self.error(ErrorKind::BadType)},
                    Value::None => {
                        self.error(ErrorKind::BadType)
                    },
                }
            },
            None => todo!(),
        }   
    }

    fn add_assignment(&mut self, name: &String, left: &Value, right: &Value) {
        let values = (left, right);

        if let (Value::Object(o1), Value::Object(o2))  = values {
            let added = self.add_objects(Rc::clone(o1), Rc::clone(o2));
            self.current_context().update_var_in_scopes(name, Value::Object(added));
            return;
        }

        if let (Value::String(s1), Value::String(s2)) = values {
            let s = s1.clone();
            let other = s2.clone();
            let added = s + &other;
            self.current_context().update_var_in_scopes(name, Value::String(added));
            return;
        }

        if let (Value::Number(n1), Value::Number(n2)) = values {
            match n1.checked_add(*n2) {
                Some(n) => self.current_context().update_var_in_scopes(name, Value::Number(n)),
                None => self.error(ErrorKind::NumberOverflow),
            }
            self.current_context().update_var_in_scopes(name, Value::Number((*n1)+n2));
            return;
        }

        self.error(ErrorKind::NotAllowedOperation);
    }

    fn subtract_assignment(&mut self, name: &String, left: &Value, right: &Value) {
        let values = (left, right);

        if let (Value::Number(n1), Value::Number(n2)) = values {
            match n1.checked_sub(*n2) {
                Some(n) => self.current_context().update_var_in_scopes(name, Value::Number(n)),
                None => self.error(ErrorKind::NumberOverflow),
            }
            self.current_context().update_var_in_scopes(name, Value::Number((*n1)+n2));
        } else {
            self.error(ErrorKind::NotAllowedOperation);
        }
    }

    fn mul_assignment(&mut self, name: &String, left: &Value, right: &Value) {
        let values = (left, right);
        if let (Value::Number(n1), Value::Number(n2)) = values {
            match n1.checked_mul(*n2) {
                Some(n) => self.current_context().update_var_in_scopes(name, Value::Number(n)),
                None => self.error(ErrorKind::NumberOverflow),
            }
            self.current_context().update_var_in_scopes(name, Value::Number((*n1)+n2));
        } else {
            self.error(ErrorKind::NotAllowedOperation);
        }
    }

    fn div_assignment(&mut self, name: &String, left: &Value, right: &Value) {
        let values = (left, right);
        if let (Value::Number(n1), Value::Number(n2)) = values {
            match n1.checked_div(*n2) {
                Some(n) => self.current_context().update_var_in_scopes(name, Value::Number(n).clone()),
                None => self.error(ErrorKind::NumberOverflow),
            }
            self.current_context().update_var_in_scopes(name, Value::Number((*n1)+n2));
        } else {
            self.error(ErrorKind::NotAllowedOperation);
        }
    }

    fn modulo_assignment(&mut self, name: &String, left: &Value, right: &Value) {
        let values = (left, right);
        if let (Value::Number(mut n1), Value::Number(mut n2)) = values {
            match n1.checked_rem(n2) {
                Some(n) => self.current_context().update_var_in_scopes(name, Value::Number(n.clone())),
                None => self.error(ErrorKind::NumberOverflow),
            }
            self.current_context().update_var_in_scopes(name, Value::Number((n1)+n2));
        } else {
            self.error(ErrorKind::NotAllowedOperation);
        }
    }

    fn add_objects(&mut self, left: ObjectRef, right: ObjectRef) -> ObjectRef {
        let mut copy = Rc::new(RefCell::new((*left).borrow_mut().clone()));
        let rightRef:ObjectRef = Rc::clone(&right);
        for field in (*rightRef).borrow_mut().fields {
            //TODO: DEEEP COPTY?
            (*copy).borrow_mut().add_field(&field.0, field.1)
        }

        copy
    }

    fn error(&mut self, err: ErrorKind) {
        ErrorHandler::fatal(err)
    }

    fn current_context(&mut self) -> &mut FunCallContext {
        return self.stack.back_mut().unwrap();
    }

    fn new_scope(&mut self) {
        todo!()
    }

    


    fn user_function_call(&mut self, n: &Function, arguments: Option<VecDeque<Argument>>) -> Value {
        let mut new_call = FunCallContext{
            name: n.name.to_string(),
            scopes: VecDeque::new(),
            cur_return: Value::None,
            returnable: false
        };

        let mut base_scope:Scope = Scope::new();

        if let Some(args) = arguments {
            for (par, arg) in zip(n.parameters.clone(), args) {
                base_scope.insert_var(&par.name.to_string(), self.visit_argument(&arg));
            }
        };

        new_call.scopes.push_back(base_scope);
        
        self.stack.push_back(new_call.clone());

        self.visit_block(&n.block);

        self.stack.remove(self.stack.len()-1);

        //TODO: possible problem with refrences
        let to_ret = new_call.cur_return; 
        to_ret
    }

}