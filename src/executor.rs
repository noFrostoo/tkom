use std::{
    borrow::{BorrowMut, Borrow},
    cell::RefCell,
    collections::{HashMap, VecDeque},
    iter::zip,
    process::exit,
    rc::Rc,
    str::FromStr,
};

use rust_decimal::{prelude::ToPrimitive, Decimal};

use crate::{
    errors::{ErrorHandler, ErrorKind},
    types::*,
    visitor::Visitor,
};

pub type ObjectRef = Rc<RefCell<Object>>;

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Object(Rc<RefCell<Object>>),
    Number(Decimal),
    String(String),
    Bool(bool),
    Function(String),
    None,
}

impl Value {
    fn to_string(&self) -> String {
        match self {
            Value::Object(o) => (**o).borrow_mut().to_string(),
            Value::Number(n) => n.to_string(),
            Value::String(s) => s.clone(),
            Value::Bool(b) => b.to_string(),
            Value::Function(f) => String::from(format!("#Function: #{}", f)),
            Value::None => "None".to_string(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Object {
    fields: HashMap<String, Value>,
}

impl Object {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
        }
    }

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
    
    pub fn entry(&mut self, name: &String, value: Value) {
        self.fields.insert(name.clone(), value);
    }


    pub fn add_field(&mut self, name: &String, value: Value) {
        match self.fields.get(name) {
            Some(_) => {},
            None => {self.fields.insert(name.clone(), value);},
        }
    }
    
    pub fn update_field(&mut self, name: &String, value: Value) {
        match self.fields.get(name) {
            Some(_) => {self.fields.insert(name.clone(), value);},
            None => {},
        }
    }

    pub fn len(&self) -> usize {
        self.fields.len()
    }

    pub fn to_string(&self) -> String {
        let mut obj_str: VecDeque<String> = VecDeque::new();
        obj_str.push_back("<".to_string());

        for elem in self.fields.borrow() {
            obj_str.push_back(elem.1.to_string());
        }

        obj_str.push_back("<".to_string());
        String::from_iter(obj_str)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Scope {
    values: HashMap<String, Value>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

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
    cur_return: Value,
    returnable: bool,
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
                Some(v) => return Some(&v),
                None => {}
            }
        }

        None
    }

    fn update_var_in_scopes(&mut self, name: &String, value: Value) {
        // can unwrap here since stack can't be empty
        for scope in self.scopes.borrow_mut().into_iter().rev() {
            match scope.get_val(name) {
                Some(_) => {
                    scope.insert_var(name, value);
                    return;
                }
                None => {}
            }
        }
    }

    fn drop_scope(&mut self) {
        self.scopes.remove(self.scopes.len() - 1);
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

const MAIN_FUNC_NAME: &str = "main";

pub struct Executor {
    functions: HashMap<String, Function>,
    stack: VecDeque<FunCallContext>,
    standard_lib: HashMap<String, fn(&mut Executor, VecDeque<Argument>) -> Value>,
    last_ret: Value,
}

impl Executor {
    pub fn new() -> Self {
        let mut s = Self {
            functions: HashMap::new(),
            stack: VecDeque::new(),
            standard_lib: HashMap::new(),
            last_ret: Value::None,
        };
        s.init_std_lib();
        s
    }
}

impl Visitor<Value> for Executor {
    fn visit_program(&mut self, n: &Program) -> Value {
        self.functions = n.functions.clone();
        self.last_ret = self.visit_function_call(MAIN_FUNC_NAME.to_string(), VecDeque::new());
        return self.last_ret.clone();
    }

    fn visit_function_call(&mut self, name: String, arguments: VecDeque<Argument>) -> Value {
        let value;
        //todo std jako normalne 
        if let Some(func) = self.standard_lib.get(&name) {
            value = func(self, arguments);
        } else {
            value = self.user_function_call(name, arguments);
        }

        value
    }

    fn visit_argument(&mut self, n: &Argument) -> Value {
        self.visit_expr(&n.expr)
    }

    fn visit_block(&mut self, n: &Block) -> Value{
        self.current_context().new_scope();

        for stmt in n.statements.to_owned() {
            match stmt {
                Statement::Expression(e) => {
                    self.visit_expr(&e);
                }
                Statement::If(i) => {self.visit_if(&i);},
                Statement::For(f) => {self.visit_for(&f);},
                Statement::While(w) => {self.visit_while(&w);},
                Statement::Return(r) => {self.visit_return(&r);},
            };

            if self.current_context().returnable {
                self.current_context().drop_scope();
                return Value::None;
            }
        }

        self.current_context().drop_scope();
        Value::None
    }

    fn visit_expr(&mut self, e: &Expression) -> Value {
        match e {
            Expression::OrExpression(e) => self.visit_or_expression(e),
            Expression::AndExpression(e) => self.visit_and_expression(e),
            Expression::EqualExpression(e) => self.visit_equal_expression(e),
            Expression::RelationalExpression(e) => self.visit_relation_expression(e),
            Expression::AdditiveExpression(e) => self.visit_additive_expression(e),
            Expression::MultiplicativeExpression(e) => self.visit_multiplicative_expression(e),
            Expression::UnaryExpression(e) => self.visit_unary_expression(e),
            Expression::HasExpression(e) => self.visit_has_expression(e),
            Expression::StringLiteral(s) => Value::String(s.clone()),
            Expression::Number(d) => Value::Number(*d),
            Expression::VariableExpression(e) => (self.visit_variable_expression(e).clone()),
            Expression::AssignmentExpression(e) => self.visit_assignment_expression(&e),
        }
    }

    fn visit_if(&mut self, e: &If) -> Value {
        let cond_expr = e.condition.borrow();
        let cond;
        match cond_expr {
            Some(c) => {
                let expr = self.visit_expr(&c);
                cond = self.value_to_bool(&expr);
            }
            None => cond = true,
        }

        if cond {
            self.visit_block(&e.block);
        } else {
            match &e.else_block {
                Some(e) => {self.visit_if(&e);},
                None => {}
            }
        }

        Value::None
    }

    fn visit_for(&mut self, e: &For) -> Value {
        let iterator = self.visit_expr(&e.object);
        if let Value::Object(o) = iterator {

            for elem in (*o).borrow_mut().to_owned().fields {
                self.current_context()
                    .scopes
                    .back_mut()
                    .unwrap()
                    .insert_var(&e.iterator, elem.1);
                self.visit_block(&e.block);

                if self.current_context().returnable {
                    return Value::None;
                }
            }
        } else if let Value::String(s) = iterator {
            
            for ch in s.chars() {
                self.current_context()
                    .scopes
                    .back_mut()
                    .unwrap()
                    .insert_var(&e.iterator, Value::String(ch.to_string()));
                self.visit_block(&e.block);

                if self.current_context().returnable {
                    return Value::None;
                }
            }
        } else {
            self.error(ErrorKind::NotIterable);
        }

        Value::None
    }

    fn visit_while(&mut self, e: &While) -> Value {
        let mut expr = self.visit_expr(&e.condition);
        let mut value = self.value_to_bool(&expr);

        if !value {
            return Value::None;
        }

        expr = self.visit_expr(&e.condition);
        value = self.value_to_bool(&expr);
        while value {
            self.visit_block(&e.block);
            if self.current_context().returnable {
                return Value::None;
            }
            //TODO: move to while()
            expr = self.visit_expr(&e.condition);
            value = self.value_to_bool(&expr);
        }

        Value::None
    }

    fn visit_return(&mut self, r: &Return) -> Value {
        self.current_context().returnable = true;
        match &r.expression {
            Some(e) => {
                let return_val = self.visit_expr(e);
                self.current_context().cur_return = return_val.clone();
                return_val
            }
            None => Value::None
        }
    }

    fn visit_or_expression(&mut self, expr: &OrExpression) -> Value {
        let val = self.logical_cond(&expr.left, &expr.right);

        Value::Bool(val.0 || val.1)
    }

    fn visit_and_expression(&mut self, expr: &AndExpression) -> Value {
        let val = self.logical_cond(&expr.left, &expr.right);

        Value::Bool(val.0 && val.1)
    }

    fn visit_equal_expression(&mut self, expr: &EqualExpression) -> Value {
        let values = (self.visit_expr(&expr.left), self.visit_expr(&expr.right));

        if let (Value::Number(n1), Value::Number(n2)) = values {
            match expr.operator {
                EqualOperator::Equal => return Value::Bool(n1.eq(&n2)),
                EqualOperator::NotEqual => return Value::Bool(!n1.eq(&n2)),
            }
        }

        if let (Value::String(s1), Value::String(s2)) = values {
            match expr.operator {
                EqualOperator::Equal => return Value::Bool(s1 == s2),
                EqualOperator::NotEqual => return Value::Bool(s1 != s2),
            }
        }

        Value::Bool(false)
    }

    fn visit_relation_expression(&mut self, expr: &RelationalExpression) -> Value {
        let values = (self.visit_expr(&expr.left), self.visit_expr(&expr.right));

        if let (Value::Number(n1), Value::Number(n2)) = values {
            match expr.operator {
                RelationOperator::Grater => return Value::Bool(n1 > n2),
                RelationOperator::GreaterEqual => return Value::Bool(n1 >= n2),
                RelationOperator::Less => return Value::Bool(n1 < n2),
                RelationOperator::LessEqual => return Value::Bool(n1 <= n2),
            }
        }

        if let (Value::String(s1), Value::String(s2)) = values {
            match expr.operator {
                RelationOperator::Grater => return Value::Bool(s1 > s2),
                RelationOperator::GreaterEqual => return Value::Bool(s1 >= s2),
                RelationOperator::Less => return Value::Bool(s1 < s2),
                RelationOperator::LessEqual => return Value::Bool(s1 <= s2),
            }
        }

        ErrorHandler::fatal(ErrorKind::CompareDifferentTypes);
    }

    fn visit_additive_expression(&mut self, expr: &AdditiveExpression) -> Value {
        let values = (self.visit_expr(&expr.left), self.visit_expr(&expr.right));

        if let (Value::Object(o1), Value::Object(o2)) = values {
            return Value::Object(self.add_objects(o1, o2));
        }

        if let (Value::Number(n1), Value::Number(n2)) = values {
            match expr.operator {
                AdditionOperator::Add => match n1.checked_add(n2) {
                    Some(d) => return Value::Number(d),
                    None => self.error(ErrorKind::NumberOverflow),
                },
                AdditionOperator::Subtract => match n1.checked_sub(n2) {
                    Some(d) => return Value::Number(d),
                    None => self.error(ErrorKind::NumberOverflow),
                },
            }
        }

        if let (Value::String(s1), Value::String(s2)) = values {
            match expr.operator {
                AdditionOperator::Add => {
                    let s = s1.clone();
                    let other = s2.clone();
                    let added = s + &other;
                    return Value::String(added);
                }
                AdditionOperator::Subtract => self.error(ErrorKind::NotAllowedOperation),
            }
        }

        self.error(ErrorKind::NotAllowedOperation);
        Value::Number(Decimal::new(0, 0))
    }

    fn visit_multiplicative_expression(&mut self, expr: &MultiplicativeExpression) -> Value {
        let values = (self.visit_expr(&expr.left), self.visit_expr(&expr.right));

        if let (Value::Number(n1), Value::Number(n2)) = values {
            match expr.operator {
                MultiplicationOperator::Multiplication => match n1.checked_mul(n2) {
                    Some(d) => return Value::Number(d),
                    None => self.error(ErrorKind::NumberOverflow),
                },
                MultiplicationOperator::Division => match n1.checked_div(n2) {
                    Some(d) => return Value::Number(d),
                    None => self.error(ErrorKind::NumberOverflow),
                },
                MultiplicationOperator::Modulo => match n1.checked_rem(n2) {
                    Some(d) => return Value::Number(d),
                    None => self.error(ErrorKind::NumberOverflow),
                },
            }
        }

        self.error(ErrorKind::NotAllowedOperation);
        Value::None
    }

    fn visit_unary_expression(&mut self, expr: &NotExpression) -> Value {
        let left = self.visit_expr(&expr.expression);

        match left {
            Value::Object(_) => {
                Value::Bool(true)
            }
            Value::Number(n) => {
                if n.eq(&Decimal::new(0, 0)) {
                    return Value::Bool(true);
                } else {
                    Value::Bool(false)
                }
            }
            Value::String(s) => {
                if s.len() == 0 {
                    return Value::Bool(true);
                } else {
                    Value::Bool(false)
                }
            }
            Value::Bool(b) => {
                return Value::Bool(!b);
            }
            Value::Function(_) => {
                self.error(ErrorKind::NotAllowedOperation);
                Value::Bool(true)
            }
            Value::None => {
                return Value::Bool(true);
            }
        }
    }

    fn visit_has_expression(&mut self, expr: &HasExpression) -> Value {
        let obj = self.visit_expr(&expr.expression);
        if let Value::Object(o) = obj {
            return Value::Bool((*o).borrow_mut().has(expr.ident.clone()));
        } else {
            self.error(ErrorKind::ObjectExpected);
            Value::Bool(false)
        }
    }

    fn visit_variable_expression(&mut self, expr: &VariableExpression) -> Value {
        let mut val = Value::None;
        for elem in expr.path.clone() {
            if let Some(arguments) = elem.arguments {
                match val.clone() {
                    Value::Object(o) => {
                        if let Some(rc) = (*o).borrow_mut().access_field(&elem.name) {
                            if let Value::Function(f) = rc {
                                val = self.user_function_call(f, arguments);
                            } else {
                                self.error(ErrorKind::NotCallable);
                            }
                        } else {
                            self.error(ErrorKind::NoField {})
                        }
                    }
                    Value::Number(_) => todo!(),
                    Value::String(_) => todo!(),
                    Value::Bool(_) => todo!(),
                    Value::None => {
                        val = self.visit_function_call(elem.name, arguments);
                    }
                    _ => {self.error(ErrorKind::IllegalAccess { on: val.clone(), want: elem.name.clone() })}
                }
                //TODO: functions on build - in types ?
                /*
                    function on string: len
                */
            } else {
                match val.clone() {
                    Value::Object(o) => match (*o).borrow_mut().access_field(&elem.name) {
                        Some(v) => val = v,
                        None => self.error(ErrorKind::NoField {}),
                    },
                    Value::None => {
                        let mut accessed = false;
                        if let Some(_) = self.functions.get(&elem.name) {
                            val = Value::Function(elem.name.clone());
                            accessed = true;
                        }

                        if let Some(v) = self.current_context().find_var_in_scopes(&elem.name) {
                            val = v.clone();
                            accessed = true;
                        }
                        print!("{}", elem.name);
                        //TODO: test Print = Print
                        if !accessed {
                            self.error(ErrorKind::NotDefined);
                        }
                    },
                    _ => {self.error(ErrorKind::IllegalAccess { on: val, want: elem.name.clone() }); return Value::None}
                }
            }
        }

        val
    }

    fn visit_assignment_expression(&mut self, expr: &AssignmentExpression) -> Value {
        let left;
        let right = self.visit_expr(&expr.right);
        //TODO: flaga
        if let Expression::VariableExpression(mut e) = *expr.left.to_owned() {
            let name = e.path[0].name.clone(); 
            if e.path.len() == 1 {
                match self.current_context().find_var_in_scopes(&name).clone() {
                    Some(val) => {
                        left = val.clone();
                        match expr.operator {
                            AssignmentOperator::Assignment => {
                                self.current_context().update_var_in_scopes(&name, right);
                            }
                            AssignmentOperator::AddAssignment => {
                                self.add_assignment(&name, &left, &right)
                            }
                            AssignmentOperator::SubtractAssignment => {
                                self.subtract_assignment(&name, &left, &right)
                            }
                            AssignmentOperator::MultiplicationAssignment => {
                                self.mul_assignment(&name, &left, &right)
                            }
                            AssignmentOperator::DivisionAssignment => {
                                self.div_assignment(&name, &left, &right)
                            }
                            AssignmentOperator::ModuloAssignment => {
                                self.modulo_assignment(&name, &left, &right)
                            }
                        }
                    }
                    None => {
                        if let AssignmentOperator::Assignment = expr.operator {
                                self
                                .current_context()
                                .scopes
                                .back_mut()
                                .unwrap()
                                .insert_var(&name, right.clone());
                                return right;
                        } else {
                            self.error(ErrorKind::NotDefined);
                            return Value::None;
                        };
                    }
                }
            } else {
                //len can be > 2, can do this safely
                let last = e.path.remove(e.path.len() - 1).unwrap();
                if let Some(_) = last.arguments {
                    self.error(ErrorKind::NotCallable);
                    return Value::None;
                }

                left = self.visit_variable_expression(&e);
                if let Value::Object(o) = left.clone() {
                    match expr.operator {
                        AssignmentOperator::Assignment => {
                            (*o).borrow_mut().entry(&last.name, right);
                        }
                        AssignmentOperator::AddAssignment => {
                            self.add_assignment_object(&last.name, o, &right);
                        }
                        _ => {self.error(ErrorKind::NotAllowedOperation)}
                    }
                } else {
                    self.error(ErrorKind::NotAssignable);
                    return Value::None;
                }
            }
        } else {
            self.error(ErrorKind::UnexpectedExpression);
            return Value::None;
        }

        left
    }

    fn visit_string_expression(&mut self, e: &String) -> Value {
        Value::String(e.clone())
    }

    fn visit_number_expression(&mut self, e: &Decimal) -> Value {
        Value::Number(*e)
    }

}

impl Executor {
    fn logical_cond(&mut self, left_expr: &Expression, right_expr: &Expression) -> (bool, bool) {
        let left = self.visit_expr(left_expr);
        let right = self.visit_expr(right_expr);

        return (self.value_to_bool(&left), self.value_to_bool(&right));
    }

    fn value_to_bool(&mut self, val: &Value) -> bool {
        match val {
            Value::Object(o) => (**o).borrow_mut().len() != 0,
            Value::Number(n) => {
                if n.eq(&Decimal::new(0, 0)) {
                    return false;
                }
                n.is_sign_positive()
            }
            Value::String(s) => s.len() != 0,
            Value::Bool(b) => *b,
            Value::Function(_) => false,
            Value::None => false,
        }
    }

    fn add_assignment_object(&mut self, name: &String, left: ObjectRef, right: &Value) {
        let mut obj = (*left).borrow_mut();
        match obj.access_field(name) {
            Some(v) => match v {
                Value::Object(o1) => {
                    if let Value::Object(o2) = right {
                        obj.add_field(name, Value::Object(self.add_objects(o1, Rc::clone(o2))));
                    } else {
                        self.error(ErrorKind::BadType)
                    }
                }
                Value::Number(n1) => {
                    if let Value::Number(n2) = right {
                        match n1.checked_add(*n2) {
                            Some(n) => obj.add_field(name, Value::Number(n)),
                            None => self.error(ErrorKind::NumberOverflow),
                        }
                    } else {
                        self.error(ErrorKind::BadType)
                    }
                }
                Value::String(s1) => {
                    if let Value::String(s2) = right {
                        let s = s1.clone();
                        let other = s2.clone();
                        let added = s + &other;
                        obj.add_field(name, Value::String(added));
                    } else {
                        self.error(ErrorKind::BadType)
                    }
                }
                _ => {self.error(ErrorKind::BadType)}
            },
            None => {self.error(ErrorKind::NoField {  })},
        }
    }

    fn add_assignment(&mut self, name: &String, left: &Value, right: &Value) {
        let values = (left, right);

        if let (Value::Object(o1), Value::Object(o2)) = values {
            let added = self.add_objects(Rc::clone(o1), Rc::clone(o2));
            self.current_context()
                .update_var_in_scopes(name, Value::Object(added));
            return;
        }

        if let (Value::String(s1), Value::String(s2)) = values {
            let s = s1.clone();
            let other = s2.clone();
            let added = s + &other;
            self.current_context()
                .update_var_in_scopes(name, Value::String(added));
            return;
        }

        if let (Value::Number(n1), Value::Number(n2)) = values {
            match n1.checked_add(*n2) {
                Some(n) => self
                    .current_context()
                    .update_var_in_scopes(name, Value::Number(n)),
                None => self.error(ErrorKind::NumberOverflow),
            }
            return;
        }

        self.error(ErrorKind::NotAllowedOperation);
    }

    fn subtract_assignment(&mut self, name: &String, left: &Value, right: &Value) {
        let values = (left, right);

        if let (Value::Number(n1), Value::Number(n2)) = values {
            match n1.checked_sub(*n2) {
                Some(n) => self
                    .current_context()
                    .update_var_in_scopes(name, Value::Number(n)),
                None => self.error(ErrorKind::NumberOverflow),
            }
        } else {
            self.error(ErrorKind::NotAllowedOperation);
        }
    }

    fn mul_assignment(&mut self, name: &String, left: &Value, right: &Value) {
        let values = (left, right);
        if let (Value::Number(n1), Value::Number(n2)) = values {
            match n1.checked_mul(*n2) {
                Some(n) => self
                    .current_context()
                    .update_var_in_scopes(name, Value::Number(n)),
                None => self.error(ErrorKind::NumberOverflow),
            }
        } else {
            self.error(ErrorKind::NotAllowedOperation);
        }
    }

    fn div_assignment(&mut self, name: &String, left: &Value, right: &Value) {
        let values = (left, right);
        if let (Value::Number(n1), Value::Number(n2)) = values {
            match n1.checked_div(*n2) {
                Some(n) => self
                    .current_context()
                    .update_var_in_scopes(name, Value::Number(n)),
                None => self.error(ErrorKind::NumberOverflow),
            }
        } else {
            self.error(ErrorKind::NotAllowedOperation);
        }
    }

    fn modulo_assignment(&mut self, name: &String, left: &Value, right: &Value) {
        let values = (left, right);
        if let (Value::Number(n1), Value::Number(mut n2)) = values {
            match n1.checked_rem(n2) {
                Some(n) => self
                    .current_context()
                    .update_var_in_scopes(name, Value::Number(n)),
                None => self.error(ErrorKind::NumberOverflow),
            }
        } else {
            self.error(ErrorKind::NotAllowedOperation);
        }
    }

    fn add_objects(&mut self, left: ObjectRef, right: ObjectRef) -> ObjectRef {
        let copy = Rc::new(RefCell::new((*left).borrow_mut().clone()));
        let right_ref: ObjectRef = Rc::clone(&right);
        for field in (*right_ref).borrow_mut().to_owned().fields {
            (*copy).borrow_mut().add_field(&field.0, field.1)
        }

        copy
    }

    fn error(&mut self, err: ErrorKind) {
        //TODO: stack trace
        ErrorHandler::fatal(err)
    }

    fn current_context(&mut self) -> &mut FunCallContext {
        return self.stack.back_mut().unwrap();
    }

    fn user_function_call(&mut self, name: String, arguments: VecDeque<Argument>) -> Value {
        let func = match self.functions.get(&name) {
            Some(f) => f.clone(),
            None => {
                self.error(ErrorKind::UnknownFunction);
                return Value::None;
            }
        };

        if arguments.len() != func.parameters.len() {
            self.error(ErrorKind::MismatchedArgumentsLen {
                expected: func.parameters.len(),
                got: arguments.len(),
                function: func.name.clone(),
            });
        }

        let mut new_call = FunCallContext {
            name: name.clone(),
            scopes: VecDeque::new(),
            cur_return: Value::None,
            returnable: false,
        };
        
        let mut base_scope: Scope = Scope::new();

        for (par, arg) in zip(func.parameters, arguments) {
            base_scope.insert_var(&par.name.to_string(), self.visit_argument(&arg));
        }

        new_call.scopes.push_back(base_scope);

        self.stack.push_back(new_call);

        self.visit_block(&func.block);

        let to_ret = self.current_context().cur_return.clone();

        self.stack.remove(self.stack.len() - 1);
        to_ret
    }

    fn init_std_lib(&mut self) {
        self.standard_lib
            .insert("Print".to_string(), Executor::print);
        self.standard_lib
            .insert("Input".to_string(), Executor::input);
        self.standard_lib
            .insert("Input_number".to_string(), Executor::input_number);
        self.standard_lib
            .insert("String".to_string(), Executor::convert_to_string);
        self.standard_lib
            .insert("Numeric".to_string(), Executor::convert_to_number);
        self.standard_lib.insert("Exit".to_string(), Executor::exit);
        self.standard_lib
            .insert("Object".to_string(), Executor::new_object);
    }

    fn print(&mut self, arguments: VecDeque<Argument>) -> Value {
        for arg in arguments {
            let val = self.visit_argument(&arg);
            print!("{}", val.to_string());
        }
        Value::None
    }

    fn input(&mut self, _: VecDeque<Argument>) -> Value {
        use std::io::stdin;
        let mut s = String::new();
        stdin()
            .read_line(&mut s)
            .expect("Did not enter a correct string");
        if let Some('\n') = s.chars().next_back() {
            s.pop();
        }
        if let Some('\r') = s.chars().next_back() {
            s.pop();
        }
        return Value::String(s);
    }

    fn input_number(&mut self, _: VecDeque<Argument>) -> Value {
        use std::io::stdin;
        let mut s = String::new();
        stdin()
            .read_line(&mut s)
            .expect("Did not enter a correct number");
        match Decimal::from_str(&(*s)) {
            Ok(d) => Value::Number(d),
            Err(e) => {
                self.error(ErrorKind::BadInputNumber {
                    err_msg: e.to_string(),
                });
                return Value::None;
            }
        }
    }

    fn new_object(&mut self, _: VecDeque<Argument>) -> Value {
        Value::Object(Rc::new(RefCell::new(Object::new())))
    }

    fn convert_to_string(&mut self, arguments: VecDeque<Argument>) -> Value {
        if 1 != arguments.len() {
            self.error(ErrorKind::MismatchedArgumentsLen {
                expected: 1,
                got: arguments.len(),
                function: "String".to_string(),
            });
            return Value::None;
        }

        let value = self.visit_argument(&arguments[0]);

        return Value::String(value.to_string());
    }

    fn convert_to_number(&mut self, arguments: VecDeque<Argument>) -> Value {
        if 1 != arguments.len() {
            self.error(ErrorKind::MismatchedArgumentsLen {
                expected: 1,
                got: arguments.len(),
                function: "Numeric".to_string(),
            });
            return Value::None;
        }

        let value = self.visit_argument(&arguments[0]);

        return Value::String(value.to_string());
    }

    fn exit(&mut self, arguments: VecDeque<Argument>) -> Value {
        if 1 != arguments.len() {
            let val = self.visit_argument(&arguments[0]);
            if let Value::Number(d) = val {
                match d.to_i32() {
                    Some(i) => exit(i),
                    None => exit(-99),
                }
            }
        }
        exit(-99);
    }

    fn delete(&mut self, arguments: VecDeque<Argument>) -> Value {
        if 1 != arguments.len() {
            self.error(ErrorKind::MismatchedArgumentsLen {
                expected: 1,
                got: arguments.len(),
                function: "Numeric".to_string(),
            });
            return Value::None;
        }

        let value = self.visit_argument(&arguments[0]);

        match value {
            Value::Object(_) => todo!(),
            Value::Number(_) => todo!(),
            Value::String(_) => todo!(),
            Value::Bool(_) => todo!(),
            Value::Function(_) => todo!(),
            Value::None => todo!(),
        }

        Value::None
    }
}

mod test {

    macro_rules! executor_test {
        (FAIL: $name:ident, $text:expr) => {
            #[test]
            #[should_panic]
            fn $name() {
                let text: &str = $text;

                set_up_test(text);
            }
        };
        ($name:ident, $text:expr, $result:expr) => {
            #[test]
            fn $name() {
                let text: &str = $text;
                let should_be: Value = $result;

                let result = set_up_test(text);
                assert_eq!(result, should_be);
            }
        };
    }

    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    use rust_decimal::Decimal;

    use crate::{
        executor::{Object, Value},
        file_handler,
        lexer::Lexer,
        parser::Parser,
        visitor::Visitor,
    };

    use super::Executor;

    fn set_up_test(text: &str) -> Value {
        let test_source = file_handler::TestSource::new(String::from(text), 0);
        let lex = Lexer::new(Box::new(test_source));
        let mut parser = Parser::new(Box::new(lex), false);
        let program = parser.parse();
        print!("{:?}", program);
        let mut executor = Executor::new();
        executor.visit_program(&program)
    }

    executor_test!(basic_test, "main(){  }", Value::None);
    executor_test!(
        basic_test2,
        "main(){  return 1 + 2;  }",
        Value::Number(Decimal::new(3, 0))
    );
    executor_test!(
        basic_test3,
        "main(){ x = 5; x = 3 + 8 ; return x;  }",
        Value::Number(Decimal::new(11, 0))
    );
    executor_test!(
        basic_test4,
        "main(){ x = \"tests\"; x = 3 + 8 ; return x;  }",
        Value::Number(Decimal::new(11, 0))
    );
    executor_test!(
        basic_test5,
        "main(){ x = \"tests\"; x += \"5\" ; return x;  }",
        Value::String("tests5".to_string())
    );
    executor_test!(
        if_test,
        "main(){ x = \"xx\" ; if (x) { x = 4; } return x; }",
        Value::Number(Decimal::new(4, 0))
    );
    executor_test!(
        if_test2,
        "main(){ x = 0; if (x) { x = 4; } else { x = 5 ;} return x; }",
        Value::Number(Decimal::new(5, 0))
    );
    executor_test!(
        if_test3,
        "main(){ 
        x = 0; if (x) {
             x = 4; 
        } else if (x == -1) { 
            x = 5 ;
        } else if (x == -2) {
             x = 1; 
        } else if (x == 0) {
             x = 2;
        } else { 
            x = 7; 
        } 
         return x; 
        }",
        Value::Number(Decimal::new(2, 0))
    );
    executor_test!(
        object_test1,
        "main(){ x = Object(); return x; }",
        Value::Object(Rc::new(RefCell::new(Object::new())))
    );
    executor_test!(
        object_test2,
        "main(){ x = Object();  x.val = 1;  return x; }",
        Value::Object(Rc::new(RefCell::new(Object {
            fields: HashMap::from([("val".to_string(), Value::Number(Decimal::new(1, 0)))])
        })))
    );
    executor_test!(
        object_test3,
        "main(){ x = Object();  x.val = 1 + 2;  return x; }",
        Value::Object(Rc::new(RefCell::new(Object {
            fields: HashMap::from([("val".to_string(), Value::Number(Decimal::new(3, 0)))])
        })))
    );
    executor_test!(
        object_test4,
    "main(){ x = Object(); x.val = 1; y = x.val + 1; return y;  }",
    Value::Number(Decimal::new(2, 0))
    );
    executor_test!(
        logical_test1,
        "main(){ x = 1 < 2; return x; }",
        Value::Bool(true)
    );
    executor_test!(
        logical_test2,
        "main(){ x = 1 > 2; return x; }",
        Value::Bool(false)
    );
    executor_test!(
        logical_test3,
        "main(){ x = 1 <= 2; return x; }",
        Value::Bool(true)
    );
    executor_test!(
        logical_test4,
        "main(){ x = 2 <= 2; return x; }",
        Value::Bool(true)
    );
    executor_test!(
        logical_test5,
        "main(){ x = 1 >= 2; return x; }",
        Value::Bool(false)
    );
    executor_test!(
        logical_test6,
        "main(){ x = 1 == 2; return x; }",
        Value::Bool(false)
    );
    executor_test!(
        logical_test7,
        "main(){ x = 2 == 2; return x; }",
        Value::Bool(true)
    );
    executor_test!(
        logical_test_str1,
        "main(){ x = \"aaa\" < \"abb\"; return x; }",
        Value::Bool(true)
    );
    executor_test!(
        logical_test_str2,
        "main(){ x = \"aaa\" > \"abb\"; return x; }",
        Value::Bool(false)
    );
    executor_test!(
        logical_test_str3,
        "main(){ x = \"aaa\" <= \"abb\"; return x; }",
        Value::Bool(true)
    );
    executor_test!(
        logical_test_str4,
        "main(){ x = \"abb\" <= \"abb\"; return x; }",
        Value::Bool(true)
    );
    executor_test!(
        logical_test_str5,
        "main(){ x = \"aaa\" >= \"abb\"; return x; }",
        Value::Bool(false)
    );
    executor_test!(
        logical_test_str6,
        "main(){ x = \"aaa\" == \"abb\"; return x; }",
        Value::Bool(false)
    );
    executor_test!(
        logical_test_str7,
        "main(){ x = \"abb\" == \"abb\"; return x; }",
        Value::Bool(true)
    );
    executor_test!(
        add_assignment_test,
        "main(){ x = 0; x += 1; return x; }",
        Value::Number(Decimal::new(1,0))
    );
    executor_test!(
        add_assignment_test2,
        "main(){ x = 0; x += -1; return x; }",
        Value::Number(Decimal::new(-1,0))
    );
    executor_test!(
        sub_assignment_test1,
        "main(){ x = 0; x -= 1; return x; }",
        Value::Number(Decimal::new(-1,0))
    );
    executor_test!(
        mul_assignment_test1,
        "main(){ x = 2; x *= 2; return x; }",
        Value::Number(Decimal::new(4,0))
    );
    executor_test!(
        div_assignment_test1,
        "main(){ x = 2; x /= 2; return x; }",
        Value::Number(Decimal::new(1,0))
    );
    executor_test!(
        FAIL: div_assignment_test2,
        "main(){ x = 2; x /= 0; return x; }"
    );
    executor_test!(
        modulo_assignment_test1,
        "main(){ x = 4; x %= 2; return x; }",
        Value::Number(Decimal::new(0,0))
    );
    executor_test!(
        modulo_assignment_test2,
        "main(){ x = 5; x %= 2; return x; }",
        Value::Number(Decimal::new(1,0))
    );
    executor_test!(
        add_test,
        "main(){ x = 2 + 2; return x; }",
        Value::Number(Decimal::new(4,0))
    );
    executor_test!(
        sub_test,
        "main(){ x = 2 - 2; return x; }",
        Value::Number(Decimal::new(0,0))
    );
    executor_test!(
        mul_test,
        "main(){ x = 2 * 2; return x; }",
        Value::Number(Decimal::new(4,0))
    );
    executor_test!(
        div_test,
        "main(){ x = 2 / 2; return x; }",
        Value::Number(Decimal::new(1,0))
    );
    executor_test!(
        mod_test,
        "main(){ x = 4 % 2; return x; }",
        Value::Number(Decimal::new(0,0))
    );
    executor_test!(
        mod_test2,
        "main(){ x = 5 % 2; return x; }",
        Value::Number(Decimal::new(1,0))
    );
    executor_test!(
        for_test,
    "main(){
        x = Object(); 
        x.val = 1; 
        x.val2 = 1;
        x.val3 = 1;
        y = 0;
        for f in x {
            y += f;
        }
        return y;  
    }",
    Value::Number(Decimal::new(3, 0))
    );
    executor_test!(
        for_test2,
    "main(){
        x = \"aaaa\";
        y = \"a\";
        z = 0;
        for f in x {
            y += f;
        }
        return y;  
    }", Value::String("aaaaa".to_string())
    );
    executor_test!(
        FAIL: for_test_fail,
    "main(){
        x = 0;
        y = 0;
        for f in 0 {
            y += 1;
        }
        return y;  
    }"
    );
    executor_test!(
        function_test,
        "doStuff() {  
        x = Object();  
        x.val = 1 + 2;  
        return x; 
     }  
     main(){ 
        y = doStuff();
        return y; 
    }",
        Value::Object(Rc::new(RefCell::new(Object {
            fields: HashMap::from([("val".to_string(), Value::Number(Decimal::new(3, 0)))])
        })))
    );
    executor_test!(
        function_ptr_test,
        "doStuff() {  
     }  
     main(){ 
        y = doStuff;
        return y; 
    }",
        Value::Function("doStuff".to_string())
    );
    executor_test!(
        function_ptr_obj_test,
        "doStuff() {  
        x = Object();  
        x.val = 1 + 2;  
        return x; 
     }  
     main(){ 
        y = Object();
        y.func = doStuff;
        return y; 
    }",
        Value::Object(Rc::new(RefCell::new(Object {
            fields: HashMap::from([("func".to_string(), Value::Function("doStuff".to_string()))])
        })))
    );
    executor_test!(
        function_obj_run_test,
        "doStuff() {  
        x = Object();  
        x.val = 1 + 2;  
        return x; 
     }  
     main(){ 
        y = Object();
        y.func = doStuff;
        t = y.func();
        return t; 
    }",
        Value::Object(Rc::new(RefCell::new(Object {
            fields: HashMap::from([("val".to_string(), Value::Number(Decimal::new(3, 0)))])
        })))
    );
    executor_test!(
        function_obj_run_test2,
        "doStuff(y) {  
        x = Object();  
        x.val = 1 + y;  
        return x; 
     }  
     main(){ 
        y = Object();
        y.func = doStuff;
        t = y.func(5);
        return t; 
    }",
        Value::Object(Rc::new(RefCell::new(Object {
            fields: HashMap::from([("val".to_string(), Value::Number(Decimal::new(6, 0)))])
        })))
    );
    executor_test!(
        while_test,
    "main(){
        i = 0;
        while (i < 10) {
            i += 1;
        }
        return i;  
    }",
    Value::Number(Decimal::new(10, 0))
    );
    executor_test!(
        FAIL: while_test_scope,
    "main(){
        i = 0;
        while (i < 10) {
            i += 1;
            t = 0;
        }
        return t;  
    }"
    );
    executor_test!(function_test2,
        "
    xxx(o) {
        o.other = \"xdd\";
        return o 
    }
        
    doStuff() {  
        x = Object();  
        x.val = 1 + 2;
        xxx(x);  
        return x; 
     }  

     main(){ 
        y = doStuff();
        return y; 
    }",
        Value::Object(Rc::new(RefCell::new(Object {
            fields: HashMap::from([("val".to_string(), Value::Number(Decimal::new(3, 0))),
            ("other".to_string(), Value::String("xdd".to_string()))])
        })))
    );
    executor_test!(factorial,
        "
    factorial(n) {  
        if(n == 0) {
            return 1;
        } else {
            return n * factorial(n - 1);
        }
     }  

     main(){ 
        y = factorial(5);
        return y; 
    }",
    Value::Number(Decimal::new(120, 0))
    );

    executor_test!(
        has_test,
        "main(){ x = Object();  
            x.val = 1; 
            if (x has val) {
                x.val = 2 ;
            }
            return x; }",
        Value::Object(Rc::new(RefCell::new(Object {
            fields: HashMap::from([("val".to_string(), Value::Number(Decimal::new(2, 0)))])
        })))
    );
    executor_test!(
        has_test2, // do not have 
        "main(){ x = Object();  
            x.val = 1; 
            if (x has val2) {
                x.val = 2 ;
            }
            return x; }",
        Value::Object(Rc::new(RefCell::new(Object {
            fields: HashMap::from([("val".to_string(), Value::Number(Decimal::new(1, 0)))])
        })))
    );
    executor_test!(
        FAIL: scope_test_fail,
        "doStuff() { 
         x = Object();  x.val = 1 + 2;  return x;  } 
    main(){ y = doStuff(); y = x + 1;  return y; }"
    );
    executor_test!(
        FAIL: function_not_found,
        "doStuff() { 
         x = Object();  x.val = 1 + 2;  return x;  } 
    main(){ y = notFound(); y = x + 1;  return y; }"
    );
    executor_test!(
        FAIL: function_mismatched_arguments,
        "doStuff(a, b ) {} 
    main(){ y = doStuff(1);  return y; }"
    );
    executor_test!(
        FAIL: not_defined,
    "main(){ x + 1;  return y; }"
    );
    executor_test!(
        FAIL: not_callable,
    "main(){ x = 1; x();  return x; }"
    );
    executor_test!(
        FAIL: no_field,
    "main(){ x = Object(); y = x.val + 1; return y;  }"
    );
    executor_test!(
        func_ptr_fail1,
    "main(){ Print = Print; return Print;  }",
    Value::Function("Print".to_string())
    );
}
