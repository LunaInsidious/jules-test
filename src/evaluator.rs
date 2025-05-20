use crate::ast::{Node, Program, Statement, Expression, Identifier, BlockStatement}; 
use crate::object::{Object, Integer, Class, Instance, FunctionObject, StringObject, BooleanObject}; // Added BooleanObject
use crate::environment::Environment;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

// Constants
const NULL: Object = Object::Null;
// Define TRUE_OBJ and FALSE_OBJ (Step 4 from prompt)
const TRUE_OBJ: Object = Object::Boolean(BooleanObject { value: true });
const FALSE_OBJ: Object = Object::Boolean(BooleanObject { value: false });

// Helper to convert native bool to Mylang Boolean object (Step 4 from prompt)
fn native_bool_to_boolean_object(input: bool) -> Object {
    if input {
        TRUE_OBJ.clone() // Clone to ensure distinct objects if Rc/Arc were used, though consts are copied.
    } else {
        FALSE_OBJ.clone()
    }
}

pub fn eval(node: Box<dyn Node>, env: Rc<RefCell<Environment>>) -> Option<Object> { 
    // Downcast to specific node types
    if let Some(program_node) = node.as_any().downcast_ref::<Program>() {
        return eval_program(program_node, env);
    }
    if let Some(statement_node) = node.as_any().downcast_ref::<Statement>() {
        return eval_statement(statement_node, env);
    }
    if let Some(expression_node) = node.as_any().downcast_ref::<Expression>() {
        return eval_expression(expression_node, env);
    }
    if let Some(block_stmt_node) = node.as_any().downcast_ref::<BlockStatement>() {
        return eval_block_statement(block_stmt_node, env);
    }
    None 
}

fn eval_program(program: &Program, env: Rc<RefCell<Environment>>) -> Option<Object> { 
    let mut result = None; 
    for statement in &program.statements {
        let eval_result = eval_statement(statement, env.clone()); 
        match eval_result {
            Some(Object::ReturnValue(value)) => return Some(*value), 
            Some(Object::Error(msg)) => return Some(Object::Error(msg)), 
            Some(_) => result = eval_result, 
            None => result = None, 
        }
    }
    result 
}

fn eval_block_statement(block: &BlockStatement, env: Rc<RefCell<Environment>>) -> Option<Object> { 
    let mut result = None;
    for statement in &block.statements {
        let eval_result = eval_statement(statement, env.clone()); 
        match eval_result {
            Some(Object::ReturnValue(value)) => return Some(Object::ReturnValue(value)), 
            Some(Object::Error(msg)) => return Some(Object::Error(msg)), 
            Some(_) => result = eval_result,
            None => result = None,
        }
    }
    result
}


fn eval_statement(statement: &Statement, env: Rc<RefCell<Environment>>) -> Option<Object> { 
    match statement {
        Statement::LetStatement { name, value, .. } => {
            eval_let_statement(name, value, env)
        }
        Statement::ExpressionStatement { expression, .. } => {
            eval_expression(expression, env)
        }
        Statement::ClassStatement { name, methods, .. } => { 
            eval_class_statement(name, methods, env)
        }
    }
}

fn eval_class_statement(name_ident: &Identifier, method_nodes: &Vec<crate::ast::FunctionLiteral>, env: Rc<RefCell<Environment>>) -> Option<Object> { 
    let class_name = name_ident.value.clone();
    
    let mut methods_map = HashMap::new();
    for method_literal in method_nodes {
        let func_obj = FunctionObject {
            parameters: method_literal.parameters.clone(),
            body: method_literal.body.clone(),
            env: env.clone(), 
            name: method_literal.name.clone(),
        };
        if let Some(method_name) = &method_literal.name {
            methods_map.insert(method_name.clone(), Rc::new(func_obj));
        } else {
            return Some(Object::Error("Method must have a name".to_string()));
        }
    }

    let class_obj = Object::Class(Rc::new(Class {
        name: class_name.clone(),
        methods: methods_map,
    }));

    env.borrow_mut().set(class_name, class_obj); 
    Some(NULL) 
}


fn eval_let_statement(name: &Identifier, value_expr: &Expression, env: Rc<RefCell<Environment>>) -> Option<Object> { 
    let val = eval_expression(value_expr, env.clone()); 
    match val {
        Some(Object::Error(e)) => return Some(Object::Error(e)), 
        Some(obj_val) => {
            env.borrow_mut().set(name.value.clone(), obj_val); 
            Some(NULL) 
        }
        None => Some(Object::Error(format!("Expression for let {} did not produce a value", name.value))),
    }
}

fn eval_expression(expression: &Expression, env: Rc<RefCell<Environment>>) -> Option<Object> { 
    match expression {
        Expression::IntegerLiteral { value, .. } => {
            Some(Object::Integer(Integer { value: *value }))
        }
        Expression::Identifier(ident_node) => {
            eval_identifier(ident_node, env)
        }
        Expression::StringLiteral { value, .. } => { 
            Some(Object::String(StringObject { value: value.clone() }))
        }
        Expression::ClassInstantiation { name, arguments, .. } => { 
            eval_class_instantiation(name, arguments, env)
        }
        Expression::FunctionLiteral(func_lit_node) => { 
            Some(Object::Function(Rc::new(FunctionObject {
                parameters: func_lit_node.parameters.clone(),
                body: func_lit_node.body.clone(),
                env: env.clone(), 
                name: func_lit_node.name.clone(),
            })))
        }
        Expression::MethodCall { object, name, arguments, .. } => { 
            eval_method_call(object, name, arguments, env)
        }
        Expression::InfixExpression { left, operator, right, .. } => { // Step 1 (eval_expression part)
            let left_val_opt = eval_expression(left, env.clone());
            if let Some(Object::Error(e)) = left_val_opt { return Some(Object::Error(e)); }
            if left_val_opt.is_none() { return Some(Object::Error("left operand evaluated to None".to_string()));} // Should not happen if no errors
            let left_val = left_val_opt.unwrap();

            let right_val_opt = eval_expression(right, env.clone());
            if let Some(Object::Error(e)) = right_val_opt { return Some(Object::Error(e)); }
            if right_val_opt.is_none() { return Some(Object::Error("right operand evaluated to None".to_string()));}
            let right_val = right_val_opt.unwrap();
            
            eval_infix_expression(operator, left_val, right_val)
        }
        // TODO: PrefixExpression, Boolean literals (true, false)
    }
}

// Helper function for evaluating infix expressions (Step 2)
fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Option<Object> {
    match (&left, &right) {
        (Object::Integer(l_int), Object::Integer(r_int)) => {
            eval_integer_infix_expression(operator, l_int.value, r_int.value)
        }
        (Object::String(l_str), Object::String(r_str)) => {
            eval_string_infix_expression(operator, &l_str.value, &r_str.value)
        }
        (Object::Boolean(l_bool), Object::Boolean(r_bool)) => {
            eval_boolean_infix_expression(operator, l_bool.value, r_bool.value)
        }
        _ => { // Type mismatch or unsupported types for the operator
            // Special case: Allow == and != for any types if they are of different types
            // (e.g. Integer vs String should be false for ==, true for !=)
            // However, the prompt specifies these only for same-type operands.
            // For now, strict type matching for operations other than direct pointer comparison for some reference types.
            if left.object_type() != right.object_type() {
                 match operator {
                    "==" => return Some(native_bool_to_boolean_object(false)),
                    "!=" => return Some(native_bool_to_boolean_object(true)),
                    _ => {} // Fall through to type mismatch error
                }
            }
            Some(Object::Error(format!(
                "type mismatch: {} {} {}",
                left.object_type(), operator, right.object_type()
            )))
        }
    }
}

fn eval_integer_infix_expression(operator: &str, left_val: i64, right_val: i64) -> Option<Object> {
    match operator {
        "+" => Some(Object::Integer(Integer { value: left_val + right_val })),
        "-" => Some(Object::Integer(Integer { value: left_val - right_val })),
        "*" => Some(Object::Integer(Integer { value: left_val * right_val })),
        "/" => {
            if right_val == 0 {
                Some(Object::Error("division by zero".to_string()))
            } else {
                Some(Object::Integer(Integer { value: left_val / right_val }))
            }
        }
        "%" => {
            if right_val == 0 {
                Some(Object::Error("modulo by zero".to_string()))
            } else {
                Some(Object::Integer(Integer { value: left_val % right_val }))
            }
        }
        "<" => Some(native_bool_to_boolean_object(left_val < right_val)),
        ">" => Some(native_bool_to_boolean_object(left_val > right_val)),
        "==" => Some(native_bool_to_boolean_object(left_val == right_val)),
        "!=" => Some(native_bool_to_boolean_object(left_val != right_val)),
        "<=" => Some(native_bool_to_boolean_object(left_val <= right_val)),
        ">=" => Some(native_bool_to_boolean_object(left_val >= right_val)),
        _ => Some(Object::Error(format!("unknown operator: INTEGER {} INTEGER", operator))),
    }
}

fn eval_string_infix_expression(operator: &str, left_val: &str, right_val: &str) -> Option<Object> {
    match operator {
        "+" => Some(Object::String(StringObject { value: format!("{}{}", left_val, right_val) })),
        "==" => Some(native_bool_to_boolean_object(left_val == right_val)),
        "!=" => Some(native_bool_to_boolean_object(left_val != right_val)),
        _ => Some(Object::Error(format!("unknown operator: STRING {} STRING", operator))),
    }
}

fn eval_boolean_infix_expression(operator: &str, left_val: bool, right_val: bool) -> Option<Object> {
    match operator {
        "==" => Some(native_bool_to_boolean_object(left_val == right_val)),
        "!=" => Some(native_bool_to_boolean_object(left_val != right_val)),
        _ => Some(Object::Error(format!("unknown operator: BOOLEAN {} BOOLEAN", operator))),
    }
}


fn eval_class_instantiation(class_name_ident: &Identifier, arg_exprs: &Vec<Expression>, env: Rc<RefCell<Environment>>) -> Option<Object> { 
    match env.borrow().get(&class_name_ident.value) { 
        Some(Object::Class(class_rc)) => {
            let _evaluated_args = arg_exprs.iter()
                .map(|arg_expr| eval_expression(arg_expr, env.clone())) 
                .collect::<Vec<_>>();
            
            let instance = Object::Instance(Rc::new(Instance {
                class: class_rc.clone(), 
                fields: Rc::new(RefCell::new(HashMap::new())),
            }));
            Some(instance)
        }
        Some(other_obj) => {
            Some(Object::Error(format!("Expected class '{}', found {}", class_name_ident.value, other_obj.inspect())))
        }
        None => {
            Some(Object::Error(format!("Class '{}' not found.", class_name_ident.value)))
        }
    }
}

fn eval_method_call(object_expr: &Expression, method_name_ident: &Identifier, arg_exprs: &Vec<Expression>, env: Rc<RefCell<Environment>>) -> Option<Object> { 
    let object_val_opt = eval_expression(object_expr, env.clone()); 
    if object_val_opt.is_none() {
        return Some(Object::Error("Failed to evaluate object for method call".to_string()));
    }
    let object_val = object_val_opt.unwrap();
     match &object_val { 
        Object::Error(e) => return Some(Object::Error(e.clone())), 
        _ => {}
    }

    let instance_rc = match &object_val { 
        Object::Instance(inst_rc) => inst_rc.clone(), 
        _ => return Some(Object::Error(format!("Method call on non-instance type: {}", object_val.object_type()))),
    };

    let class_rc = &instance_rc.class; 

    let method_name_str = &method_name_ident.value;
    let func_obj_rc = match class_rc.methods.get(method_name_str) {
        Some(method_rc) => method_rc.clone(),
        None => return Some(Object::Error(format!("Method '{}' not found on class '{}'", method_name_str, class_rc.name))),
    };

    let mut method_env_val = Environment::new_enclosed(func_obj_rc.env.clone()); 
    method_env_val.set("self".to_string(), object_val.clone()); 

    let mut evaluated_args = Vec::new();
    for arg_expr in arg_exprs { 
        match eval_expression(arg_expr, env.clone()) { 
            Some(Object::Error(e)) => return Some(Object::Error(e)),
            Some(arg_val) => evaluated_args.push(arg_val),
            None => return Some(Object::Error("Failed to evaluate argument for method call".to_string())),
        }
    }
    
    if func_obj_rc.parameters.len() != evaluated_args.len() {
        return Some(Object::Error(format!(
            "Method '{}' expected {} arguments, got {}",
            method_name_str,
            func_obj_rc.parameters.len(),
            evaluated_args.len()
        )));
    }
    for (param_ident, arg_val) in func_obj_rc.parameters.iter().zip(evaluated_args.iter()) {
        method_env_val.set(param_ident.value.clone(), arg_val.clone());
    }

    let method_env_rc = Rc::new(RefCell::new(method_env_val));
    let eval_result = eval_block_statement(&func_obj_rc.body, method_env_rc);

    match eval_result {
        Some(Object::ReturnValue(value)) => Some(*value),
        Some(Object::Error(e)) => Some(Object::Error(e)),
        Some(other) => Some(other), 
        None => Some(NULL), 
    }
}


fn eval_identifier(ident: &Identifier, env: Rc<RefCell<Environment>>) -> Option<Object> { 
    match env.borrow().get(&ident.value) { 
        Some(obj) => Some(obj.clone()), 
        None => {
            Some(Object::Error(format!("Identifier '{}' not found.", ident.value)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    
    fn test_eval_program(input: &str) -> Option<Object> {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program_node = parser.parse_program(); 
        let errors = parser.get_errors();
        if !errors.is_empty() {
            eprintln!("Parser errors for input:\n{}\nErrors: {:?}", input, errors);
            panic!("Parser errors: {:?}", errors);
        }

        let env = Rc::new(RefCell::new(Environment::new())); 
        eval(Box::new(program_node), env)
    }

    #[test]
    fn test_eval_integer_expression() {
        let input = "5";
        let expected = Object::Integer(Integer { value: 5 });
        match test_eval_program(input) { 
            Some(obj) => assert_eq!(obj, expected),
            None => panic!("test_eval_program returned None"),
        }
    }

    // ... other tests ...
    #[test]
    fn test_eval_let_statement() {
         match test_eval_program("let x = 5; x;") {
            Some(Object::Integer(val)) => assert_eq!(val.value, 5),
            Some(other) => panic!("Expected Integer, got {:?}", other),
            None => panic!("test_eval_program returned None"),
        }
    }

    #[test]
    fn test_class_definition_and_instantiation() {
        let input = r#" class Point { } let p = Point(); p; "#;
        match test_eval_program(input) {
            Some(Object::Instance(instance_rc)) => {
                assert_eq!(instance_rc.class.name, "Point");
            }
            _ => panic!("Not an instance"),
        }
    }
    
    #[test]
    fn test_method_definition_and_call() {
        let input = r#" class Greeter { fn say_hi() { "hi"; } } let g = Greeter(); g.say_hi(); "#;
        match test_eval_program(input) {
            Some(Object::String(s_obj)) => assert_eq!(s_obj.value, "hi"),
            _ => panic!("Not a string obj"),
        }
    }

    #[test]
    fn test_string_literal_evaluation() {
        match test_eval_program(r#""hello world""#) {
            Some(Object::String(s_obj)) => assert_eq!(s_obj.value, "hello world"),
            _ => panic!("Not a string obj"),
        }
    }
    
    #[test]
    fn test_class_not_found() {
         match test_eval_program("let p = UnknownClass(); p;") {
            Some(Object::Error(e)) => assert!(e.contains("Class 'UnknownClass' not found")),
            _ => panic!("Expected error"),
        }
    }

    #[test]
    fn test_instantiating_non_class() {
        match test_eval_program("let x = 10; let p = x(); p;") {
            Some(Object::Error(e)) => assert!(e.contains("Expected class 'x'")),
            _ => panic!("Expected error"),
        }
    }

    #[test]
    fn test_method_not_found() {
        match test_eval_program("class T{} let t = T(); t.unknown();") {
            Some(Object::Error(e)) => assert!(e.contains("Method 'unknown' not found")),
            _ => panic!("Expected error"),
        }
    }

    // Tests for Infix Expressions
    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            ("5 + 5", Some(Object::Integer(Integer { value: 10 }))),
            ("5 - 5", Some(Object::Integer(Integer { value: 0 }))),
            ("5 * 5", Some(Object::Integer(Integer { value: 25 }))),
            ("5 / 5", Some(Object::Integer(Integer { value: 1 }))),
            ("10 / 2", Some(Object::Integer(Integer { value: 5 }))),
            ("5 % 2", Some(Object::Integer(Integer { value: 1 }))),
            ("10 % 2", Some(Object::Integer(Integer { value: 0 }))),
            ("5 + 5 - 2 * 3 / 1 % 4", Some(Object::Integer(Integer{value: (5 + 5 - 2*3/1%4)}))), // 10 - 6 % 4 = 10 - 2 = 8
        ];
        for (input, expected) in tests {
            assert_eq!(test_eval_program(input), expected, "Input: {}", input);
        }
    }

    #[test]
    fn test_integer_division_by_zero() {
        match test_eval_program("5 / 0") {
            Some(Object::Error(e)) => assert_eq!(e, "division by zero"),
            other => panic!("Expected division by zero error, got {:?}", other),
        }
        match test_eval_program("5 % 0") {
            Some(Object::Error(e)) => assert_eq!(e, "modulo by zero"),
            other => panic!("Expected modulo by zero error, got {:?}", other),
        }
    }

    #[test]
    fn test_integer_comparisons() {
        let tests = vec![
            ("1 < 2", Some(TRUE_OBJ.clone())),
            ("1 > 2", Some(FALSE_OBJ.clone())),
            ("1 < 1", Some(FALSE_OBJ.clone())),
            ("1 > 1", Some(FALSE_OBJ.clone())),
            ("1 == 1", Some(TRUE_OBJ.clone())),
            ("1 != 1", Some(FALSE_OBJ.clone())),
            ("1 == 2", Some(FALSE_OBJ.clone())),
            ("1 != 2", Some(TRUE_OBJ.clone())),
            ("1 <= 2", Some(TRUE_OBJ.clone())),
            ("1 <= 1", Some(TRUE_OBJ.clone())),
            ("2 <= 1", Some(FALSE_OBJ.clone())),
            ("1 >= 2", Some(FALSE_OBJ.clone())),
            ("1 >= 1", Some(TRUE_OBJ.clone())),
            ("2 >= 1", Some(TRUE_OBJ.clone())),
        ];
        for (input, expected) in tests {
            assert_eq!(test_eval_program(input), expected, "Input: {}", input);
        }
    }

    #[test]
    fn test_string_concatenation_and_comparison() {
        let tests = vec![
            (r#""Hello" + " " + "World!""#, Some(Object::String(StringObject { value: "Hello World!".to_string() }))),
            (r#""Hello" == "Hello""#, Some(TRUE_OBJ.clone())),
            (r#""Hello" == "World""#, Some(FALSE_OBJ.clone())),
            (r#""Hello" != "World""#, Some(TRUE_OBJ.clone())),
            (r#""Hello" != "Hello""#, Some(FALSE_OBJ.clone())),
        ];
        for (input, expected) in tests {
            assert_eq!(test_eval_program(input), expected, "Input: {}", input);
        }
    }
    
    #[test]
    fn test_boolean_comparison() {
        let tests = vec![
            // Assuming true and false keywords are parsed into BooleanObjects
            // For now, these will fail as true/false are not keywords yet.
            // We can test by comparing results of other boolean expressions.
            ("(1 < 2) == (5 > 3)", Some(TRUE_OBJ.clone())), // true == true
            ("(1 > 2) == (5 > 3)", Some(FALSE_OBJ.clone())),// false == true
            ("(1 < 2) != (5 < 3)", Some(TRUE_OBJ.clone())),  // true != false
        ];
         for (input, expected) in tests {
            assert_eq!(test_eval_program(input), expected, "Input: {}", input);
        }
    }

    #[test]
    fn test_type_mismatch_errors() {
        let tests = vec![
            ("5 + \"hello\"", "type mismatch: INTEGER + STRING"),
            ("\"hello\" - 5", "type mismatch: STRING - INTEGER"),
            ("5 * \"true\"", "type mismatch: INTEGER * STRING"), // Assuming "true" is string for now
            // Add more type mismatches for other operators if they are not explicitly allowed
        ];
        for (input, expected_msg_part) in tests {
            match test_eval_program(input) {
                Some(Object::Error(e)) => assert!(e.contains(expected_msg_part), "Input: '{}', Expected msg to contain '{}', got '{}'", input, expected_msg_part, e),
                other => panic!("Expected type mismatch error for input '{}', got {:?}", input, other),
            }
        }
    }

    #[test]
    fn test_unknown_operator_errors() {
        let tests = vec![
            ("5 & 5", "unknown operator: INTEGER & INTEGER"), // Assuming & is not defined
            ("\"hello\" * \"world\"", "unknown operator: STRING * STRING"),
        ];
        for (input, expected_msg_part) in tests {
            match test_eval_program(input) {
                Some(Object::Error(e)) => assert!(e.contains(expected_msg_part), "Input: '{}', Expected msg to contain '{}', got '{}'", input, expected_msg_part, e),
                other => panic!("Expected unknown operator error for input '{}', got {:?}", input, other),
            }
        }
    }

    // New comprehensive tests for infix operations

    #[test]
    fn test_infix_integer_arithmetic_evaluation() {
        let tests = vec![
            ("5 + 5;", Some(Object::Integer(Integer { value: 10 }))),
            ("5 - 10;", Some(Object::Integer(Integer { value: -5 }))),
            ("2 * 3 * 4;", Some(Object::Integer(Integer { value: 24 }))),
            ("10 / 2;", Some(Object::Integer(Integer { value: 5 }))),
            ("10 % 3;", Some(Object::Integer(Integer { value: 1 }))),
            ("(2 + 3) * 4;", Some(Object::Integer(Integer { value: 20 }))),
        ];
        for (input, expected) in tests {
            assert_eq!(test_eval_program(input), expected, "Input: {}", input);
        }

        // Test division/modulo by zero
        match test_eval_program("10 / 0;") {
            Some(Object::Error(e)) => assert_eq!(e, "division by zero"),
            other => panic!("Expected division by zero error, got {:?}", other),
        }
        match test_eval_program("10 % 0;") {
            Some(Object::Error(e)) => assert_eq!(e, "modulo by zero"),
            other => panic!("Expected modulo by zero error, got {:?}", other),
        }
    }

    #[test]
    fn test_infix_integer_comparison_evaluation() {
        let tests = vec![
            ("5 < 10;", Some(TRUE_OBJ.clone())),
            ("10 > 5;", Some(TRUE_OBJ.clone())),
            ("5 == 5;", Some(TRUE_OBJ.clone())),
            ("5 != 10;", Some(TRUE_OBJ.clone())),
            ("5 <= 5;", Some(TRUE_OBJ.clone())),
            ("5 <= 4;", Some(FALSE_OBJ.clone())),
            ("5 >= 5;", Some(TRUE_OBJ.clone())),
            ("5 >= 6;", Some(FALSE_OBJ.clone())),
        ];
        for (input, expected) in tests {
            assert_eq!(test_eval_program(input), expected, "Input: {}", input);
        }
    }

    #[test]
    fn test_infix_string_concatenation_evaluation() {
        let tests = vec![
            (r#""hello" + " " + "world";"#, Some(Object::String(StringObject { value: "hello world".to_string() }))),
            (r#""foo" + "bar";"#, Some(Object::String(StringObject { value: "foobar".to_string() }))),
        ];
        for (input, expected) in tests {
            assert_eq!(test_eval_program(input), expected, "Input: {}", input);
        }
    }

    #[test]
    fn test_infix_string_comparison_evaluation() {
        let tests = vec![
            (r#""hello" == "hello";"#, Some(TRUE_OBJ.clone())),
            (r#""hello" == "world";"#, Some(FALSE_OBJ.clone())),
            (r#""hello" != "world";"#, Some(TRUE_OBJ.clone())),
            (r#""hello" != "hello";"#, Some(FALSE_OBJ.clone())),
        ];
        for (input, expected) in tests {
            assert_eq!(test_eval_program(input), expected, "Input: {}", input);
        }
    }

    #[test]
    fn test_infix_boolean_comparison_evaluation() {
        // Since true/false keywords are not yet supported,
        // we use expressions that evaluate to booleans.
        let tests = vec![
            ("(1 < 2) == (5 > 3);", Some(TRUE_OBJ.clone())),   // true == true
            ("(1 < 2) == (2 < 1);", Some(FALSE_OBJ.clone())),  // true == false
            ("(1 > 2) != (5 > 3);", Some(TRUE_OBJ.clone())),   // false != true
            ("(1 < 2) != (5 > 3);", Some(FALSE_OBJ.clone())),  // true != true -> false
        ];
        for (input, expected) in tests {
            assert_eq!(test_eval_program(input), expected, "Input: {}", input);
        }
    }

    #[test]
    fn test_operator_precedence_evaluation() {
        let tests = vec![
            ("1 + 2 * 3;", Some(Object::Integer(Integer { value: 7 }))),
            ("1 * 2 + 3;", Some(Object::Integer(Integer { value: 5 }))),
            ("(1 + 2) * 3;", Some(Object::Integer(Integer { value: 9 }))),
            // Tests requiring prefix '-' or '!' are skipped as they are not implemented.
            // ("-(1 + 2) * 3;", Some(Object::Integer(Integer { value: -9 }))),
            // ("!(true == false);", Some(TRUE_OBJ.clone())), 
        ];
        for (input, expected) in tests {
            assert_eq!(test_eval_program(input), expected, "Input: {}", input);
        }
    }

    #[test]
    fn test_infix_type_mismatch_error_evaluation() {
        let tests = vec![
            ("5 + \"hello\";", "type mismatch: INTEGER + STRING"),
            ("\"hello\" - 5;", "unknown operator: STRING - INTEGER"), // String does not support '-'
            // ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"), // This will be tested when true/false literals are supported
            ("(1 < 2) + (2 < 1);", "unknown operator: BOOLEAN + BOOLEAN"), // Test with boolean expressions
        ];
        for (input, expected_msg_part) in tests {
            match test_eval_program(input) {
                Some(Object::Error(e)) => assert!(e.contains(expected_msg_part), "Input: '{}', Expected msg to contain '{}', got '{}'", input, expected_msg_part, e),
                other => panic!("Expected type mismatch error for input '{}', got {:?}", input, other),
            }
        }
    }
}
