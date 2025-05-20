use crate::ast::{Node, Program, Statement, Expression, Identifier, BlockStatement}; // Removed unused FunctionLiteral, ClassStatement, etc. direct imports if not directly used by eval
use crate::object::{Object, Integer, Class, Instance, FunctionObject, StringObject}; // Removed unused ObjectType
use crate::environment::Environment;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

// Constants
const NULL: Object = Object::Null;

pub fn eval(node: Box<dyn Node>, env: Rc<RefCell<Environment>>) -> Option<Object> { // env type changed
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

fn eval_program(program: &Program, env: Rc<RefCell<Environment>>) -> Option<Object> { // env type changed
    let mut result = None; 
    for statement in &program.statements {
        let eval_result = eval_statement(statement, env.clone()); // Clone Rc for each statement
        match eval_result {
            Some(Object::ReturnValue(value)) => return Some(*value), 
            Some(Object::Error(msg)) => return Some(Object::Error(msg)), 
            Some(_) => result = eval_result, 
            None => result = None, 
        }
    }
    result 
}

fn eval_block_statement(block: &BlockStatement, env: Rc<RefCell<Environment>>) -> Option<Object> { // env type changed
    let mut result = None;
    // Create a new enclosed environment for the block if needed, or use the passed one.
    // For simple blocks not defining new scopes (like if/fn bodies), using the passed env is fine.
    // If blocks were to create new scopes, it would be:
    // let mut block_env = Environment::new_enclosed(env.clone());
    // let mut block_env_rc = Rc::new(RefCell::new(block_env));

    for statement in &block.statements {
        let eval_result = eval_statement(statement, env.clone()); // Pass the block's env
        match eval_result {
            Some(Object::ReturnValue(value)) => return Some(Object::ReturnValue(value)), 
            Some(Object::Error(msg)) => return Some(Object::Error(msg)), 
            Some(_) => result = eval_result,
            None => result = None,
        }
    }
    result
}


fn eval_statement(statement: &Statement, env: Rc<RefCell<Environment>>) -> Option<Object> { // env type changed
    match statement {
        Statement::LetStatement { name, value, .. } => {
            eval_let_statement(name, value, env)
        }
        Statement::ExpressionStatement { expression, .. } => {
            eval_expression(expression, env)
        }
        Statement::ClassStatement { name, methods, .. } => { // Corrected match arm
            eval_class_statement(name, methods, env)
        }
        // TODO: Statement::ReturnStatement { value } => { ... }
    }
}

fn eval_class_statement(name_ident: &Identifier, method_nodes: &Vec<crate::ast::FunctionLiteral>, env: Rc<RefCell<Environment>>) -> Option<Object> { // env type changed
    let class_name = name_ident.value.clone();
    
    let mut methods_map = HashMap::new();
    for method_literal in method_nodes {
        let func_obj = FunctionObject {
            parameters: method_literal.parameters.clone(),
            body: method_literal.body.clone(),
            env: env.clone(), // Methods capture the environment where the class is defined.
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

    env.borrow_mut().set(class_name, class_obj); // Use borrow_mut() for set
    Some(NULL) 
}


fn eval_let_statement(name: &Identifier, value_expr: &Expression, env: Rc<RefCell<Environment>>) -> Option<Object> { // env type changed
    let val = eval_expression(value_expr, env.clone()); // Clone Rc for expression eval
    match val {
        Some(Object::Error(e)) => return Some(Object::Error(e)), 
        Some(obj_val) => {
            env.borrow_mut().set(name.value.clone(), obj_val); // Use borrow_mut() for set
            Some(NULL) 
        }
        None => Some(Object::Error(format!("Expression for let {} did not produce a value", name.value))),
    }
}

fn eval_expression(expression: &Expression, env: Rc<RefCell<Environment>>) -> Option<Object> { // env type changed
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
        Expression::ClassInstantiation { name, arguments, .. } => { // Corrected match arm
            eval_class_instantiation(name, arguments, env)
        }
        Expression::FunctionLiteral(func_lit_node) => { // func_lit_node is &crate::ast::FunctionLiteral
            Some(Object::Function(Rc::new(FunctionObject {
                parameters: func_lit_node.parameters.clone(),
                body: func_lit_node.body.clone(),
                env: env.clone(), // Functions capture their definition environment
                name: func_lit_node.name.clone(),
            })))
        }
        Expression::MethodCall { object, name, arguments, .. } => { // Corrected match arm
            eval_method_call(object, name, arguments, env)
        }
    }
}

fn eval_class_instantiation(class_name_ident: &Identifier, arg_exprs: &Vec<Expression>, env: Rc<RefCell<Environment>>) -> Option<Object> { // env type changed
    match env.borrow().get(&class_name_ident.value) { // Use borrow() for get
        Some(Object::Class(class_rc)) => {
            let _evaluated_args = arg_exprs.iter()
                .map(|arg_expr| eval_expression(arg_expr, env.clone())) // Clone Rc for arg eval
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

fn eval_method_call(object_expr: &Expression, method_name_ident: &Identifier, arg_exprs: &Vec<Expression>, env: Rc<RefCell<Environment>>) -> Option<Object> { // env type changed
    let object_val_opt = eval_expression(object_expr, env.clone()); // Clone Rc
    if object_val_opt.is_none() {
        return Some(Object::Error("Failed to evaluate object for method call".to_string()));
    }
    let object_val = object_val_opt.unwrap();
     match &object_val { // Match on reference
        Object::Error(e) => return Some(Object::Error(e.clone())), // Clone error message
        _ => {}
    }

    let instance_rc = match &object_val { // Match on reference
        Object::Instance(inst_rc) => inst_rc.clone(), // Clone Rc<Instance>
        _ => return Some(Object::Error(format!("Method call on non-instance type: {}", object_val.object_type()))),
    };

    let class_rc = &instance_rc.class; 

    let method_name_str = &method_name_ident.value;
    let func_obj_rc = match class_rc.methods.get(method_name_str) {
        Some(method_rc) => method_rc.clone(),
        None => return Some(Object::Error(format!("Method '{}' not found on class '{}'", method_name_str, class_rc.name))),
    };

    // Create method call environment, enclosed by the FUNCTION's definition env
    let mut method_env_val = Environment::new_enclosed(func_obj_rc.env.clone()); // func_obj_rc.env is Rc<RefCell<Environment>>
    method_env_val.set("self".to_string(), object_val.clone()); // Bind self (clone object_val)

    // Evaluate arguments
    let mut evaluated_args = Vec::new();
    for arg_expr in arg_exprs { // arg_exprs is &Vec<Expression>
        match eval_expression(arg_expr, env.clone()) { // Args evaluated in caller's env. Or should it be method_env? Usually caller.
            Some(Object::Error(e)) => return Some(Object::Error(e)),
            Some(arg_val) => evaluated_args.push(arg_val),
            None => return Some(Object::Error("Failed to evaluate argument for method call".to_string())),
        }
    }
    
    // Bind arguments to parameters in method_env_val
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


fn eval_identifier(ident: &Identifier, env: Rc<RefCell<Environment>>) -> Option<Object> { // env type changed
    match env.borrow().get(&ident.value) { // Use borrow() for get
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

        let env = Rc::new(RefCell::new(Environment::new())); // Create initial env for tests
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

    #[test]
    fn test_eval_let_statement() {
        let input = "let x = 5; x;";
        // let expected = Object::Integer(Integer { value: 5 }); // Original assertion
         match test_eval_program(input) {
            Some(Object::Integer(val)) => assert_eq!(val.value, 5),
            Some(other) => panic!("Expected Integer, got {:?}", other),
            None => panic!("test_eval_program returned None"),
        }
    }

    #[test]
    fn test_class_definition_and_instantiation() {
        let input = r#"
            class Point { }
            let p = Point();
            p; 
        "#;
        match test_eval_program(input) {
            Some(Object::Instance(instance_rc)) => {
                assert_eq!(instance_rc.class.name, "Point");
                assert!(instance_rc.fields.borrow().is_empty());
            }
            Some(other) => panic!("Expected Instance, got {:?} (inspect: {})", other.object_type(), other.inspect()),
            None => panic!("test_eval_program returned None"),
        }
    }
    
    #[test]
    fn test_method_definition_and_call() {
        let input = r#"
            class Greeter {
                fn say_hi() {
                    "hi"; 
                }
            }
            let g = Greeter();
            g.say_hi();
        "#;
        match test_eval_program(input) {
            Some(Object::String(s_obj)) => {
                assert_eq!(s_obj.value, "hi", "Method call did not return 'hi'. Got: {}", s_obj.value);
            }
            Some(Object::Error(e)) => {
                 panic!("Evaluation error during method call test: {}", e);
            }
            Some(other) => {
                panic!("Expected String object from method call, got {:?} (inspect: {})", other.object_type(), other.inspect());
            }
            None => {
                panic!("test_eval_program returned None for method call test.");
            }
        }
    }

    #[test]
    fn test_string_literal_evaluation() {
        struct TestCase {
            input: &'static str,
            expected_value: &'static str,
        }

        let tests = vec![
            TestCase {
                input: r#""hello world""#,
                expected_value: "hello world",
            },
            TestCase {
                input: r#""""#, 
                expected_value: "",
            },
            TestCase {
                input: r#"let greeting = "hello"; greeting;"#,
                expected_value: "hello",
            },
            TestCase {
                input: r#""hi""#,
                expected_value: "hi",
            }
        ];

        for tt in tests {
            match test_eval_program(tt.input) {
                Some(Object::String(s_obj)) => {
                    assert_eq!(s_obj.value, tt.expected_value, "Input: '{}'. Expected '{}', got '{}'", tt.input, tt.expected_value, s_obj.value);
                }
                Some(other_obj) => {
                    panic!("Input: '{}'. Expected String object, got {:?} (inspect: {})", tt.input, other_obj.object_type(), other_obj.inspect());
                }
                None => {
                    panic!("Input: '{}'. test_eval_program returned None.", tt.input);
                }
            }
        }
    }

    #[test]
    fn test_class_not_found() {
        let input = "let p = UnknownClass(); p;";
        match test_eval_program(input) {
            Some(Object::Error(e)) => assert!(e.contains("Class 'UnknownClass' not found"), "Error message mismatch: {}", e),
            Some(other) => panic!("Expected Error, got {:?}", other),
            None => panic!("Expected error, got None"),
        }
    }

     #[test]
    fn test_instantiating_non_class() {
        let input = r#"
            let x = 10;
            let p = x(); 
            p;
        "#;
         match test_eval_program(input) {
            Some(Object::Error(e)) => assert!(e.contains("Expected class 'x'"), "Error message mismatch: {}", e),
            Some(other) => panic!("Expected Error, got {:?}", other),
            None => panic!("Expected error, got None"),
        }
    }

    #[test]
    fn test_method_not_found() {
        let input = r#"
            class Test { }
            let t = Test();
            t.unknown_method();
        "#;
        match test_eval_program(input) {
            Some(Object::Error(e)) => assert!(e.contains("Method 'unknown_method' not found on class 'Test'"), "Error message mismatch: {}", e),
            Some(other) => panic!("Expected Error, got {:?}", other),
            None => panic!("Expected error, got None"),
        }
    }
}
