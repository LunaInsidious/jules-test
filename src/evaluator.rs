use crate::ast::{Node, Program, Statement, Expression, Identifier, ClassStatement, ClassInstantiation, MethodCall, FunctionLiteral, BlockStatement};
use crate::object::{Object, ObjectType, Integer, Class, Instance, FunctionObject, StringObject}; // Added StringObject
use crate::environment::Environment;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

// Constants
const NULL: Object = Object::Null;

pub fn eval(node: Box<dyn Node>, env: &mut Environment) -> Option<Object> {
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
    // Add other top-level node types if necessary
    None 
}

fn eval_program(program: &Program, env: &mut Environment) -> Option<Object> {
    let mut result = None; 
    for statement in &program.statements {
        let eval_result = eval_statement(statement, env);
        match eval_result {
            Some(Object::ReturnValue(value)) => return Some(*value), // Unwrap the Box
            Some(Object::Error(msg)) => return Some(Object::Error(msg)), // Propagate error
            Some(_) => result = eval_result, // Store normal result
            None => result = None, // Propagate None if statement evaluation failed
        }
        // If an error or return occurred, we should have already returned from the function.
    }
    result 
}

fn eval_block_statement(block: &BlockStatement, env: &mut Environment) -> Option<Object> {
    let mut result = None;
    for statement in &block.statements {
        let eval_result = eval_statement(statement, env);
        match eval_result {
            Some(Object::ReturnValue(value)) => return Some(Object::ReturnValue(value)), // Propagate return
            Some(Object::Error(msg)) => return Some(Object::Error(msg)), // Propagate error
            Some(_) => result = eval_result,
            None => result = None,
        }
    }
    result
}


fn eval_statement(statement: &Statement, env: &mut Environment) -> Option<Object> {
    match statement {
        Statement::LetStatement { name, value, .. } => {
            eval_let_statement(name, value, env)
        }
        Statement::ExpressionStatement { expression, .. } => {
            eval_expression(expression, env)
        }
        Statement::ClassStatement(class_stmt_node) => { 
            eval_class_statement(class_stmt_node, env)
        }
        // TODO: Statement::ReturnStatement { value } => { ... }
    }
}

fn eval_class_statement(class_stmt: &ClassStatement, env: &mut Environment) -> Option<Object> {
    let class_name = class_stmt.name.value.clone();
    
    let mut methods = HashMap::new();
    for method_literal in &class_stmt.methods {
        let func_obj = FunctionObject {
            parameters: method_literal.parameters.clone(),
            body: method_literal.body.clone(),
            env: env.clone(), // Capture environment where class is defined
            name: method_literal.name.clone(),
        };
        if let Some(method_name) = &method_literal.name {
            methods.insert(method_name.clone(), Rc::new(func_obj));
        } else {
            // This case should ideally not happen for methods as per current parsing logic
            return Some(Object::Error("Method must have a name".to_string()));
        }
    }

    let class_obj = Object::Class(Rc::new(Class {
        name: class_name.clone(),
        methods,
    }));

    env.set(class_name, class_obj);
    Some(NULL) 
}


fn eval_let_statement(name: &Identifier, value_expr: &Expression, env: &mut Environment) -> Option<Object> {
    let val = eval_expression(value_expr, env);
    match val {
        Some(Object::Error(e)) => return Some(Object::Error(e)), // Propagate error
        Some(obj_val) => {
            env.set(name.value.clone(), obj_val);
            Some(NULL) 
        }
        None => Some(Object::Error(format!("Expression for let {} did not produce a value", name.value))),
    }
}

fn eval_expression(expression: &Expression, env: &mut Environment) -> Option<Object> {
    match expression {
        Expression::IntegerLiteral { value, .. } => {
            Some(Object::Integer(Integer { value: *value }))
        }
        Expression::Identifier(ident_node) => {
            eval_identifier(ident_node, env)
        }
        Expression::ClassInstantiation(inst_expr_node) => { 
            eval_class_instantiation(inst_expr_node, env)
        }
        Expression::FunctionLiteral(func_lit_node) => {
            Some(Object::Function(Rc::new(FunctionObject {
                parameters: func_lit_node.parameters.clone(),
                body: func_lit_node.body.clone(),
                env: env.clone(), 
                name: func_lit_node.name.clone(),
            })))
        }
        Expression::MethodCall(mc_expr_node) => {
            eval_method_call(mc_expr_node, env)
        }
        Expression::StringLiteral { value, .. } => { // Added case for StringLiteral
            Some(Object::String(StringObject { value: value.clone() }))
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
                input: r#""""#, // Empty string
                expected_value: "",
            },
            TestCase {
                input: r#"let greeting = "hello"; greeting;"#,
                expected_value: "hello",
            },
            // Test case to ensure the HACK for "hi" is no longer needed/interfering
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
}

fn eval_class_instantiation(inst_expr: &ClassInstantiation, env: &mut Environment) -> Option<Object> {
    let class_name_ident = &inst_expr.name;

    match env.get(&class_name_ident.value) {
        Some(Object::Class(class_rc)) => {
            // TODO: Argument evaluation and passing to an __init__ method if it exists.
            // For now, arguments are ignored.
            let _evaluated_args = inst_expr.arguments.iter()
                .map(|arg_expr| eval_expression(arg_expr, env))
                .collect::<Vec<_>>();
            // Check for errors in argument evaluation if needed.

            let instance = Object::Instance(Rc::new(Instance {
                class: class_rc.clone(), // Store Rc<Class> in instance
                fields: Rc::new(RefCell::new(HashMap::new())),
            }));
            Some(instance)
        }
        Some(other_obj) => {
            Some(Object::Error(format!("Expected class '{}', found {:?}", class_name_ident.value, other_obj.object_type())))
        }
        None => {
            Some(Object::Error(format!("Class '{}' not found.", class_name_ident.value)))
        }
    }
}

fn eval_method_call(mc_expr: &MethodCall, env: &mut Environment) -> Option<Object> {
    // 1. Evaluate the receiver object
    let object_val_opt = eval_expression(&mc_expr.object, env);
    if object_val_opt.is_none() {
        return Some(Object::Error("Failed to evaluate object for method call".to_string()));
    }
    let object_val = object_val_opt.unwrap();
     match object_val {
        Object::Error(e) => return Some(Object::Error(e)),
        _ => {}
    }


    // 2. Ensure it's an Instance
    let instance_rc = match object_val.clone() { // Clone object_val for self binding
        Object::Instance(inst_rc) => inst_rc,
        _ => return Some(Object::Error(format!("Method call on non-instance type: {}", object_val.object_type()))),
    };

    // 3. Instance now holds Rc<Class>, so no need to re-lookup class from env.
    let class_rc = &instance_rc.class;

    // 4. Find the method in the class's methods
    let method_name = &mc_expr.name.value;
    let func_obj_rc = match class_rc.methods.get(method_name) {
        Some(method_rc) => method_rc.clone(),
        None => return Some(Object::Error(format!("Method '{}' not found on class '{}'", method_name, class_rc.name))),
    };

    // 5. Create method call environment
    //    The method's function object captures the environment where the *class* was defined.
    //    For method calls, we need an environment for the method's execution.
    //    This environment should be enclosed by the function's definition environment.
    let mut method_env = Environment::new_enclosed(func_obj_rc.env.clone());
    method_env.set("self".to_string(), object_val); // Bind self to the instance

    // 6. Evaluate arguments (currently empty for this subtask)
    let mut evaluated_args = Vec::new();
    for arg_expr in &mc_expr.arguments {
        match eval_expression(arg_expr, &mut method_env) { // Use method_env for arg eval? Or outer_env? Usually outer.
            Some(Object::Error(e)) => return Some(Object::Error(e)),
            Some(arg_val) => evaluated_args.push(arg_val),
            None => return Some(Object::Error("Failed to evaluate argument for method call".to_string())),
        }
    }
    // TODO: Bind evaluated_args to func_obj_rc.parameters in method_env

    // 7. Evaluate method body
    let eval_result = eval_block_statement(&func_obj_rc.body, &mut method_env);

    // 8. Unwrap return value if present
    match eval_result {
        Some(Object::ReturnValue(value)) => Some(*value),
        Some(Object::Error(e)) => Some(Object::Error(e)),
        Some(other) => Some(other), // Last expression in block is the implicit return
        None => Some(NULL), // If block is empty or last statement isn't an expression
    }
}


fn eval_identifier(ident: &Identifier, env: &mut Environment) -> Option<Object> {
    // The HACK for "hi" string literal can now be removed as StringLiterals are properly handled.
    // if ident.value == "\"hi\"" { 
    //     return Some(Object::String(StringObject{value: "hi".to_string()}));
    // }

    match env.get(&ident.value) {
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
            // Print parser errors for easier debugging during tests
            eprintln!("Parser errors for input:\n{}\nErrors: {:?}", input, errors);
            panic!("Parser errors: {:?}", errors);
        }

        let mut env = Environment::new();
        eval(Box::new(program_node), &mut env)
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
        let expected = Object::Integer(Integer { value: 5 });
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
        // For this test to pass, we need string literals and implicit returns from blocks,
        // or an explicit return statement. The prompt implies "hi" should be returned.
        // Let's assume for now that the last expression in a block is its return value.
        // And that `Identifier("hi")` will be hacked to produce a string object.
        let input = r#"
            class Greeter {
                fn say_hi() {
                    "hi"; // This needs to be parsed as a String literal ExpressionStatement
                }
            }
            let g = Greeter();
            g.say_hi();
        "#;

        // Modify parser to handle "hi" as an identifier for now, and evaluator to return StringObject
        // This requires `mylang/src/token.rs` to have `String(String)` in Token enum,
        // `mylang/src/lexer.rs` to tokenize strings,
        // `mylang/src/ast.rs` to have `Expression::StringLiteral { token: Token, value: String }`,
        // `mylang/src/parser.rs` to parse string literals into AST.
        // The current setup will parse "hi" as an Identifier.
        // The HACK in `eval_identifier` will catch `Ident("hi")`.

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
    fn test_class_not_found() {
        let input = "let p = UnknownClass(); p;";
        match test_eval_program(input) {
            Some(Object::Error(e)) => assert!(e.contains("Class 'UnknownClass' not found")),
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
            Some(Object::Error(e)) => assert!(e.contains("Expected class 'x'")),
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
            Some(Object::Error(e)) => assert!(e.contains("Method 'unknown_method' not found on class 'Test'")),
            Some(other) => panic!("Expected Error, got {:?}", other),
            None => panic!("Expected error, got None"),
        }
    }
}
