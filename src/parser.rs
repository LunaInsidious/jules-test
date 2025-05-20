use crate::ast::{Program, Statement, Expression, Identifier, BlockStatement, FunctionLiteral};
use crate::lexer::Lexer;
use crate::token::Token;
use std::mem;
use std::collections::HashMap;

// Precedence levels for operators (Step 1 - confirm/update)
#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
enum Precedence {
    Lowest,
    Equals,      // ==, !=
    LessGreater, // >, <, >=, <=
    Sum,         // +, -
    Product,     // *, /, %
    Prefix,      // -X or !X
    Call,        // myFunction(X)
    Index,       // array[index] (Future)
    Dot,         // object.method
}

// Define function types for Pratt parsing
type PrefixParseFn = fn(&mut Parser) -> Option<Expression>; 
// InfixParseFn now takes Box<Expression> as its left argument and returns Option<Box<Expression>>
type InfixParseFn = fn(&mut Parser, Box<Expression>) -> Option<Box<Expression>>;

// Helper function to map tokens to precedence (Step 1)
fn token_to_precedence(token: &Token) -> Precedence {
    match token {
        Token::Eq | Token::NotEq => Precedence::Equals,
        Token::LT | Token::GT | Token::LTEq | Token::GTEq => Precedence::LessGreater,
        Token::Plus | Token::Minus => Precedence::Sum,
        Token::Slash | Token::Asterisk | Token::Percent => Precedence::Product,
        Token::LParen => Precedence::Call,
        Token::Dot => Precedence::Dot,
        // Token::LBracket => Precedence::Index, // Future
        _ => Precedence::Lowest,
    }
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,

    prefix_parse_fns: HashMap<mem::Discriminant<Token>, PrefixParseFn>,
    infix_parse_fns: HashMap<mem::Discriminant<Token>, InfixParseFn>,
    // precedences HashMap is removed (Step 1)
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let mut p = Parser {
            lexer,
            cur_token,
            peek_token,
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
            // precedences: HashMap::new(), // Removed
        };

        // Register prefix parsing functions
        p.register_prefix(mem::discriminant(&Token::Ident("".into())), Parser::parse_identifier_expression);
        p.register_prefix(mem::discriminant(&Token::Int("".into())), Parser::parse_integer_literal_expression);
        p.register_prefix(mem::discriminant(&Token::Function), Parser::parse_function_literal_expression);
        p.register_prefix(mem::discriminant(&Token::String("".into())), Parser::parse_string_literal_expression);
        // TODO: Register prefix functions for Token::Bang, Token::Minus (for PrefixExpression)

        // Register infix parsing functions (Step 2)
        p.register_infix(mem::discriminant(&Token::LParen), Parser::parse_call_or_instantiation_expression);
        p.register_infix(mem::discriminant(&Token::Dot), Parser::parse_dot_expression);
        
        // Register new infix operators
        p.register_infix(mem::discriminant(&Token::Plus), Parser::parse_infix_expression);
        p.register_infix(mem::discriminant(&Token::Minus), Parser::parse_infix_expression);
        p.register_infix(mem::discriminant(&Token::Slash), Parser::parse_infix_expression);
        p.register_infix(mem::discriminant(&Token::Asterisk), Parser::parse_infix_expression);
        p.register_infix(mem::discriminant(&Token::Percent), Parser::parse_infix_expression);
        p.register_infix(mem::discriminant(&Token::Eq), Parser::parse_infix_expression);
        p.register_infix(mem::discriminant(&Token::NotEq), Parser::parse_infix_expression);
        p.register_infix(mem::discriminant(&Token::LT), Parser::parse_infix_expression);
        p.register_infix(mem::discriminant(&Token::GT), Parser::parse_infix_expression);
        p.register_infix(mem::discriminant(&Token::LTEq), Parser::parse_infix_expression);
        p.register_infix(mem::discriminant(&Token::GTEq), Parser::parse_infix_expression);
        
        p
    }

    fn register_prefix(&mut self, token_type_disc: mem::Discriminant<Token>, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type_disc, func);
    }

    fn register_infix(&mut self, token_type_disc: mem::Discriminant<Token>, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type_disc, func);
    }

    // Helper methods for precedence using token_to_precedence (Step 5)
    fn peek_precedence(&self) -> Precedence {
        token_to_precedence(&self.peek_token)
    }

    fn cur_precedence(&self) -> Precedence {
        token_to_precedence(&self.cur_token)
    }
    
    // Helper to check if peek_token is Semicolon (Step 5)
    fn peek_token_is_semicolon(&self) -> bool {
        self.peek_token_is(Token::Semicolon)
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };
        while self.cur_token != Token::Eof {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            } else {
                self.next_token();
            }
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Class => self.parse_class_statement(),
            _ => self.parse_expression_statement(),
        }
    }
    
    fn parse_function_literal(&mut self, is_method: bool) -> Option<FunctionLiteral> {
        let fn_token = self.cur_token.clone(); 
    
        let name = if is_method {
            if !self.expect_peek_discriminant(mem::discriminant(&Token::Ident("".into()))) {
                self.peek_error_discriminant(mem::discriminant(&Token::Ident("".into())));
                return None;
            }
            match self.cur_token.clone() {
                Token::Ident(val) => Some(val),
                _ => return None, 
            }
        } else {
            if self.peek_token_is_discriminant(mem::discriminant(&Token::Ident("".into()))) {
                 self.next_token(); 
                 match self.cur_token.clone() {
                    Token::Ident(val) => Some(val),
                    _ => return None, 
                }
            } else {
                None
            }
        };
    
        if !self.expect_peek(Token::LParen) {
            self.peek_error(Token::LParen);
            return None;
        }
    
        let parameters = self.parse_function_parameters()?;
    
        if !self.expect_peek(Token::LBrace) {
            self.peek_error(Token::LBrace);
            return None;
        }
    
        let body = match self.parse_block_statement() {
            Some(bs) => bs,
            None => return None,
        };
        
        Some(FunctionLiteral {
            token: fn_token,
            parameters,
            body,
            name,
        })
    }

    fn parse_function_literal_expression(&mut self) -> Option<Expression> {
        self.parse_function_literal(false).map(Expression::FunctionLiteral)
    }


    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers = Vec::new();
        if self.peek_token_is(Token::RParen) {
            self.next_token(); 
            return Some(identifiers);
        }
        self.next_token(); 
        match &self.cur_token {
            Token::Ident(name_val) => {
                identifiers.push(Identifier { token: self.cur_token.clone(), value: name_val.clone() });
            }
            _ => {
                self.errors.push(format!("Expected identifier in parameter list, got {:?}", self.cur_token));
                return None;
            }
        }
        while self.peek_token_is(Token::Comma) {
            self.next_token(); 
            self.next_token(); 
             match &self.cur_token {
                Token::Ident(name_val) => {
                    identifiers.push(Identifier { token: self.cur_token.clone(), value: name_val.clone() });
                }
                _ => {
                    self.errors.push(format!("Expected identifier in parameter list after comma, got {:?}", self.cur_token));
                    return None;
                }
            }
        }
        if !self.expect_peek(Token::RParen) {
            self.peek_error(Token::RParen);
            return None;
        }
        Some(identifiers)
    }


    fn parse_class_statement(&mut self) -> Option<Statement> {
        let class_token = self.cur_token.clone(); 
        if !self.expect_peek_discriminant(mem::discriminant(&Token::Ident("".into()))) {
            self.peek_error_discriminant(mem::discriminant(&Token::Ident("".into())));
            return None;
        }
        let name = match self.cur_token.clone() {
            Token::Ident(val) => Identifier { token: self.cur_token.clone(), value: val },
            _ => return None, 
        };
        if !self.expect_peek(Token::LBrace) {
            self.peek_error(Token::LBrace);
            return None;
        }
        self.next_token(); 
        let mut methods = Vec::new();
        while !self.cur_token_is(Token::RBrace) && !self.cur_token_is(Token::Eof) {
            if self.cur_token_is(Token::Function) {
                if let Some(method_fn) = self.parse_function_literal(true) { 
                    methods.push(method_fn);
                } else {
                    self.errors.push("Failed to parse method in class".to_string());
                    return None; 
                }
            } else {
                self.errors.push(format!("Expected 'fn' for method or '}}' to end class, got {:?}", self.cur_token));
                self.next_token(); 
            }
        }
        if !self.cur_token_is(Token::RBrace) { 
            self.errors.push(format!("Expected '}}' for class {}, got {:?}", name.value, self.cur_token));
            return None;
        }
        self.next_token(); 
        Some(Statement::ClassStatement {
            token: class_token,
            name,
            methods,
        })
    }


    fn parse_let_statement(&mut self) -> Option<Statement> {
        let let_token = self.cur_token.clone(); 
        if !self.expect_peek_discriminant(mem::discriminant(&Token::Ident("".into()))) {
            self.peek_error_discriminant(mem::discriminant(&Token::Ident("".into())));
            return None;
        }
        let name = match self.cur_token.clone() {
            Token::Ident(ident_val) => Identifier {
                token: self.cur_token.clone(),
                value: ident_val,
            },
            _ => return None, 
        };
        if !self.expect_peek(Token::Assign) {
            self.peek_error(Token::Assign);
            return None;
        }
        self.next_token(); 
        let value_expression = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => *expr, // Unbox here
            None => {
                self.errors.push("Failed to parse expression in let statement".to_string());
                return None;
            }
        };
        if self.peek_token_is(Token::Semicolon) {
            self.next_token(); 
        }
        self.next_token(); 
        Some(Statement::LetStatement {
            token: let_token,
            name,
            value: value_expression,
        })
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let stmt_token = self.cur_token.clone();
        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => *expr, // Unbox here
            None => return None, 
        };
        if self.peek_token_is(Token::Semicolon) {
            self.next_token(); 
        }
        self.next_token(); 
        Some(Statement::ExpressionStatement {
            token: stmt_token, 
            expression,
        })
    }
    
    // parse_expression now returns Option<Box<Expression>> (Step 4)
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<Expression>> {
        let prefix_fn_opt = self.prefix_parse_fns.get(&mem::discriminant(&self.cur_token)).copied();

        let mut left_exp_opt = match prefix_fn_opt {
            Some(prefix_fn) => prefix_fn(self).map(Box::new), // Box the result of prefix_fn
            None => {
                self.no_prefix_parse_fn_error();
                return None;
            }
        };
        
        // Pratt parser loop for infix expressions (Step 4)
        while !self.peek_token_is_semicolon() && precedence < self.peek_precedence() {
            let infix_fn_opt = self.infix_parse_fns.get(&mem::discriminant(&self.peek_token)).copied();
            
            if infix_fn_opt.is_none() { 
                return left_exp_opt; 
            }
            
            self.next_token(); // Consume the infix operator, making it cur_token.
            
            if left_exp_opt.is_none() { 
                self.errors.push(format!("Expected an expression before infix operator {}, but found None.", self.cur_token.to_string()));
                return None; 
            }

            left_exp_opt = infix_fn_opt.unwrap()(self, left_exp_opt.unwrap()); 
                                                                    
            if left_exp_opt.is_none() { 
                return None; 
            } 
        }
        left_exp_opt
    }

    // Prefix parsing functions
    fn parse_identifier_expression(&mut self) -> Option<Expression> {
        match &self.cur_token {
            Token::Ident(name) => Some(Expression::Identifier(Identifier {
                token: self.cur_token.clone(),
                value: name.clone(),
            })),
            _ => None, 
        }
    }

    fn parse_integer_literal_expression(&mut self) -> Option<Expression> {
        match &self.cur_token {
            Token::Int(val_str) => {
                match val_str.parse::<i64>() {
                    Ok(value) => Some(Expression::IntegerLiteral {
                        token: self.cur_token.clone(),
                        value,
                    }),
                    Err(_) => {
                        self.errors.push(format!("Could not parse '{}' as integer", val_str));
                        None
                    }
                }
            },
            _ => None, 
        }
    }

    fn parse_string_literal_expression(&mut self) -> Option<Expression> {
        match &self.cur_token {
            Token::String(value) => Some(Expression::StringLiteral {
                token: self.cur_token.clone(),
                value: value.clone(),
            }),
            _ => {
                self.errors.push(format!("Expected Token::String, got {:?}", self.cur_token));
                None
            }
        }
    }

    // Infix parsing function for common infix operators (Step 3)
    fn parse_infix_expression(&mut self, left: Box<Expression>) -> Option<Box<Expression>> {
        let operator_token = self.cur_token.clone(); 
        let operator_str = self.cur_token.to_string(); 
        let current_precedence = self.cur_precedence(); 

        self.next_token(); // Consume the operator token

        let right_opt = self.parse_expression(current_precedence); 
        
        if right_opt.is_none() {
            self.errors.push(format!("Expected expression on the right side of operator {}", operator_str));
            return None;
        }
        let right = right_opt.unwrap(); 

        Some(Box::new(Expression::InfixExpression {
            token: operator_token,
            left, 
            operator: operator_str,
            right, 
        }))
    }
    
    // parse_call_or_instantiation_expression now takes and returns Box<Expression>
    fn parse_call_or_instantiation_expression(&mut self, left_expression: Box<Expression>) -> Option<Box<Expression>> {
        let arguments = self.parse_expression_list(Token::RParen)?;
        match *left_expression { 
            Expression::Identifier(ident_node) => { 
                 Some(Box::new(Expression::ClassInstantiation { 
                    token: ident_node.token.clone(), 
                    name: ident_node, 
                    arguments, 
                }))
            }
            _ => {
                self.errors.push(format!("Calling non-identifier/non-function not yet supported. Got: {:?}", left_expression));
                None
            }
        }
    }

    // parse_dot_expression now takes and returns Box<Expression>
    fn parse_dot_expression(&mut self, left_expression: Box<Expression>) -> Option<Box<Expression>> {
        let dot_token = self.cur_token.clone();
        if !self.expect_peek_discriminant(mem::discriminant(&Token::Ident("".into()))) {
            self.peek_error_discriminant(mem::discriminant(&Token::Ident("".into())));
            return None;
        }
        let method_name = match self.cur_token.clone() {
            Token::Ident(val) => Identifier { token: self.cur_token.clone(), value: val },
            _ => return None, 
        };
        if !self.expect_peek(Token::LParen) {
            self.errors.push(format!("Field access (e.g. object.field) is not yet supported. Expected '(' for method call after '.{}'.", method_name.value));
            return None;
        }
        let arguments = self.parse_expression_list(Token::RParen)?;
        Some(Box::new(Expression::MethodCall {
            token: dot_token,
            object: left_expression,
            name: method_name,
            arguments,
        }))
    }
    
    // parse_expression_list now unboxes expressions from parse_expression (Step 4)
    fn parse_expression_list(&mut self, end_token_type: Token) -> Option<Vec<Expression>> { 
        let mut list = Vec::new();
        if self.peek_token_is(end_token_type.clone()) { 
            self.next_token(); 
            return Some(list); 
        }
        self.next_token(); 
        if let Some(exp) = self.parse_expression(Precedence::Lowest).map(|boxed_exp| *boxed_exp) { 
            list.push(exp);
        } else {
            self.errors.push("Failed to parse first expression in list".to_string());
            return None; 
        }
        while self.peek_token_is(Token::Comma) {
            self.next_token(); 
            self.next_token(); 
            if let Some(exp) = self.parse_expression(Precedence::Lowest).map(|boxed_exp| *boxed_exp) { 
                list.push(exp);
            } else {
                 self.errors.push("Failed to parse expression after comma in list".to_string());
                return None; 
            }
        }
        if !self.expect_peek(end_token_type.clone()) { 
            self.peek_error(end_token_type); 
            return None;
        }
        Some(list)
    }
    
    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        let block_token = self.cur_token.clone(); 
        let mut statements = Vec::new();
        self.next_token(); 
        while !self.cur_token_is(Token::RBrace) && !self.cur_token_is(Token::Eof) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            } else {
                 self.next_token(); 
            }
        }
        if !self.cur_token_is(Token::RBrace) {
            self.errors.push(format!("Expected '}}' for block statement, got {:?}", self.cur_token));
            return None; 
        }
        self.next_token(); 
        Some(BlockStatement {
            token: block_token,
            statements,
        })
    }
    
    fn no_prefix_parse_fn_error(&mut self) {
        self.errors.push(format!(
            "No prefix parse function for {:?} found",
            self.cur_token
        ));
    }

    // Helper methods
    fn cur_token_is(&self, token_type: Token) -> bool {
        self.cur_token == token_type
    }
    
    #[allow(dead_code)] 
    fn cur_token_is_discriminant(&self, discriminant: mem::Discriminant<Token>) -> bool {
        mem::discriminant(&self.cur_token) == discriminant
    }

    fn peek_token_is(&self, token_type: Token) -> bool {
        self.peek_token == token_type
    }

    fn peek_token_is_discriminant(&self, discriminant: mem::Discriminant<Token>) -> bool {
        mem::discriminant(&self.peek_token) == discriminant
    }

    fn expect_peek(&mut self, expected_token: Token) -> bool {
        if self.peek_token_is(expected_token.clone()) { 
            self.next_token();
            true
        } else {
            false
        }
    }
    
    fn expect_peek_discriminant(&mut self, expected_discriminant: mem::Discriminant<Token>) -> bool {
        if self.peek_token_is_discriminant(expected_discriminant) {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn peek_error(&mut self, expected_token: Token) {
        let msg = format!(
            "Expected next token to be {:?}, got {:?} instead",
            expected_token, self.peek_token
        );
        self.errors.push(msg);
    }

    fn peek_error_discriminant(&mut self, expected_discriminant: mem::Discriminant<Token>) {
         let msg = format!(
            "Expected next token to be of type {:?}, got {:?} (type {:?}) instead",
            Self::token_name_from_discriminant(expected_discriminant),
            self.peek_token,
            mem::discriminant(&self.peek_token)
        );
        self.errors.push(msg);
    }
    
    fn token_name_from_discriminant(discriminant: mem::Discriminant<Token>) -> &'static str {
        if discriminant == mem::discriminant(&Token::Ident("".into())) { "Ident" }
        else if discriminant == mem::discriminant(&Token::Int("".into())) { "Int" }
        else if discriminant == mem::discriminant(&Token::Let) { "Let" }
        else if discriminant == mem::discriminant(&Token::Assign) { "Assign" }
        else if discriminant == mem::discriminant(&Token::Semicolon) { "Semicolon" }
        else if discriminant == mem::discriminant(&Token::LParen) { "LParen" }
        else if discriminant == mem::discriminant(&Token::RParen) { "RParen" }
        else if discriminant == mem::discriminant(&Token::LBrace) { "LBrace" }
        else if discriminant == mem::discriminant(&Token::RBrace) { "RBrace" }
        else if discriminant == mem::discriminant(&Token::Class) { "Class" }
        else if discriminant == mem::discriminant(&Token::Function) { "Function" }
        else if discriminant == mem::discriminant(&Token::Dot) { "Dot" }
        else if discriminant == mem::discriminant(&Token::String("".into())) { "String" }
        // TODO: Add new operators here for error messages if needed
        else { "UnknownTokenDiscriminant" }
    }

    pub fn get_errors(&self) -> &Vec<String> {
        &self.errors
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Node, Statement, Expression, Identifier}; 
    use crate::lexer::Lexer;
    
    fn check_parser_errors(parser: &Parser) {
        let errors = parser.get_errors();
        if !errors.is_empty() {
            let error_messages: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
            panic!("Parser has errors: {:?}", error_messages);
        }
    }


    #[test]
    fn test_let_statement() {
        let input = "let x = 5;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(),1);
        if let Statement::LetStatement { name, value, .. } = &program.statements[0] {
            assert_eq!(name.value, "x");
            if let Expression::IntegerLiteral { value: val_expr, .. } = value {
                 assert_eq!(*val_expr, 5);
            } else {
                panic!("Expected IntegerLiteral, got {:?}", value);
            }
        } else {
            panic!("Expected LetStatement, got {:?}", program.statements[0]);
        }
    }
    
    #[test]
    fn test_class_statement_with_method() {
        let input = r#"
            class Point {
                fn getX() {}
            }
        "#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1, "Program does not contain 1 statement. got={}", program.statements.len());

        match &program.statements[0] {
            Statement::ClassStatement { name, methods, .. } => {
                assert_eq!(name.value, "Point", "Class name is not 'Point'. got={}", name.value);
                assert_eq!(methods.len(), 1, "Class should have 1 method. got={}", methods.len());
                let method = &methods[0];
                assert_eq!(method.name.as_ref().unwrap(), "getX", "Method name is not 'getX'");
                assert!(method.parameters.is_empty(), "Method parameters should be empty.");
                assert_eq!(method.body.statements.len(), 0, "Method body should be empty.");
            }
            _ => panic!("Statement is not ClassStatement. got={:?}", program.statements[0]),
        }
    }

    #[test]
    fn test_method_call_expression() {
        let input = "object.method();";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
    
        assert_eq!(program.statements.len(), 1, "Program statements should be 1, got {}", program.statements.len());
    
        let stmt = match &program.statements[0] {
            Statement::ExpressionStatement { expression, .. } => expression,
            _ => panic!("Statement is not ExpressionStatement. got={:?}", program.statements[0]),
        };
    
        match stmt {
            Expression::MethodCall { object, name, arguments, .. } => {
                if let Expression::Identifier(ident) = &**object {
                    assert_eq!(ident.value, "object", "Object name is not 'object'. got={}", ident.value);
                } else {
                    panic!("Method call object is not Identifier. got={:?}", object);
                }
                assert_eq!(name.value, "method", "Method name is not 'method'. got={}", name.value);
                assert!(arguments.is_empty(), "Method call arguments should be empty.");
            }
            _ => panic!("Expression is not MethodCall. got={:?}", stmt),
        }
    }
    
    #[test]
    fn test_class_instantiation_expression() {
        let input = "Point();"; 
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);
        if let Statement::ExpressionStatement { expression, .. } = &program.statements[0] {
            if let Expression::ClassInstantiation { name, arguments, .. } = expression {
                 assert_eq!(name.value, "Point");
                 assert!(arguments.is_empty());
            } else {
                panic!("Expected ClassInstantiation, got {:?}", expression);
            }
        } else {
             panic!("Expected ExpressionStatement, got {:?}", program.statements[0]);
        }
    }

    #[test]
    fn test_string_literal_expression_parsing() {
        let input = r#""hello world";"#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statement. got={}",
            program.statements.len()
        );

        let stmt = match program.statements.get(0) {
            Some(s) => s,
            None => panic!("No statement found in program."),
        };

        match stmt {
            Statement::ExpressionStatement { expression, .. } => {
                match expression {
                    Expression::StringLiteral { value, .. } => {
                        assert_eq!(value, "hello world", "StringLiteral value is not 'hello world'. got='{}'", value);
                    }
                    _ => panic!("Expression is not StringLiteral. got={:?}", expression),
                }
            }
            _ => panic!("Statement is not ExpressionStatement. got={:?}", stmt),
        }
    }
}
