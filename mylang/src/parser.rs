use crate::ast::{Program, Statement, Expression, Identifier, BlockStatement, FunctionLiteral};
use crate::lexer::Lexer;
use crate::token::Token;
use std::mem;
use std::collections::HashMap;

// Precedence levels for operators
#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X) or MyClass()
    Index,       // array[index]
    Dot,         // object.method
}

// Define function types for Pratt parsing
type PrefixParseFn = fn(&mut Parser) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,

    prefix_parse_fns: HashMap<mem::Discriminant<Token>, PrefixParseFn>,
    infix_parse_fns: HashMap<mem::Discriminant<Token>, InfixParseFn>,
    precedences: HashMap<mem::Discriminant<Token>, Precedence>,
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
            precedences: HashMap::new(),
        };

        // Register prefix parsing functions
        p.register_prefix(mem::discriminant(&Token::Ident("".into())), Parser::parse_identifier_expression);
        p.register_prefix(mem::discriminant(&Token::Int("".into())), Parser::parse_integer_literal_expression);
        p.register_prefix(mem::discriminant(&Token::Function), Parser::parse_function_literal_expression);
        p.register_prefix(mem::discriminant(&Token::String("".into())), Parser::parse_string_literal_expression); // Added
        // TODO: Register other prefix functions like !, -, (, if, true, false

        // Register infix parsing functions
        p.register_infix(mem::discriminant(&Token::LParen), Parser::parse_call_or_instantiation_expression);
        p.register_infix(mem::discriminant(&Token::Dot), Parser::parse_dot_expression);


        // Define precedences
        p.precedences.insert(mem::discriminant(&Token::LParen), Precedence::Call);
        p.precedences.insert(mem::discriminant(&Token::Dot), Precedence::Dot);
        // Add other precedences: +, -, *, /, ==, !=, <, >
        
        p
    }

    fn register_prefix(&mut self, token_type_disc: mem::Discriminant<Token>, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type_disc, func);
    }

    fn register_infix(&mut self, token_type_disc: mem::Discriminant<Token>, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type_disc, func);
    }

    fn peek_precedence(&self) -> Precedence {
        self.precedences.get(&mem::discriminant(&self.peek_token)).cloned().unwrap_or(Precedence::Lowest)
    }

    #[allow(dead_code)] 
    fn cur_precedence(&self) -> Precedence {
        self.precedences.get(&mem::discriminant(&self.cur_token)).cloned().unwrap_or(Precedence::Lowest)
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
                // If parse_statement returns None, it means an error likely occurred,
                // or it's an unimplemented path. To prevent infinite loops on some errors,
                // we advance the token. The statement parser should ideally consume tokens
                // correctly or report errors that halt parsing if needed.
                self.next_token();
            }
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Class => self.parse_class_statement(),
            // TODO: Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }
    
    fn parse_function_literal(&mut self, is_method: bool) -> Option<FunctionLiteral> {
        let fn_token = self.cur_token.clone(); // Token::Function
    
        let name = if is_method {
            if !self.expect_peek_discriminant(mem::discriminant(&Token::Ident("".into()))) {
                self.peek_error_discriminant(mem::discriminant(&Token::Ident("".into())));
                return None;
            }
            match self.cur_token.clone() {
                Token::Ident(val) => Some(val),
                _ => return None, // Should not happen
            }
        } else {
            // For standalone function expressions, a name might be optional or handled differently
            // For now, methods MUST have a name. Standalone anonymous functions not supported yet.
            if self.peek_token_is_discriminant(mem::discriminant(&Token::Ident("".into()))) {
                 self.next_token(); // Consume optional name for now
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
    
        // TODO: Parse parameters here. For now, parameters list is empty.
        let parameters = self.parse_function_parameters()?;
    
        if !self.expect_peek(Token::LBrace) {
            self.peek_error(Token::LBrace);
            return None;
        }
    
        let body = match self.parse_block_statement() {
            Some(bs) => bs,
            None => return None,
        };
        
        // parse_block_statement should consume the RBrace.
        // self.next_token(); // Consume RBrace from block, cur_token is now RBrace
    
        Some(FunctionLiteral {
            token: fn_token,
            parameters,
            body,
            name,
        })
    }

    // Wrapper for parse_function_literal to be used as a prefix parse function
    fn parse_function_literal_expression(&mut self) -> Option<Expression> {
        self.parse_function_literal(false).map(Expression::FunctionLiteral)
    }


    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(Token::RParen) {
            self.next_token(); // consume RParen
            return Some(identifiers);
        }

        self.next_token(); // consume LParen or Comma

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
            self.next_token(); // consume Comma
            self.next_token(); // consume actual Ident token
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
        // expect_peek consumed RParen

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
        
        self.next_token(); // Consume LBrace, cur_token is now LBrace.

        let mut methods = Vec::new();
        while !self.cur_token_is(Token::RBrace) && !self.cur_token_is(Token::Eof) {
            if self.cur_token_is(Token::Function) {
                if let Some(method_fn) = self.parse_function_literal(true) { // true for is_method
                    methods.push(method_fn);
                } else {
                    // Error parsing method, stop class parsing or try to recover
                    self.errors.push("Failed to parse method in class".to_string());
                    // To prevent infinite loop if parse_function_literal didn't advance:
                    // self.next_token(); // This might skip valid tokens if recovery is poor.
                    // For now, let's assume parse_function_literal advances on its own or we stop.
                    return None; // Stop parsing class on method error for now
                }
            } else {
                self.errors.push(format!("Expected 'fn' for method or '}}' to end class, got {:?}", self.cur_token));
                self.next_token(); // Skip unexpected token
                // return None; // Or try to recover by skipping tokens until 'fn' or '}'
            }
        }

        if !self.cur_token_is(Token::RBrace) { 
            self.errors.push(format!("Expected '}}' for class {}, got {:?}", name.value, self.cur_token));
            return None;
        }
        
        self.next_token(); // Consume RBrace

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
        
        self.next_token(); // Consume '='

        let value_expression = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => {
                self.errors.push("Failed to parse expression in let statement".to_string());
                return None;
            }
        };

        if self.peek_token_is(Token::Semicolon) {
            self.next_token(); 
        }
        // self.next_token(); // Let statement does not consume the token after expression/semicolon
                           // This is handled by the main loop in parse_program or by parse_expression_statement
        // After parse_expression, cur_token is the last token of value_expression.
        if self.peek_token_is(Token::Semicolon) {
            self.next_token(); // Consume ';'. cur_token is now ';'.
        }
        // Advance past the statement.
        self.next_token(); 

        Some(Statement::LetStatement {
            token: let_token,
            name,
            value: value_expression,
        })
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let stmt_token = self.cur_token.clone();
        
        let expression_opt = self.parse_expression(Precedence::Lowest);
        
        if self.peek_token_is(Token::Semicolon) {
            self.next_token(); 
        }
        // self.next_token(); // Expression statement also does not consume the token after expression/semicolon

        match expression_opt {
            Some(expression) => {
                // Statement parser should advance cur_token to the next token *after* this statement.
                self.next_token(); 
                Some(Statement::ExpressionStatement {
                    token: stmt_token, 
                    expression,
                })
            }
            None => None
        }
    }
    
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix_fn_opt = self.prefix_parse_fns.get(&mem::discriminant(&self.cur_token)).copied();

        let mut left_exp_opt = match prefix_fn_opt {
            Some(prefix_fn) => prefix_fn(self),
            None => {
                self.no_prefix_parse_fn_error();
                return None;
            }
        };
        
        // After prefix parsing, cur_token is the last token of the prefix expression.
        // The loop for infix parsing should check peek_token.

        while !self.peek_token_is(Token::Semicolon) && precedence < self.peek_precedence() {
             // Check current token, not peek, because prefix parsing already advanced.
             // No, this is wrong. peek_precedence should be for peek_token.
             // The loop condition should be `precedence < self.peek_precedence()`
             // And inside, we use `self.peek_token` for the infix operator.
             //
             // Let's correct the flow:
             // 1. Parse prefix expression. `cur_token` is its last token.
             // 2. Loop while `peek_token` is an infix operator of higher precedence.
             // 3. Inside loop: `next_token()` to make operator `cur_token`.
             // 4. `next_token()` again to make RHS first token `cur_token`.
             // 5. Call infix parse function.

            // Correction: The loop condition `precedence < self.peek_precedence()` is correct.
            // Inside the loop, if we find an infix operator on `peek_token`, we then call `next_token()`
            // to make it `cur_token`, then call the infix parse function.
            // The infix parse function then parses the right-hand side.

            let infix_fn_opt = self.infix_parse_fns.get(&mem::discriminant(&self.peek_token)).copied();
            
            if infix_fn_opt.is_none() {
                return left_exp_opt; 
            }
            
            self.next_token(); // Consume the infix operator, peek_token is now the operator, make it cur_token.
            
            // Check if left_exp_opt is Some before unwrapping
             if left_exp_opt.is_none() { return None; } // Should not happen if prefix parsing succeeded.

            left_exp_opt = infix_fn_opt.unwrap()(self, left_exp_opt.unwrap()); 
                                                                    
            if left_exp_opt.is_none() { return None; } 
            
            // After an infix operation, the infix_parse_fn (e.g. parse_dot_expression, parse_call_or_instantiation_expression)
            // is responsible for consuming all tokens related to the infix operation, including the RHS and any closing tokens.
            // The cur_token should be the last token of the expression parsed by the infix_fn.
            // The loop in parse_expression will then call next_token() at the start of the next iteration IF it continues.
            // So, the extra self.next_token() here was likely incorrect.
            // Removing it. The main loop of parse_expression or its callers should handle advancing.
            // self.next_token(); // REMOVED THIS LINE
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
                // This case should ideally not be reached if called correctly by the dispatcher
                self.errors.push(format!("Expected Token::String, got {:?}", self.cur_token));
                None
            }
        }
    }

    // Infix parsing functions
    fn parse_call_or_instantiation_expression(&mut self, left_expression: Expression) -> Option<Expression> {
        // cur_token is LParen. left_expression is what was before it.
        let arguments = self.parse_expression_list(Token::RParen)?;
        // parse_expression_list consumes up to and including RParen.
        // So cur_token is now RParen.

        match left_expression {
            Expression::Identifier(ident_node) => { // Could be ClassInstantiation or FunctionCall
                // For this subtask, assume Identifier() is ClassInstantiation.
                // A more robust solution would check if 'ident_node.value' is a known class name.
                // Or, have separate parsing paths if grammar allows distinguishing.
                // For now, if it's an Identifier, we treat it as potential class instantiation.
                 Some(Expression::ClassInstantiation {
                    token: ident_node.token.clone(), 
                    name: ident_node.clone(),
                    arguments, 
                })
                // TODO: Later, differentiate true function calls like `myFunc()` from `MyClass()`
            }
            _ => {
                // TODO: Handle other callable expressions e.g. (fn(){})()
                self.errors.push(format!("Calling non-identifier/non-function not yet supported. Got: {:?}", left_expression));
                None
            }
        }
    }

    fn parse_dot_expression(&mut self, left_expression: Expression) -> Option<Expression> {
        // cur_token is Token::Dot. left_expression is the object.
        let dot_token = self.cur_token.clone();
    
        if !self.expect_peek_discriminant(mem::discriminant(&Token::Ident("".into()))) {
            self.peek_error_discriminant(mem::discriminant(&Token::Ident("".into())));
            return None;
        }
        // cur_token is now the Identifier (method name)
        let method_name = match self.cur_token.clone() {
            Token::Ident(val) => Identifier { token: self.cur_token.clone(), value: val },
            _ => return None, // Should not happen
        };
    
        if !self.expect_peek(Token::LParen) {
            // This means it's potentially a field access like `object.field`
            // For this subtask, field access is an error or not implemented.
            self.errors.push(format!("Field access (e.g. object.field) is not yet supported. Expected '(' for method call after '.{}'.", method_name.value));
            return None;
        }
        // cur_token is now LParen
    
        let arguments = self.parse_expression_list(Token::RParen)?;
        // cur_token is now RParen after parse_expression_list
    
        Some(Expression::MethodCall {
            token: dot_token,
            object: Box::new(left_expression),
            name: method_name,
            arguments,
        })
    }
    
    // Renamed from parse_call_arguments to parse_expression_list for generality
    fn parse_expression_list(&mut self, end_token: Token) -> Option<Vec<Expression>> {
        let mut list = Vec::new();

        if self.peek_token_is(end_token.clone()) { 
            self.next_token(); 
            return Some(list); 
        }

        self.next_token(); // Consume LParen (or Comma for subsequent args)
        
        if let Some(exp) = self.parse_expression(Precedence::Lowest) {
            list.push(exp);
        } else {
            self.errors.push("Failed to parse expression in list".to_string());
            return None; 
        }
        
        // After parsing the first expression, cur_token is its last token.
        // We need to advance to check for comma.
        self.next_token();


        while self.cur_token_is(Token::Comma) { // Check cur_token for Comma
            self.next_token(); // Consume Comma, cur_token is now the start of the next expression
            if let Some(exp) = self.parse_expression(Precedence::Lowest) {
                list.push(exp);
            } else {
                 self.errors.push("Failed to parse expression after comma in list".to_string());
                return None; 
            }
            // Advance after parsing the expression in the list
            self.next_token();
        }

        if !self.cur_token_is(end_token.clone()) { // After loop, check cur_token is end_token
            self.errors.push(format!("Expected {:?} to end expression list, got {:?}", end_token, self.cur_token));
            return None;
        }
        // self.next_token(); // Consume the end_token - This is done by the caller of parse_expression_list (e.g. parse_call_or_instantiation)
                           // No, parse_expression_list itself should consume the end_token.
                           // The caller (parse_call_or_instantiation / parse_dot_expression) expects cur_token
                           // to be the end_token (RParen) when this function returns.

        Some(list)
    }


    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        let block_token = self.cur_token.clone(); 
        let mut statements = Vec::new();

        self.next_token(); // Consume '{'

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
        self.next_token(); // Consume '}'

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
