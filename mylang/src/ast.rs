use crate::token::Token;
use std::any::Any;
use std::fmt::Debug; // Import Debug

// Trait for all AST nodes
pub trait Node: Debug { // Add Debug as a supertrait
    fn token_literal(&self) -> String;
    fn string(&self) -> String; // For debugging and testing
    fn as_any(&self) -> &dyn Any;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub token: Token, // Token::Ident
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.to_string() // Updated
    }
    fn string(&self) -> String {
        self.value.clone()
    }
    fn as_any(&self) -> &dyn Any { self }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    pub token: Token, // The '{' token
    pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.to_string() 
    }
    fn string(&self) -> String {
        let mut out = String::new();
        for s in &self.statements {
            out.push_str(&s.string());
        }
        out
    }
    fn as_any(&self) -> &dyn Any { self }
}


#[derive(Debug, PartialEq, Clone)]
pub struct FunctionLiteral {
    pub token: Token, // The 'fn' token
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub name: Option<String>, // For named functions, including methods
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.to_string() // Updated
    }
    fn string(&self) -> String {
        let mut out = String::new();
        out.push_str(self.token_literal().as_str()); // Uses the updated token_literal
        if let Some(name) = &self.name {
            out.push_str(&format!("<{}>", name));
        }
        out.push('(');
        let params: Vec<String> = self.parameters.iter().map(|p| p.string()).collect();
        out.push_str(&params.join(", "));
        out.push_str(") ");
        out.push_str(&self.body.string());
        out
    }
    fn as_any(&self) -> &dyn Any { self }
}


#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier), // Identifier itself holds a token
    IntegerLiteral { token: Token, value: i64 },
    FunctionLiteral(FunctionLiteral), // FunctionLiteral itself holds a token
    ClassInstantiation { 
        token: Token, // Identifier token of the class name
        name: Identifier, 
        arguments: Vec<Expression>, 
    },
    MethodCall { 
        token: Token, // '.' token
        object: Box<Expression>, 
        name: Identifier, 
        arguments: Vec<Expression>, 
    },
    StringLiteral { 
        token: Token, // Token::String
        value: String,
    },
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(ident) => ident.token.to_string(), // Updated: access token from Identifier
            Expression::IntegerLiteral { token, .. } => token.to_string(), // Updated
            Expression::FunctionLiteral(fl) => fl.token.to_string(), // Updated: access token from FunctionLiteral
            Expression::ClassInstantiation { token, .. } => token.to_string(), // Updated
            Expression::MethodCall { token, .. } => token.to_string(), // Updated
            Expression::StringLiteral { token, .. } => token.to_string(), // Updated
        }
    }
    fn string(&self) -> String {
        match self {
            Expression::Identifier(ident) => ident.string(),
            Expression::IntegerLiteral { value, .. } => value.to_string(),
            Expression::FunctionLiteral(fl) => fl.string(),
            Expression::ClassInstantiation { name, arguments, .. } => {
                let args_str: Vec<String> = arguments.iter().map(|a| a.string()).collect();
                format!("{}({})", name.string(), args_str.join(", "))
            }
            Expression::MethodCall { object, name, arguments, .. } => {
                let args_str: Vec<String> = arguments.iter().map(|a| a.string()).collect();
                format!("{}.{}({})", object.string(), name.string(), args_str.join(", "))
            }
            Expression::StringLiteral { value, .. } => format!("\"{}\"", value), // For string(), show quotes
        }
    }
    fn as_any(&self) -> &dyn Any { self }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetStatement {
        token: Token, // Token::Let
        name: Identifier,
        value: Expression,
    },
    ExpressionStatement {
        token: Token, // The first token of the expression
        expression: Expression,
    },
    ClassStatement { 
        token: Token, // Token::Class
        name: Identifier,
        methods: Vec<FunctionLiteral>, 
    },
    // TODO: Add other statement types like ReturnStatement
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::LetStatement { token, .. } => token.to_string(), // Updated
            Statement::ExpressionStatement { token, .. } => token.to_string(), // Updated (assuming this token is representative)
                                                                                // Alternatively, could be expression.token_literal() if that's more appropriate.
                                                                                // The prompt implies using the statement's own token if it has one.
            Statement::ClassStatement { token, .. } => token.to_string(), // Updated
        }
    }
    fn string(&self) -> String {
        match self {
            Statement::LetStatement { name, value, .. } => {
                format!(
                    "let {} = {};",
                    name.string(),
                    value.string()
                )
            }
            Statement::ExpressionStatement { expression, .. } => expression.string(),
            Statement::ClassStatement { name, methods, .. } => {
                let mut out = String::new();
                out.push_str("class ");
                out.push_str(&name.string());
                out.push_str(" {");
                for method in methods {
                    out.push_str(&method.string()); 
                }
                out.push_str(" }");
                out
            }
        }
    }
    fn as_any(&self) -> &dyn Any { self }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            self.statements[0].token_literal() // Delegates to the first statement's token_literal
        } else {
            "".to_string()
        }
    }
    fn string(&self) -> String {
        self.statements
            .iter()
            .map(|s| s.string())
            .collect::<Vec<String>>()
            .join("\n")
    }
    fn as_any(&self) -> &dyn Any { self }
}
