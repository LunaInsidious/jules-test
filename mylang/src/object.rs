use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::ast::{Identifier, BlockStatement, Node}; // Added Node
use crate::environment::Environment;      

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ObjectType {
    IntegerObj,
    NullObj,
    ClassObj,    
    InstanceObj, 
    FunctionObj, // Added for completeness
    ReturnValueObj, // Will be needed soon
    ErrorObj,       // For error handling
    StringObj,      // For string literals
    BooleanObj,     // Added for Boolean objects
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ObjectType::IntegerObj => write!(f, "INTEGER"),
            ObjectType::NullObj => write!(f, "NULL"),
            ObjectType::ClassObj => write!(f, "CLASS"),
            ObjectType::InstanceObj => write!(f, "INSTANCE"),
            ObjectType::FunctionObj => write!(f, "FUNCTION"),
            ObjectType::ReturnValueObj => write!(f, "RETURN_VALUE"),
            ObjectType::ErrorObj => write!(f, "ERROR"),
            ObjectType::StringObj => write!(f, "STRING"),
            ObjectType::BooleanObj => write!(f, "BOOLEAN"), // Added
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Integer {
    pub value: i64,
}

#[derive(Debug, Clone)]
pub struct FunctionObject {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>, // Changed to Rc<RefCell<Environment>>
    pub name: Option<String>, 
}
impl PartialEq for FunctionObject {
    fn eq(&self, other: &Self) -> bool {
        self.parameters == other.parameters && 
        self.body.string() == other.body.string() &&
        self.name == other.name &&
        Rc::ptr_eq(&self.env, &other.env) // Compare environments by Rc pointer equality
    }
}


#[derive(Debug, Clone)] 
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, Rc<FunctionObject>>, 
}
impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.methods == other.methods
    }
}


#[derive(Debug, Clone)] 
pub struct Instance {
    // pub class_name: String, // Replaced
    pub class: Rc<Class>, // Changed to hold Rc<Class> directly
    pub fields: Rc<RefCell<HashMap<String, Object>>>, 
}
impl PartialEq for Instance {
    fn eq(&self, other: &Self) -> bool {
        // self.class_name == other.class_name && Rc::ptr_eq(&self.fields, &other.fields)
        Rc::ptr_eq(&self.class, &other.class) && Rc::ptr_eq(&self.fields, &other.fields)
    }
}

// For String objects
#[derive(Debug, PartialEq, Clone)]
pub struct StringObject {
    pub value: String,
}

// Define BooleanObject struct (Step 2)
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct BooleanObject {
    pub value: bool,
}


#[derive(Debug, Clone, PartialEq)] 
pub enum Object {
    Integer(Integer),
    Null,
    Class(Rc<Class>),          
    Instance(Rc<Instance>),    
    Function(Rc<FunctionObject>),
    ReturnValue(Box<Object>), 
    Error(String),            
    String(StringObject),     
    Boolean(BooleanObject), // Added Boolean variant (Step 3)
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => format!("{}", i.value),
            Object::Null => "null".to_string(),
            Object::Class(c) => format!("<class {}>", c.name),
            Object::Instance(i) => format!("<instance of {}>", i.class.name), 
            Object::Function(f) => {
                let params: Vec<String> = f.parameters.iter().map(|p| p.value.clone()).collect();
                let name_prefix = f.name.as_ref().map_or_else(|| "fn".to_string(), |n| format!("fn {}", n));
                format!("{}({}) {{ ... }}", name_prefix, params.join(", "))
            }
            Object::ReturnValue(val) => format!("ReturnValue({})", val.inspect()),
            Object::Error(msg) => format!("Error: {}", msg),
            Object::String(s) => format!("\"{}\"", s.value),
            Object::Boolean(b) => b.value.to_string(), // Updated for Boolean (Step 4)
        }
    }

    pub fn object_type(&self) -> ObjectType {
        match self {
            Object::Integer(_) => ObjectType::IntegerObj,
            Object::Null => ObjectType::NullObj,
            Object::Class(_) => ObjectType::ClassObj,
            Object::Instance(_) => ObjectType::InstanceObj,
            Object::Function(_) => ObjectType::FunctionObj, 
            Object::ReturnValue(_) => ObjectType::ReturnValueObj,
            Object::Error(_) => ObjectType::ErrorObj,
            Object::String(_) => ObjectType::StringObj,
            Object::Boolean(_) => ObjectType::BooleanObj, // Updated for Boolean (Step 5)
        }
    }
}
