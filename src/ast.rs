// Re-export position utilities from utils module
pub use crate::utils::positioned::{Positioned, PositionedError, Span};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Int(i64),
    Boolean(bool),
    String(String),
    Byte(u8),
    Identifier(String),
    Let,
    Assign,
    As,
    Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
    Modulo,
    LeftParen,
    RightParen,
    // Comparison operators
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    // Conditional keywords
    If,
    Then,
    Else,
    // Loop keywords
    While,
    // Vector keywords
    New,
    Push,         // <-
    LeftBracket,  // [
    RightBracket, // ]
    LeftBrace,    // {
    RightBrace,   // }
    Fun,
    Do,
    End,
    Return,
    Comma,
    Colon,
    // Struct keywords
    Type,
    Struct,
    Dot,
    // Pointer keyword
    Pointer,
    // Memory allocation
    Alloc,
    // Type size operator
    Sizeof,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub position: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    // Comparison operators
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IndexContainerType {
    Pointer,
    String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StructNewKind {
    Regular, // new Type { ... }
    Pattern, // new(struct) Type { ... }
}

pub type PositionedExpr = Positioned<Expr>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Boolean(bool),
    String(String),
    Byte(u8),
    Identifier(String),
    Binary {
        left: Box<PositionedExpr>,
        op: BinaryOp,
        right: Box<PositionedExpr>,
    },
    Call {
        callee: Box<PositionedExpr>,
        args: Vec<PositionedExpr>,
    },
    Index {
        container: Box<PositionedExpr>,
        index: Box<PositionedExpr>,
        container_type: Option<IndexContainerType>,
        container_value_type: Option<Type>,
    },
    StructNew {
        type_name: Type,
        fields: Vec<(String, PositionedExpr)>,
        kind: StructNewKind,
    },
    FieldAccess {
        object: Box<PositionedExpr>,
        field: String,
        object_type: Option<Type>, // Type of the object being accessed, filled by type checker
    },
    MethodCall {
        object: Option<Box<PositionedExpr>>, // None for associated calls (Type::method), Some for instance calls (obj.method)
        object_type: Option<Type>, // Type for associated calls, filled by type checker for instance calls
        method: String,
        args: Vec<PositionedExpr>,
    },
    TypeExpr {
        type_name: Type,
    },
    Alloc {
        element_type: Type,
        size: Box<PositionedExpr>,
    },
    Sizeof {
        type_name: Type,
    },
    Cast {
        expr: Box<PositionedExpr>,
        target_type: Type,
    },
    PushString(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunParam {
    pub name: String,
    pub param_type: Option<Type>,
}

// Top-level program structure
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub declarations: Vec<PositionedDecl>,
}

// Top-level declarations
pub type PositionedDecl = Positioned<Decl>;

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Function(PositionedFunction),
    Struct(PositionedStructDecl),
    GlobalVariable(PositionedGlobalVariable),
}

// Global variable declaration
pub type PositionedGlobalVariable = Positioned<GlobalVariable>;

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalVariable {
    pub name: String,
    pub value: Option<PositionedExpr>, // Optional initialization
}

// Struct declaration
pub type PositionedStructDecl = Positioned<StructDecl>;

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub name: String,
    pub type_params: Vec<Type>, // Generic type parameters
    pub fields: Vec<StructField>,
    pub methods: Vec<PositionedFunction>,
}

// Struct field
#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    Simple(String),
    Pointer(Box<TypeExpr>),
    Generic { name: String, args: Vec<TypeExpr> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: String,
    pub field_type: Type,
}

// Function declaration with body
pub type PositionedFunction = Positioned<Function>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub type_params: Vec<Type>, // Generic type parameters
    pub params: Vec<FunParam>,
    pub body: Vec<PositionedStmt>,
}

pub type PositionedStmt = Positioned<Stmt>;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        name: String,
        value: PositionedExpr,
    },
    Expression(PositionedExpr),
    Return(PositionedExpr),
    If {
        condition: PositionedExpr,
        then_branch: Vec<PositionedStmt>,
        else_branch: Option<Vec<PositionedStmt>>,
    },
    While {
        condition: PositionedExpr,
        body: Vec<PositionedStmt>,
    },
    Assign {
        lvalue: PositionedExpr,
        value: PositionedExpr,
    },
    VectorPush {
        vector: String,
        value: PositionedExpr,
        vector_type: Option<Type>,
    },
}

// Type system for semantic analysis
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unknown,
    Int,
    Boolean,
    String,
    Byte,
    Struct {
        name: String,
        args: Vec<Type>,
    },
    Pointer(Box<Type>),
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    TypeParameter(String),
}

impl Type {
    /// Separator used for method name mangling
    pub const METHOD_SEPARATOR: &'static str = "#";

    pub fn sizeof(&self) -> usize {
        match self {
            Type::Boolean => 1,
            Type::Byte => 1,
            Type::Int => 8,
            Type::String => 8,           // String is a pointer, so 8 bytes
            Type::Pointer(_) => 8,       // Pointer types are 8 bytes
            Type::Struct { .. } => 8,    // For struct types, default to 8 bytes
            Type::Function { .. } => 8,  // Function pointers are 8 bytes
            Type::TypeParameter(_) => 8, // Type parameters default to 8 bytes
            Type::Unknown => 8,          // Unknown types default to 8 bytes
        }
    }

    /// Generate a mangled method name from this type and a method name
    /// Example: Type::Struct("Point", []) + "sum" -> "Point#sum"
    /// Example: Type::Struct("Container", [Type::Int]) + "get" -> "Container(int)#get"
    pub fn mangle_method_name(&self, method_name: &str) -> String {
        format!("{}{}{}", self, Self::METHOD_SEPARATOR, method_name)
    }

    /// Create a simple struct type from a name (for legacy compatibility)
    /// Example: Type::from_struct_name("Point") -> Type::Struct { name: "Point", args: [] }
    pub fn from_struct_name(name: &str) -> Self {
        Type::Struct {
            name: name.to_string(),
            args: vec![],
        }
    }

    /// Substitute type parameters with concrete types
    /// Example: Type::TypeParameter("T") with {"T": Type::Int} -> Type::Int
    pub fn substitute(&self, substitutions: &std::collections::HashMap<String, Type>) -> Type {
        match self {
            Type::TypeParameter(param_name) => {
                // Replace type parameter with concrete type if substitution exists
                substitutions
                    .get(param_name)
                    .cloned()
                    .unwrap_or_else(|| self.clone())
            }
            Type::Struct { name, args } => {
                // Recursively substitute type arguments
                let substituted_args = args
                    .iter()
                    .map(|arg| arg.substitute(substitutions))
                    .collect();
                Type::Struct {
                    name: name.clone(),
                    args: substituted_args,
                }
            }
            Type::Pointer(inner_type) => {
                Type::Pointer(Box::new(inner_type.substitute(substitutions)))
            }
            Type::Function {
                params,
                return_type,
            } => {
                // Recursively substitute function parameter and return types
                let substituted_params = params
                    .iter()
                    .map(|param| param.substitute(substitutions))
                    .collect();
                let substituted_return_type = Box::new(return_type.substitute(substitutions));
                Type::Function {
                    params: substituted_params,
                    return_type: substituted_return_type,
                }
            }
            // Primitive types don't need substitution
            Type::Int | Type::Boolean | Type::String | Type::Byte | Type::Unknown => self.clone(),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Int => write!(f, "int"),
            Type::Boolean => write!(f, "boolean"),
            Type::String => write!(f, "string"),
            Type::Byte => write!(f, "byte"),
            Type::Struct { name, args } => {
                if args.is_empty() {
                    write!(f, "{}", name)
                } else {
                    write!(f, "{}(", name)?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        // Use source-friendly names for common types in generic contexts
                        match arg {
                            Type::Int => write!(f, "int")?,
                            Type::Boolean => write!(f, "bool")?,
                            Type::String => write!(f, "string")?,
                            Type::Byte => write!(f, "byte")?,
                            other => write!(f, "{}", other)?,
                        }
                    }
                    write!(f, ")")
                }
            }
            Type::Pointer(elem_type) => write!(f, "[*]{}", elem_type),
            Type::Function {
                params,
                return_type,
            } => {
                write!(f, "fun(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, "): {}", return_type)
            }
            Type::TypeParameter(name) => write!(f, "{}", name),
        }
    }
}
