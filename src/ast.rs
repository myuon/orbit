#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Int(i64),
    Boolean(bool),
    String(String),
    Byte(u8),
    Identifier(String),
    Let,
    Assign,
    Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
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
    Vec,
    Push,         // <-
    LeftBracket,  // [
    RightBracket, // ]
    LeftBrace,    // {
    RightBrace,   // }
    // Map keywords
    Map,
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
    Vector,
    Map,
    Pointer,
    String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StructNewKind {
    Regular, // new Type { ... }
    Pattern, // new(struct) Type { ... }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AllocKind {
    Sized,   // alloc(type, size)
    Pointer, // pointer(type, [values])
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Boolean(bool),
    String(String),
    Byte(u8),
    Identifier(String),
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    VectorNew {
        element_type: String,
        initial_values: Vec<Expr>,
    },
    Index {
        container: Box<Expr>,
        index: Box<Expr>,
        container_type: Option<IndexContainerType>,
    },
    MapNew {
        key_type: String,
        value_type: String,
        initial_pairs: Vec<(Expr, Expr)>,
    },
    StructNew {
        type_name: String,
        fields: Vec<(String, Expr)>,
        kind: StructNewKind,
    },
    FieldAccess {
        object: Box<Expr>,
        field: String,
    },
    MethodCall {
        object: Option<Box<Expr>>, // None for associated calls (Type::method), Some for instance calls (obj.method)
        type_name: Option<String>, // Type name for associated calls, filled by type checker for instance calls
        method: String,
        args: Vec<Expr>,
    },
    TypeExpr {
        type_name: String,
    },
    Alloc {
        element_type: String,
        kind: AllocKind,
        size: Option<Box<Expr>>,           // Some for Sized, None for Pointer
        initial_values: Option<Vec<Expr>>, // None for Sized, Some for Pointer
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunParam {
    pub name: String,
    pub type_name: Option<String>,
}

// Top-level program structure
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub declarations: Vec<Decl>,
}

// Top-level declarations
#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Function(Function),
    Struct(StructDecl),
    GlobalVariable(GlobalVariable),
}

// Global variable declaration
#[derive(Debug, Clone, PartialEq)]
pub struct GlobalVariable {
    pub name: String,
    pub value: Expr,
}

// Struct declaration
#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub name: String,
    pub type_params: Vec<String>, // Generic type parameters
    pub fields: Vec<StructField>,
    pub methods: Vec<Function>,
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
    pub type_name: String,
}

// Function declaration with body
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub type_params: Vec<String>, // Generic type parameters
    pub params: Vec<FunParam>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        name: String,
        value: Expr,
    },
    Expression(Expr),
    Return(Expr),
    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },
    Assign {
        name: String,
        value: Expr,
    },
    VectorPush {
        vector: String,
        value: Expr,
    },
    IndexAssign {
        container: String,
        index: Expr,
        value: Expr,
        container_type: Option<IndexContainerType>,
    },
    FieldAssign {
        object: Expr,
        field: String,
        value: Expr,
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
    Vector(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Struct(String),
    Pointer(Box<Type>),
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Generic {
        name: String,
        args: Vec<Type>,
    },
    TypeParameter(String),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Int => write!(f, "int"),
            Type::Boolean => write!(f, "boolean"),
            Type::String => write!(f, "string"),
            Type::Byte => write!(f, "byte"),
            Type::Vector(elem_type) => write!(f, "vec({})", elem_type),
            Type::Map(key_type, value_type) => write!(f, "map({}, {})", key_type, value_type),
            Type::Struct(name) => write!(f, "{}", name),
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
            Type::Generic { name, args } => {
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
            Type::TypeParameter(name) => write!(f, "{}", name),
        }
    }
}
