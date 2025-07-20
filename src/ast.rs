#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: Option<usize>,
    pub end: Option<usize>,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start: Some(start),
            end: Some(end),
        }
    }

    pub fn unknown() -> Self {
        Self {
            start: None,
            end: None,
        }
    }

    pub fn single(pos: usize) -> Self {
        Self {
            start: Some(pos),
            end: Some(pos),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Positioned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Positioned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    pub fn with_unknown_span(value: T) -> Self {
        Self {
            value,
            span: Span::unknown(),
        }
    }
}

/// Custom error type with span information for better error handling
#[derive(Debug, Clone)]
pub struct CompilerError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for CompilerError {}

impl CompilerError {
    /// Create a compiler error with a span
    pub fn new_with_span(message: String, span: Span) -> Self {
        Self { message, span }
    }

    /// Create a compiler error without span information
    pub fn new_without_span(message: String) -> Self {
        Self {
            message,
            span: Span::unknown(),
        }
    }

    /// Convert to anyhow::Error
    pub fn into_anyhow(self) -> anyhow::Error {
        anyhow::anyhow!(self)
    }
}

/// Type alias for positioned errors - errors with span information
pub type PositionedError = Positioned<anyhow::Error>;

impl PositionedError {
    /// Create a positioned error with a span
    pub fn new_with_span(message: String, span: Span) -> Self {
        Self {
            value: anyhow::anyhow!(message),
            span,
        }
    }

    /// Create a positioned error from an anyhow::Error and span
    pub fn from_error_with_span(error: anyhow::Error, span: Span) -> Self {
        Self { value: error, span }
    }

    /// Create a positioned error without span information
    pub fn new_without_span(message: String) -> Self {
        Self {
            value: anyhow::anyhow!(message),
            span: Span::unknown(),
        }
    }
}

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
    Vector,
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
    VectorNew {
        element_type: Type,
        initial_values: Vec<PositionedExpr>,
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
    },
    MethodCall {
        object: Option<Box<PositionedExpr>>, // None for associated calls (Type::method), Some for instance calls (obj.method)
        type_name: Option<String>, // Type name for associated calls, filled by type checker for instance calls
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
