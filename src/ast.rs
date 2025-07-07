#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Number(f64),
    Boolean(bool),
    String(String),
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

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Boolean(bool),
    String(String),
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
    PointerNew {
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
    },
    FieldAccess {
        object: Box<Expr>,
        field: String,
    },
    MethodCall {
        object: Box<Expr>,
        method: String,
        args: Vec<Expr>,
        object_type: Option<String>, // Struct type name, filled by type checker
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
    Generic {
        name: String,
        args: Vec<TypeExpr>,
    },
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
    Number,
    Boolean,
    String,
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
            Type::Number => write!(f, "number"),
            Type::Boolean => write!(f, "boolean"),
            Type::String => write!(f, "string"),
            Type::Vector(elem_type) => write!(f, "vec({})", elem_type),
            Type::Map(key_type, value_type) => write!(f, "map({}, {})", key_type, value_type),
            Type::Struct(name) => write!(f, "{}", name),
            Type::Pointer(elem_type) => write!(f, "[*]{}", elem_type),
            Type::Function { params, return_type } => {
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
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Type::TypeParameter(name) => write!(f, "{}", name),
        }
    }
}
