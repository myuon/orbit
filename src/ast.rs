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

#[derive(Debug, Clone, PartialEq)]
pub enum IndexContainerType {
    Vector,
    Map,
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
}

// Struct declaration
#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub name: String,
    pub fields: Vec<StructField>,
}

// Struct field
#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: String,
    pub type_name: String,
}

// Function declaration with body
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
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
}
