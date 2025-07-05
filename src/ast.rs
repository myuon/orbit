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
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub position: usize,
}

impl Token {
    pub fn new(token_type: TokenType, position: usize) -> Self {
        Token { token_type, position }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        name: String,
        value: Expr,
    },
    Expression(Expr),
}

impl Expr {
    pub fn number(value: f64) -> Self {
        Expr::Number(value)
    }

    pub fn boolean(value: bool) -> Self {
        Expr::Boolean(value)
    }

    pub fn string(value: String) -> Self {
        Expr::String(value)
    }

    pub fn identifier(name: String) -> Self {
        Expr::Identifier(name)
    }

    pub fn binary(left: Expr, op: BinaryOp, right: Expr) -> Self {
        Expr::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
}

impl Stmt {
    pub fn let_stmt(name: String, value: Expr) -> Self {
        Stmt::Let { name, value }
    }

    pub fn expression(expr: Expr) -> Self {
        Stmt::Expression(expr)
    }
}