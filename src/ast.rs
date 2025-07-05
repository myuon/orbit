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
    Fun,
    Do,
    End,
    Return,
    Comma,
    Colon,
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
    // Comparison operators
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunParam {
    pub name: String,
    pub type_name: Option<String>,
}

impl FunParam {
    pub fn new(name: String, type_name: Option<String>) -> Self {
        FunParam { name, type_name }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        name: String,
        value: Expr,
    },
    Expression(Expr),
    Fun {
        name: String,
        params: Vec<FunParam>,
        body: Vec<Stmt>,
        return_expr: Option<Box<Expr>>,
    },
    Return(Expr),
    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },
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

    pub fn call(callee: Expr, args: Vec<Expr>) -> Self {
        Expr::Call {
            callee: Box::new(callee),
            args,
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

    pub fn fun_stmt(name: String, params: Vec<FunParam>, body: Vec<Stmt>, return_expr: Option<Expr>) -> Self {
        Stmt::Fun {
            name,
            params,
            body,
            return_expr: return_expr.map(Box::new),
        }
    }

    pub fn return_stmt(expr: Expr) -> Self {
        Stmt::Return(expr)
    }

    pub fn if_stmt(condition: Expr, then_branch: Vec<Stmt>, else_branch: Option<Vec<Stmt>>) -> Self {
        Stmt::If {
            condition,
            then_branch,
            else_branch,
        }
    }
}