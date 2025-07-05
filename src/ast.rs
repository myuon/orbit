#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Number(f64),
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

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
}

impl Expr {
    pub fn number(value: f64) -> Self {
        Expr::Number(value)
    }

    pub fn binary(left: Expr, op: BinaryOp, right: Expr) -> Self {
        Expr::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }
}