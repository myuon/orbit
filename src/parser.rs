use crate::ast::{BinaryOp, Expr, Token, TokenType};
use anyhow::{bail, Result};

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    eof_token: Token,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { 
            tokens, 
            position: 0,
            eof_token: Token::new(TokenType::Eof, 0),
        }
    }

    fn current_token(&self) -> &Token {
        self.tokens.get(self.position).unwrap_or(&self.eof_token)
    }

    fn advance(&mut self) {
        if self.position < self.tokens.len() {
            self.position += 1;
        }
    }

    fn consume(&mut self, expected: TokenType) -> Result<()> {
        if std::mem::discriminant(&self.current_token().token_type) == std::mem::discriminant(&expected) {
            self.advance();
            Ok(())
        } else {
            bail!("Expected {:?}, found {:?}", expected, self.current_token().token_type)
        }
    }

    pub fn parse(&mut self) -> Result<Expr> {
        self.parse_expression()
    }

    fn parse_expression(&mut self) -> Result<Expr> {
        self.parse_term()
    }

    fn parse_term(&mut self) -> Result<Expr> {
        let mut left = self.parse_factor()?;

        loop {
            match &self.current_token().token_type {
                TokenType::Plus => {
                    self.advance();
                    let right = self.parse_factor()?;
                    left = Expr::binary(left, BinaryOp::Add, right);
                }
                TokenType::Minus => {
                    self.advance();
                    let right = self.parse_factor()?;
                    left = Expr::binary(left, BinaryOp::Subtract, right);
                }
                _ => break,
            }
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Expr> {
        let mut left = self.parse_primary()?;

        loop {
            match &self.current_token().token_type {
                TokenType::Star => {
                    self.advance();
                    let right = self.parse_primary()?;
                    left = Expr::binary(left, BinaryOp::Multiply, right);
                }
                TokenType::Slash => {
                    self.advance();
                    let right = self.parse_primary()?;
                    left = Expr::binary(left, BinaryOp::Divide, right);
                }
                _ => break,
            }
        }

        Ok(left)
    }

    fn parse_primary(&mut self) -> Result<Expr> {
        match &self.current_token().token_type {
            TokenType::Number(value) => {
                let num = *value;
                self.advance();
                Ok(Expr::number(num))
            }
            TokenType::Boolean(value) => {
                let bool_val = *value;
                self.advance();
                Ok(Expr::boolean(bool_val))
            }
            TokenType::String(value) => {
                let string_val = value.clone();
                self.advance();
                Ok(Expr::string(string_val))
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.consume(TokenType::RightParen)?;
                Ok(expr)
            }
            _ => bail!("Unexpected token: {:?}", self.current_token().token_type),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse_expression(input: &str) -> Result<Expr> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize()?;
        let mut parser = Parser::new(tokens);
        parser.parse()
    }

    #[test]
    fn test_parse_number() {
        let expr = parse_expression("42").unwrap();
        assert_eq!(expr, Expr::Number(42.0));
    }

    #[test]
    fn test_parse_addition() {
        let expr = parse_expression("2 + 3").unwrap();
        assert_eq!(expr, Expr::binary(Expr::Number(2.0), BinaryOp::Add, Expr::Number(3.0)));
    }

    #[test]
    fn test_parse_multiplication() {
        let expr = parse_expression("2 * 3").unwrap();
        assert_eq!(expr, Expr::binary(Expr::Number(2.0), BinaryOp::Multiply, Expr::Number(3.0)));
    }

    #[test]
    fn test_parse_precedence() {
        let expr = parse_expression("2 + 3 * 4").unwrap();
        let expected = Expr::binary(
            Expr::Number(2.0),
            BinaryOp::Add,
            Expr::binary(Expr::Number(3.0), BinaryOp::Multiply, Expr::Number(4.0))
        );
        assert_eq!(expr, expected);
    }

    #[test]
    fn test_parse_parentheses() {
        let expr = parse_expression("(2 + 3) * 4").unwrap();
        let expected = Expr::binary(
            Expr::binary(Expr::Number(2.0), BinaryOp::Add, Expr::Number(3.0)),
            BinaryOp::Multiply,
            Expr::Number(4.0)
        );
        assert_eq!(expr, expected);
    }

    #[test]
    fn test_parse_complex_expression() {
        let expr = parse_expression("2 * 3 + 4 / 2").unwrap();
        let expected = Expr::binary(
            Expr::binary(Expr::Number(2.0), BinaryOp::Multiply, Expr::Number(3.0)),
            BinaryOp::Add,
            Expr::binary(Expr::Number(4.0), BinaryOp::Divide, Expr::Number(2.0))
        );
        assert_eq!(expr, expected);
    }
}