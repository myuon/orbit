use crate::ast::{BinaryOp, Expr, FunParam, Stmt, Token, TokenType};
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

    pub fn parse_stmt(&mut self) -> Result<Stmt> {
        match &self.current_token().token_type {
            TokenType::Let => self.parse_let_stmt(),
            TokenType::Fun => self.parse_fun_stmt(),
            TokenType::Return => self.parse_return_stmt(),
            _ => {
                let expr = self.parse_expression()?;
                Ok(Stmt::expression(expr))
            }
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt> {
        self.consume(TokenType::Let)?;
        
        let name = match &self.current_token().token_type {
            TokenType::Identifier(name) => {
                let n = name.clone();
                self.advance();
                n
            }
            _ => bail!("Expected identifier after 'let'"),
        };

        self.consume(TokenType::Assign)?;
        let value = self.parse_expression()?;
        self.consume(TokenType::Semicolon)?;
        
        Ok(Stmt::let_stmt(name, value))
    }

    fn parse_fun_stmt(&mut self) -> Result<Stmt> {
        self.consume(TokenType::Fun)?;
        
        let name = match &self.current_token().token_type {
            TokenType::Identifier(name) => {
                let n = name.clone();
                self.advance();
                n
            }
            _ => bail!("Expected function name after 'fun'"),
        };

        self.consume(TokenType::LeftParen)?;
        let mut params = Vec::new();
        
        if !matches!(self.current_token().token_type, TokenType::RightParen) {
            loop {
                let param_name = match &self.current_token().token_type {
                    TokenType::Identifier(name) => {
                        let n = name.clone();
                        self.advance();
                        n
                    }
                    _ => bail!("Expected parameter name"),
                };

                let type_name = if matches!(self.current_token().token_type, TokenType::Colon) {
                    self.advance();
                    match &self.current_token().token_type {
                        TokenType::Identifier(type_name) => {
                            let t = type_name.clone();
                            self.advance();
                            Some(t)
                        }
                        _ => bail!("Expected type name after ':'"),
                    }
                } else {
                    None
                };

                params.push(FunParam::new(param_name, type_name));

                if matches!(self.current_token().token_type, TokenType::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen)?;
        self.consume(TokenType::Do)?;
        
        let mut body = Vec::new();
        let mut return_expr = None;
        
        while !matches!(self.current_token().token_type, TokenType::End) {
            if matches!(self.current_token().token_type, TokenType::Eof) {
                bail!("Unexpected end of file in function body");
            }
            
            if matches!(self.current_token().token_type, TokenType::Return) {
                let return_stmt = self.parse_return_stmt()?;
                if let Stmt::Return(expr) = return_stmt {
                    return_expr = Some(expr);
                }
            } else {
                body.push(self.parse_stmt()?);
            }
        }
        
        self.consume(TokenType::End)?;
        
        Ok(Stmt::fun_stmt(name, params, body, return_expr))
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt> {
        self.consume(TokenType::Return)?;
        let expr = self.parse_expression()?;
        Ok(Stmt::return_stmt(expr))
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
            TokenType::Identifier(name) => {
                let identifier = name.clone();
                self.advance();
                
                // Check if this is a function call
                if matches!(self.current_token().token_type, TokenType::LeftParen) {
                    self.advance(); // consume '('
                    let mut args = Vec::new();
                    
                    if !matches!(self.current_token().token_type, TokenType::RightParen) {
                        loop {
                            args.push(self.parse_expression()?);
                            
                            if matches!(self.current_token().token_type, TokenType::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    
                    self.consume(TokenType::RightParen)?;
                    Ok(Expr::call(Expr::identifier(identifier), args))
                } else {
                    Ok(Expr::identifier(identifier))
                }
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
    fn test_parse_binary_operations() {
        let test_cases = vec![
            ("2 + 3", Expr::binary(Expr::Number(2.0), BinaryOp::Add, Expr::Number(3.0))),
            ("2 * 3", Expr::binary(Expr::Number(2.0), BinaryOp::Multiply, Expr::Number(3.0))),
        ];

        for (input, expected) in test_cases {
            let expr = parse_expression(input).unwrap();
            assert_eq!(expr, expected, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_parse_complex_expressions() {
        let test_cases = vec![
            (
                "2 + 3 * 4",
                Expr::binary(
                    Expr::Number(2.0),
                    BinaryOp::Add,
                    Expr::binary(Expr::Number(3.0), BinaryOp::Multiply, Expr::Number(4.0))
                ),
            ),
            (
                "(2 + 3) * 4",
                Expr::binary(
                    Expr::binary(Expr::Number(2.0), BinaryOp::Add, Expr::Number(3.0)),
                    BinaryOp::Multiply,
                    Expr::Number(4.0)
                ),
            ),
            (
                "2 * 3 + 4 / 2",
                Expr::binary(
                    Expr::binary(Expr::Number(2.0), BinaryOp::Multiply, Expr::Number(3.0)),
                    BinaryOp::Add,
                    Expr::binary(Expr::Number(4.0), BinaryOp::Divide, Expr::Number(2.0))
                ),
            ),
        ];

        for (input, expected) in test_cases {
            let expr = parse_expression(input).unwrap();
            assert_eq!(expr, expected, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_parse_function_call() {
        let test_cases = vec![
            (
                "add(2, 3)",
                Expr::call(
                    Expr::identifier("add".to_string()),
                    vec![Expr::Number(2.0), Expr::Number(3.0)]
                )
            ),
            (
                "max(1)",
                Expr::call(
                    Expr::identifier("max".to_string()),
                    vec![Expr::Number(1.0)]
                )
            ),
            (
                "min()",
                Expr::call(
                    Expr::identifier("min".to_string()),
                    vec![]
                )
            ),
        ];

        for (input, expected) in test_cases {
            let expr = parse_expression(input).unwrap();
            assert_eq!(expr, expected, "Failed for input: {}", input);
        }
    }

    fn parse_statement(input: &str) -> Result<Stmt> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize()?;
        let mut parser = Parser::new(tokens);
        parser.parse_stmt()
    }

    #[test]
    fn test_parse_function_definition() {
        let input = "fun add(x, y) do return x + y end";
        let stmt = parse_statement(input).unwrap();
        
        let expected = Stmt::fun_stmt(
            "add".to_string(),
            vec![
                FunParam::new("x".to_string(), None),
                FunParam::new("y".to_string(), None),
            ],
            vec![],
            Some(Expr::binary(
                Expr::identifier("x".to_string()),
                BinaryOp::Add,
                Expr::identifier("y".to_string())
            ))
        );
        
        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_parse_function_with_typed_params() {
        let input = "fun multiply(x: int, y: int) do return x * y end";
        let stmt = parse_statement(input).unwrap();
        
        let expected = Stmt::fun_stmt(
            "multiply".to_string(),
            vec![
                FunParam::new("x".to_string(), Some("int".to_string())),
                FunParam::new("y".to_string(), Some("int".to_string())),
            ],
            vec![],
            Some(Expr::binary(
                Expr::identifier("x".to_string()),
                BinaryOp::Multiply,
                Expr::identifier("y".to_string())
            ))
        );
        
        assert_eq!(stmt, expected);
    }
}