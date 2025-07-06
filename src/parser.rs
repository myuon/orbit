use crate::ast::{BinaryOp, Decl, Expr, FunParam, Function, Program, Stmt, StructDecl, StructField, Token, TokenType};
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
            eof_token: Token {
                token_type: TokenType::Eof,
                position: 0,
            },
        }
    }

    pub fn current_token(&self) -> &Token {
        self.tokens.get(self.position).unwrap_or(&self.eof_token)
    }

    fn advance(&mut self) {
        if self.position < self.tokens.len() {
            self.position += 1;
        }
    }

    fn consume(&mut self, expected: TokenType) -> Result<()> {
        if std::mem::discriminant(&self.current_token().token_type)
            == std::mem::discriminant(&expected)
        {
            self.advance();
            Ok(())
        } else {
            bail!(
                "Expected {:?}, found {:?}",
                expected,
                self.current_token().token_type
            )
        }
    }

    pub fn parse(&mut self) -> Result<Expr> {
        self.parse_expression()
    }

    /// Parse a complete Program consisting of declarations
    pub fn parse_program(&mut self) -> Result<Program> {
        let mut declarations = Vec::new();

        while !matches!(self.current_token().token_type, TokenType::Eof) {
            declarations.push(self.parse_decl()?);
        }

        Ok(Program { declarations })
    }

    /// Parse a top-level declaration
    fn parse_decl(&mut self) -> Result<Decl> {
        match &self.current_token().token_type {
            TokenType::Fun => {
                let function = self.parse_function()?;
                Ok(Decl::Function(function))
            }
            TokenType::Type => {
                let struct_decl = self.parse_struct_decl()?;
                Ok(Decl::Struct(struct_decl))
            }
            _ => {
                bail!(
                    "Expected declaration (function or type), found {:?}",
                    self.current_token().token_type
                )
            }
        }
    }

    /// Parse a function declaration
    fn parse_function(&mut self) -> Result<Function> {
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

                params.push(FunParam {
                    name: param_name,
                    type_name,
                });

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

        while !matches!(self.current_token().token_type, TokenType::End) {
            if matches!(self.current_token().token_type, TokenType::Eof) {
                bail!("Unexpected end of file in function body");
            }

            body.push(self.parse_stmt()?);
        }

        self.consume(TokenType::End)?;

        Ok(Function { name, params, body })
    }

    /// Parse a struct declaration
    fn parse_struct_decl(&mut self) -> Result<StructDecl> {
        self.consume(TokenType::Type)?;

        let name = match &self.current_token().token_type {
            TokenType::Identifier(name) => {
                let n = name.clone();
                self.advance();
                n
            }
            _ => bail!("Expected struct name after 'type'"),
        };

        self.consume(TokenType::Assign)?;
        self.consume(TokenType::Struct)?;
        self.consume(TokenType::LeftBrace)?;

        let mut fields = Vec::new();

        while !matches!(self.current_token().token_type, TokenType::RightBrace) {
            if matches!(self.current_token().token_type, TokenType::Eof) {
                bail!("Unexpected end of file in struct declaration");
            }

            let field_name = match &self.current_token().token_type {
                TokenType::Identifier(name) => {
                    let n = name.clone();
                    self.advance();
                    n
                }
                _ => bail!("Expected field name in struct declaration"),
            };

            self.consume(TokenType::Colon)?;

            let type_name = match &self.current_token().token_type {
                TokenType::Identifier(name) => {
                    let n = name.clone();
                    self.advance();
                    n
                }
                _ => bail!("Expected type name in struct field"),
            };

            fields.push(StructField { name: field_name, type_name });

            if matches!(self.current_token().token_type, TokenType::Comma) {
                self.advance();
            } else if !matches!(self.current_token().token_type, TokenType::RightBrace) {
                bail!("Expected ',' or '}}' in struct declaration");
            }
        }

        self.consume(TokenType::RightBrace)?;
        self.consume(TokenType::Semicolon)?;

        Ok(StructDecl { name, fields })
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt> {
        match &self.current_token().token_type {
            TokenType::Let => self.parse_let_stmt(),
            TokenType::Return => self.parse_return_stmt(),
            TokenType::If => self.parse_if_stmt(),
            TokenType::While => self.parse_while_stmt(),
            TokenType::Identifier(_) => {
                // Look ahead to see if this is an assignment, vector push, field assign, or vector assign
                if self.position + 1 < self.tokens.len() {
                    match &self.tokens[self.position + 1].token_type {
                        TokenType::Assign => self.parse_assign_stmt(),
                        TokenType::Push => self.parse_vector_push_stmt(),
                        TokenType::LeftBracket => {
                            // Could be container[index] = value
                            self.parse_index_assign_or_expr()
                        }
                        TokenType::Dot => {
                            // Check if this is field assignment: obj.field = value
                            if self.is_field_assignment() {
                                self.parse_field_assign_stmt()
                            } else {
                                let expr = self.parse_expression()?;
                                Ok(Stmt::Expression(expr))
                            }
                        }
                        _ => {
                            let expr = self.parse_expression()?;
                            Ok(Stmt::Expression(expr))
                        }
                    }
                } else {
                    let expr = self.parse_expression()?;
                    Ok(Stmt::Expression(expr))
                }
            }
            _ => {
                let expr = self.parse_expression()?;
                Ok(Stmt::Expression(expr))
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
            _ => bail!("Expected variable name after 'let'"),
        };

        self.consume(TokenType::Assign)?;
        let value = self.parse_expression()?;
        self.consume(TokenType::Semicolon)?;

        Ok(Stmt::Let { name, value })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt> {
        self.consume(TokenType::Return)?;
        let expr = self.parse_expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Return(expr))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt> {
        self.consume(TokenType::If)?;
        let condition = self.parse_comparison()?;
        self.consume(TokenType::Then)?;

        let mut then_branch = Vec::new();

        // Parse statements until we hit 'else' or 'end'
        while !matches!(
            self.current_token().token_type,
            TokenType::Else | TokenType::End | TokenType::Eof
        ) {
            then_branch.push(self.parse_stmt()?);
        }

        let else_branch = if matches!(self.current_token().token_type, TokenType::Else) {
            self.advance(); // consume 'else'

            // Check for 'else if'
            if matches!(self.current_token().token_type, TokenType::If) {
                // Parse as a single if statement in the else branch
                Some(vec![self.parse_if_stmt()?])
            } else {
                let mut else_stmts = Vec::new();
                while !matches!(
                    self.current_token().token_type,
                    TokenType::End | TokenType::Eof
                ) {
                    else_stmts.push(self.parse_stmt()?);
                }
                Some(else_stmts)
            }
        } else {
            None
        };

        self.consume(TokenType::End)?;

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt> {
        self.consume(TokenType::While)?;
        let condition = self.parse_comparison()?;
        self.consume(TokenType::Do)?;

        let mut body = Vec::new();
        while !matches!(
            self.current_token().token_type,
            TokenType::End | TokenType::Eof
        ) {
            body.push(self.parse_stmt()?);
        }

        self.consume(TokenType::End)?;

        Ok(Stmt::While { condition, body })
    }

    /// Helper function to parse an identifier and advance
    fn parse_identifier(&mut self, context: &str) -> Result<String> {
        match &self.current_token().token_type {
            TokenType::Identifier(name) => {
                let n = name.clone();
                self.advance();
                Ok(n)
            }
            _ => bail!("Expected identifier {}", context),
        }
    }

    fn parse_assign_stmt(&mut self) -> Result<Stmt> {
        let name = self.parse_identifier("in assignment")?;
        self.consume(TokenType::Assign)?;
        let value = self.parse_expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Assign { name, value })
    }

    fn parse_vector_push_stmt(&mut self) -> Result<Stmt> {
        let vector = self.parse_identifier("in push statement")?;
        self.consume(TokenType::Push)?; // consume '<-'
        let value = self.parse_expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::VectorPush { vector, value })
    }

    fn parse_index_assign_or_expr(&mut self) -> Result<Stmt> {
        // We need to check if this is container[index] = value or just a complex expression
        // Let's parse it as an expression first and check if it's followed by assignment

        // Save the current position in case we need to backtrack
        let saved_position = self.position;

        // Try to parse container[index] assignment
        if let Ok(stmt) = self.try_parse_simple_index_assignment() {
            return Ok(stmt);
        }

        // Reset position and parse as a regular expression
        self.position = saved_position;
        let expr = self.parse_expression()?;
        Ok(Stmt::Expression(expr))
    }

    fn try_parse_simple_index_assignment(&mut self) -> Result<Stmt> {
        let name = match &self.current_token().token_type {
            TokenType::Identifier(name) => {
                let n = name.clone();
                self.advance();
                n
            }
            _ => bail!("Expected variable name"),
        };

        self.consume(TokenType::LeftBracket)?; // consume '['
        let index = self.parse_expression()?;
        self.consume(TokenType::RightBracket)?; // consume ']'

        // Check if this is an assignment
        if matches!(self.current_token().token_type, TokenType::Assign) {
            self.advance(); // consume '='
            let value = self.parse_expression()?;
            self.consume(TokenType::Semicolon)?;

            // We can't differentiate between vector and map assignment at parse time
            // Type checking will determine the container type later
            Ok(Stmt::IndexAssign {
                container: name,
                index,
                value,
                container_type: None, // Will be filled in by type checker
            })
        } else {
            bail!("Not an index assignment")
        }
    }

    fn parse_expression(&mut self) -> Result<Expr> {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> Result<Expr> {
        self.parse_binary_left_assoc(
            |parser| parser.parse_term(),
            &[
                (TokenType::Equal, BinaryOp::Equal),
                (TokenType::NotEqual, BinaryOp::NotEqual),
                (TokenType::Less, BinaryOp::Less),
                (TokenType::Greater, BinaryOp::Greater),
                (TokenType::LessEqual, BinaryOp::LessEqual),
                (TokenType::GreaterEqual, BinaryOp::GreaterEqual),
            ],
        )
    }

    /// Helper function for parsing left-associative binary operations
    fn parse_binary_left_assoc<F>(
        &mut self,
        mut operand_parser: F,
        operators: &[(TokenType, BinaryOp)],
    ) -> Result<Expr>
    where
        F: FnMut(&mut Self) -> Result<Expr>,
    {
        let mut left = operand_parser(self)?;

        loop {
            let mut found = false;
            for (token_type, binary_op) in operators {
                if std::mem::discriminant(&self.current_token().token_type)
                    == std::mem::discriminant(token_type)
                {
                    self.advance();
                    let right = operand_parser(self)?;
                    left = Expr::Binary {
                        left: Box::new(left),
                        op: *binary_op,
                        right: Box::new(right),
                    };
                    found = true;
                    break;
                }
            }
            if !found {
                break;
            }
        }

        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Expr> {
        self.parse_binary_left_assoc(
            |parser| parser.parse_factor(),
            &[
                (TokenType::Plus, BinaryOp::Add),
                (TokenType::Minus, BinaryOp::Subtract),
            ],
        )
    }

    fn parse_factor(&mut self) -> Result<Expr> {
        self.parse_binary_left_assoc(
            |parser| parser.parse_primary(),
            &[
                (TokenType::Star, BinaryOp::Multiply),
                (TokenType::Slash, BinaryOp::Divide),
            ],
        )
    }

    fn parse_primary(&mut self) -> Result<Expr> {
        match &self.current_token().token_type {
            TokenType::Number(value) => {
                let num = *value;
                self.advance();
                Ok(Expr::Number(num))
            }
            TokenType::Boolean(value) => {
                let bool_val = *value;
                self.advance();
                Ok(Expr::Boolean(bool_val))
            }
            TokenType::String(value) => {
                let string_val = value.clone();
                self.advance();
                Ok(Expr::String(string_val))
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
                    let mut expr = Expr::Call {
                        callee: Box::new(Expr::Identifier(identifier)),
                        args,
                    };

                    // Check for container indexing
                    if matches!(self.current_token().token_type, TokenType::LeftBracket) {
                        self.advance(); // consume '['
                        let index = self.parse_expression()?;
                        self.consume(TokenType::RightBracket)?;
                        expr = Expr::Index {
                            container: Box::new(expr),
                            index: Box::new(index),
                            container_type: None, // Will be filled in by type checker
                        };
                    }

                    Ok(expr)
                } else if matches!(self.current_token().token_type, TokenType::LeftBracket) {
                    // Container indexing - we'll determine the type at type checking
                    self.advance(); // consume '['
                    let index = self.parse_expression()?;
                    self.consume(TokenType::RightBracket)?;

                    Ok(Expr::Index {
                        container: Box::new(Expr::Identifier(identifier)),
                        index: Box::new(index),
                        container_type: None, // Will be filled in by type checker
                    })
                } else {
                    // Check for field access
                    let mut expr = Expr::Identifier(identifier);
                    
                    while matches!(self.current_token().token_type, TokenType::Dot) {
                        self.advance(); // consume '.'
                        let field_name = match &self.current_token().token_type {
                            TokenType::Identifier(name) => {
                                let n = name.clone();
                                self.advance();
                                n
                            }
                            _ => bail!("Expected field name after '.'"),
                        };
                        
                        expr = Expr::FieldAccess {
                            object: Box::new(expr),
                            field: field_name,
                        };
                    }
                    
                    Ok(expr)
                }
            }
            TokenType::New => {
                self.advance(); // consume 'new'

                if matches!(self.current_token().token_type, TokenType::Vec) {
                    self.advance(); // consume 'vec'
                    self.consume(TokenType::LeftParen)?; // consume '('

                    // Get the element type
                    let element_type = match &self.current_token().token_type {
                        TokenType::Identifier(type_name) => {
                            let t = type_name.clone();
                            self.advance();
                            t
                        }
                        _ => bail!("Expected type name in vector constructor"),
                    };

                    self.consume(TokenType::RightParen)?; // consume ')'
                    self.consume(TokenType::LeftBrace)?; // consume '{'

                    // Parse initial values (if any)
                    let mut initial_values = Vec::new();
                    if !matches!(self.current_token().token_type, TokenType::RightBrace) {
                        loop {
                            initial_values.push(self.parse_expression()?);

                            if matches!(self.current_token().token_type, TokenType::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }

                    self.consume(TokenType::RightBrace)?; // consume '}'
                    Ok(Expr::VectorNew {
                        element_type,
                        initial_values,
                    })
                } else if matches!(self.current_token().token_type, TokenType::Map) {
                    self.advance(); // consume 'map'
                    self.consume(TokenType::LeftParen)?; // consume '('

                    // Parse key type: expect [*]type or type
                    let key_type =
                        if matches!(self.current_token().token_type, TokenType::LeftBracket) {
                            self.advance(); // consume '['
                            self.consume(TokenType::Star)?; // consume '*'
                            self.consume(TokenType::RightBracket)?; // consume ']'

                            match &self.current_token().token_type {
                                TokenType::Identifier(type_name) => {
                                    let t = format!("[*]{}", type_name);
                                    self.advance();
                                    t
                                }
                                _ => bail!("Expected type name after [*]"),
                            }
                        } else {
                            match &self.current_token().token_type {
                                TokenType::Identifier(type_name) => {
                                    let t = type_name.clone();
                                    self.advance();
                                    t
                                }
                                _ => bail!("Expected key type in map constructor"),
                            }
                        };

                    self.consume(TokenType::Comma)?; // consume ','

                    // Parse value type
                    let value_type = match &self.current_token().token_type {
                        TokenType::Identifier(type_name) => {
                            let t = type_name.clone();
                            self.advance();
                            t
                        }
                        _ => bail!("Expected value type in map constructor"),
                    };

                    self.consume(TokenType::RightParen)?; // consume ')'
                    self.consume(TokenType::LeftBrace)?; // consume '{'

                    // Parse initial key-value pairs (if any)
                    let mut initial_pairs = Vec::new();
                    if !matches!(self.current_token().token_type, TokenType::RightBrace) {
                        loop {
                            let key = self.parse_expression()?;
                            self.consume(TokenType::Colon)?; // consume ':'
                            let value = self.parse_expression()?;
                            initial_pairs.push((key, value));

                            if matches!(self.current_token().token_type, TokenType::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }

                    self.consume(TokenType::RightBrace)?; // consume '}'
                    Ok(Expr::MapNew {
                        key_type,
                        value_type,
                        initial_pairs,
                    })
                } else {
                    // Handle struct instantiation: new TypeName { .field = value, ... }
                    let type_name = match &self.current_token().token_type {
                        TokenType::Identifier(name) => {
                            let n = name.clone();
                            self.advance();
                            n
                        }
                        _ => bail!("Expected type name after 'new'"),
                    };

                    self.consume(TokenType::LeftBrace)?;

                    let mut fields = Vec::new();
                    if !matches!(self.current_token().token_type, TokenType::RightBrace) {
                        loop {
                            self.consume(TokenType::Dot)?; // consume '.'
                            let field_name = match &self.current_token().token_type {
                                TokenType::Identifier(name) => {
                                    let n = name.clone();
                                    self.advance();
                                    n
                                }
                                _ => bail!("Expected field name after '.'"),
                            };

                            self.consume(TokenType::Assign)?; // consume '='
                            let value = self.parse_expression()?;
                            fields.push((field_name, value));

                            if matches!(self.current_token().token_type, TokenType::Comma) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }

                    self.consume(TokenType::RightBrace)?;
                    Ok(Expr::StructNew { type_name, fields })
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

    /// Check if the current position is the start of a field assignment (obj.field = value)
    fn is_field_assignment(&self) -> bool {
        // Pattern: identifier . identifier = ...
        // Current position should be at identifier, next is dot
        if self.position + 3 < self.tokens.len() {
            matches!(self.tokens[self.position + 1].token_type, TokenType::Dot) &&
            matches!(self.tokens[self.position + 2].token_type, TokenType::Identifier(_)) &&
            matches!(self.tokens[self.position + 3].token_type, TokenType::Assign)
        } else {
            false
        }
    }

    fn parse_field_assign_stmt(&mut self) -> Result<Stmt> {
        // Parse: obj.field = value;
        // Parse just the object identifier, not the full expression
        let object = Expr::Identifier(self.parse_identifier("in field assignment")?);
        self.consume(TokenType::Dot)?;
        
        let field = match &self.current_token().token_type {
            TokenType::Identifier(name) => {
                let n = name.clone();
                self.advance();
                n
            }
            _ => bail!("Expected field name after '.'"),
        };
        
        self.consume(TokenType::Assign)?;
        let value = self.parse_expression()?;
        self.consume(TokenType::Semicolon)?;
        
        Ok(Stmt::FieldAssign { object, field, value })
    }
}
