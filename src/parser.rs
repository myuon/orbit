use crate::ast::{
    BinaryOp, Decl, Expr, FunParam, Function, GlobalVariable, Program, Stmt, StructDecl,
    StructField, Token, TokenType,
};
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
            TokenType::Let => {
                let global_var = self.parse_global_variable()?;
                Ok(Decl::GlobalVariable(global_var))
            }
            _ => {
                bail!(
                    "Expected declaration (function, type, or let), found {:?}",
                    self.current_token().token_type
                )
            }
        }
    }

    /// Parse a global variable declaration
    fn parse_global_variable(&mut self) -> Result<GlobalVariable> {
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

        Ok(GlobalVariable { name, value })
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
        let mut type_params = Vec::new();

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

                if matches!(self.current_token().token_type, TokenType::Colon) {
                    self.advance();

                    // Check if this is a type parameter (": type")
                    if matches!(self.current_token().token_type, TokenType::Type) {
                        self.advance(); // consume 'type'
                        type_params.push(param_name);
                    } else {
                        let type_name = self.parse_type_name()?;
                        params.push(FunParam {
                            name: param_name,
                            type_name: Some(type_name),
                        });
                    }
                } else {
                    params.push(FunParam {
                        name: param_name,
                        type_name: None,
                    });
                }

                if matches!(self.current_token().token_type, TokenType::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen)?;

        // Parse optional return type annotation: : TypeName
        if matches!(self.current_token().token_type, TokenType::Colon) {
            self.advance(); // consume ':'
            let _return_type = self.parse_type_name()?; // Parse but ignore for now
        }

        self.consume(TokenType::Do)?;

        let mut body = Vec::new();

        while !matches!(self.current_token().token_type, TokenType::End) {
            if matches!(self.current_token().token_type, TokenType::Eof) {
                bail!("Unexpected end of file in function body");
            }

            body.push(self.parse_stmt()?);
        }

        self.consume(TokenType::End)?;

        Ok(Function {
            name,
            type_params,
            params,
            body,
        })
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

        // Parse optional type parameters: (A: type, B: type)
        let type_params = if matches!(self.current_token().token_type, TokenType::LeftParen) {
            self.advance(); // consume '('
            let mut params = Vec::new();

            while !matches!(self.current_token().token_type, TokenType::RightParen) {
                if matches!(self.current_token().token_type, TokenType::Eof) {
                    bail!("Unexpected end of file in type parameter list");
                }

                // Parse type parameter name
                let param_name = match &self.current_token().token_type {
                    TokenType::Identifier(name) => {
                        let n = name.clone();
                        self.advance();
                        n
                    }
                    _ => bail!("Expected type parameter name"),
                };

                // Expect ': type'
                self.consume(TokenType::Colon)?;
                self.consume(TokenType::Type)?;

                params.push(param_name);

                // Handle comma or end of parameter list
                if matches!(self.current_token().token_type, TokenType::Comma) {
                    self.advance(); // consume ','
                } else if !matches!(self.current_token().token_type, TokenType::RightParen) {
                    bail!("Expected ',' or ')' in type parameter list");
                }
            }

            self.consume(TokenType::RightParen)?;
            params
        } else {
            Vec::new()
        };

        self.consume(TokenType::Assign)?;
        self.consume(TokenType::Struct)?;
        self.consume(TokenType::LeftBrace)?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        while !matches!(self.current_token().token_type, TokenType::RightBrace) {
            if matches!(self.current_token().token_type, TokenType::Eof) {
                bail!("Unexpected end of file in struct declaration");
            }

            // Check if this is a method definition
            if matches!(self.current_token().token_type, TokenType::Fun) {
                let method = self.parse_function()?;
                methods.push(method);
            } else {
                // Parse field
                let field_name = match &self.current_token().token_type {
                    TokenType::Identifier(name) => {
                        let n = name.clone();
                        self.advance();
                        n
                    }
                    _ => bail!("Expected field name in struct declaration"),
                };

                self.consume(TokenType::Colon)?;

                let type_name = self.parse_type_name()?;

                fields.push(StructField {
                    name: field_name,
                    type_name,
                });

                if matches!(self.current_token().token_type, TokenType::Comma) {
                    self.advance();
                } else if !matches!(self.current_token().token_type, TokenType::RightBrace)
                    && !matches!(self.current_token().token_type, TokenType::Fun)
                {
                    bail!("Expected ',' or '}}' in struct declaration");
                }
            }
        }

        self.consume(TokenType::RightBrace)?;
        self.consume(TokenType::Semicolon)?;

        Ok(StructDecl {
            name,
            type_params,
            fields,
            methods,
        })
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
                                self.consume(TokenType::Semicolon)?;
                                Ok(Stmt::Expression(expr))
                            }
                        }
                        _ => {
                            let expr = self.parse_expression()?;
                            self.consume(TokenType::Semicolon)?;
                            Ok(Stmt::Expression(expr))
                        }
                    }
                } else {
                    let expr = self.parse_expression()?;
                    self.consume(TokenType::Semicolon)?;
                    Ok(Stmt::Expression(expr))
                }
            }
            _ => {
                let expr = self.parse_expression()?;
                self.consume(TokenType::Semicolon)?;
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

    /// Parse a type name, including pointer types [*]T
    fn parse_type_name(&mut self) -> Result<String> {
        if matches!(self.current_token().token_type, TokenType::LeftBracket) {
            self.advance(); // consume '['
            self.consume(TokenType::Star)?; // consume '*'
            self.consume(TokenType::RightBracket)?; // consume ']'

            let inner_type = self.parse_type_name()?;
            Ok(format!("[*]{}", inner_type))
        } else {
            match &self.current_token().token_type {
                TokenType::Identifier(name) => {
                    let n = name.clone();
                    self.advance();

                    // Check for generic instantiation: Type(arg1, arg2, ...)
                    if matches!(self.current_token().token_type, TokenType::LeftParen) {
                        self.advance(); // consume '('
                        let mut args = Vec::new();

                        while !matches!(self.current_token().token_type, TokenType::RightParen) {
                            if matches!(self.current_token().token_type, TokenType::Eof) {
                                bail!("Unexpected end of file in generic type arguments");
                            }

                            let arg_type = self.parse_type_name()?;
                            args.push(arg_type);

                            if matches!(self.current_token().token_type, TokenType::Comma) {
                                self.advance(); // consume ','
                            } else if !matches!(
                                self.current_token().token_type,
                                TokenType::RightParen
                            ) {
                                bail!("Expected ',' or ')' in generic type arguments");
                            }
                        }

                        self.consume(TokenType::RightParen)?;

                        // Format as generic type: Type(arg1, arg2)
                        let args_str = args.join(", ");
                        Ok(format!("{}({})", n, args_str))
                    } else {
                        Ok(n)
                    }
                }
                // Handle built-in generic types
                TokenType::Vec => {
                    self.advance(); // consume 'vec'
                    if matches!(self.current_token().token_type, TokenType::LeftParen) {
                        self.advance(); // consume '('
                        let arg_type = self.parse_type_name()?;
                        self.consume(TokenType::RightParen)?;
                        Ok(format!("vec({})", arg_type))
                    } else {
                        Ok("vec".to_string())
                    }
                }
                TokenType::Map => {
                    self.advance(); // consume 'map'
                    if matches!(self.current_token().token_type, TokenType::LeftParen) {
                        self.advance(); // consume '('
                        let key_type = self.parse_type_name()?;
                        self.consume(TokenType::Comma)?;
                        let value_type = self.parse_type_name()?;
                        self.consume(TokenType::RightParen)?;
                        Ok(format!("map({}, {})", key_type, value_type))
                    } else {
                        Ok("map".to_string())
                    }
                }
                _ => bail!("Expected type name"),
            }
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
            TokenType::Int(value) => {
                let num = *value;
                self.advance();
                Ok(Expr::Int(num))
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
            TokenType::Byte(value) => {
                let byte_val = *value;
                self.advance();
                Ok(Expr::Byte(byte_val))
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
                    // Check for field access or method call
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

                        // Check if this is a method call (followed by parentheses)
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
                            expr = Expr::MethodCall {
                                object: Box::new(expr),
                                method: field_name,
                                args,
                                object_type: None, // Will be filled by type checker
                            };
                        } else {
                            // Regular field access
                            expr = Expr::FieldAccess {
                                object: Box::new(expr),
                                field: field_name,
                            };
                        }
                    }

                    Ok(expr)
                }
            }
            TokenType::New => {
                self.advance(); // consume 'new'

                // Check for new(struct) pattern
                if matches!(self.current_token().token_type, TokenType::LeftParen) {
                    self.advance(); // consume '('
                    if matches!(self.current_token().token_type, TokenType::Struct) {
                        self.advance(); // consume 'struct'
                        self.consume(TokenType::RightParen)?; // consume ')'

                        let type_name = self.parse_type_name()?;
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
                        return Ok(Expr::StructNewPattern { type_name, fields });
                    } else {
                        bail!("Expected 'struct' after 'new('");
                    }
                } else if matches!(self.current_token().token_type, TokenType::Vec) {
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
                    // Support generic types: new Container(int) { .field = value, ... }
                    let type_name = self.parse_type_name()?;

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
            TokenType::Type => {
                self.advance(); // consume 'type'
                let type_name = self.parse_type_name()?;
                Ok(Expr::TypeExpr { type_name })
            }
            TokenType::Alloc => {
                self.advance(); // consume 'alloc'
                self.consume(TokenType::LeftParen)?; // consume '('

                // Parse element type
                let element_type = self.parse_type_name()?;

                self.consume(TokenType::Comma)?; // consume ','

                // Parse size expression
                let size = self.parse_expression()?;

                self.consume(TokenType::RightParen)?; // consume ')'

                Ok(Expr::Alloc {
                    element_type,
                    size: Box::new(size),
                })
            }
            _ => bail!("Unexpected token: {:?}", self.current_token().token_type),
        }
    }

    /// Check if the current position is the start of a field assignment (obj.field = value)
    fn is_field_assignment(&self) -> bool {
        // Pattern: identifier . identifier = ...
        // Current position should be at identifier, next is dot
        if self.position + 3 < self.tokens.len() {
            matches!(self.tokens[self.position + 1].token_type, TokenType::Dot)
                && matches!(
                    self.tokens[self.position + 2].token_type,
                    TokenType::Identifier(_)
                )
                && matches!(self.tokens[self.position + 3].token_type, TokenType::Assign)
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

        Ok(Stmt::FieldAssign {
            object,
            field,
            value,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_generic_struct_parsing() {
        let input = "type Container(T: type) = struct {
            value: T,
        };";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse_program().unwrap();

        assert_eq!(result.declarations.len(), 1);
        match &result.declarations[0] {
            Decl::Struct(struct_decl) => {
                assert_eq!(struct_decl.name, "Container");
                assert_eq!(struct_decl.type_params.len(), 1);
                assert_eq!(struct_decl.type_params[0], "T");
                assert_eq!(struct_decl.fields.len(), 1);
                assert_eq!(struct_decl.fields[0].name, "value");
                assert_eq!(struct_decl.fields[0].type_name, "T");
            }
            _ => panic!("Expected struct declaration"),
        }
    }

    #[test]
    fn test_generic_function_parsing() {
        let input = "fun identity(T: type, value: T): T do
            return value;
        end";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse_program().unwrap();

        assert_eq!(result.declarations.len(), 1);
        match &result.declarations[0] {
            Decl::Function(function) => {
                assert_eq!(function.name, "identity");
                assert_eq!(function.type_params.len(), 1);
                assert_eq!(function.type_params[0], "T");
                assert_eq!(function.params.len(), 1);
                assert_eq!(function.params[0].name, "value");
                assert_eq!(function.params[0].type_name, Some("T".to_string()));
            }
            _ => panic!("Expected function declaration"),
        }
    }

    #[test]
    fn test_generic_type_instantiation_parsing() {
        let input = "type Point = struct {
            container: Container(int),
        };";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse_program().unwrap();

        assert_eq!(result.declarations.len(), 1);
        match &result.declarations[0] {
            Decl::Struct(struct_decl) => {
                assert_eq!(struct_decl.name, "Point");
                assert_eq!(struct_decl.fields.len(), 1);
                assert_eq!(struct_decl.fields[0].name, "container");
                assert_eq!(struct_decl.fields[0].type_name, "Container(int)");
            }
            _ => panic!("Expected struct declaration"),
        }
    }

    #[test]
    fn test_struct_with_methods_parsing() {
        let code = r#"
type Point = struct {
  x: int,
  y: int,

  fun sum(self: Point) do
    return self.x + self.y;
  end
};
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().unwrap();

        assert_eq!(program.declarations.len(), 1);

        if let Decl::Struct(struct_decl) = &program.declarations[0] {
            assert_eq!(struct_decl.name, "Point");
            assert_eq!(struct_decl.fields.len(), 2);
            assert_eq!(struct_decl.methods.len(), 1);

            let method = &struct_decl.methods[0];
            assert_eq!(method.name, "sum");
            assert_eq!(method.params.len(), 1);
            assert_eq!(method.params[0].name, "self");
        } else {
            panic!("Expected struct declaration");
        }
    }

    #[test]
    fn test_method_call_parsing() {
        let code = "p.sum()";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse().unwrap();

        if let Expr::MethodCall {
            object,
            method,
            args,
            object_type,
        } = expr
        {
            if let Expr::Identifier(obj_name) = object.as_ref() {
                assert_eq!(obj_name, "p");
            } else {
                panic!("Expected identifier for object");
            }
            assert_eq!(method, "sum");
            assert_eq!(args.len(), 0);
            assert_eq!(object_type, None); // Should be None before type checking
        } else {
            panic!("Expected method call, got: {:?}", expr);
        }
    }

    #[test]
    fn test_pointer_type_parsing() {
        let code = r#"
type Point = struct {
  data: [*]int
};
"#;
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().unwrap();

        assert_eq!(program.declarations.len(), 1);

        if let Decl::Struct(struct_decl) = &program.declarations[0] {
            assert_eq!(struct_decl.name, "Point");
            assert_eq!(struct_decl.fields.len(), 1);
            assert_eq!(struct_decl.fields[0].name, "data");
            assert_eq!(struct_decl.fields[0].type_name, "[*]int");
        } else {
            panic!("Expected struct declaration");
        }
    }

    #[test]
    fn test_struct_new_pattern_parsing() {
        let code = "new(struct) Point { .x = 10, .y = 20 }";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse().unwrap();

        if let Expr::StructNewPattern { type_name, fields } = expr {
            assert_eq!(type_name, "Point");
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].0, "x");
            assert_eq!(fields[1].0, "y");
            if let Expr::Int(n) = fields[0].1 {
                assert_eq!(n, 10);
            } else {
                panic!("Expected number expression for field value");
            }
            if let Expr::Int(n) = fields[1].1 {
                assert_eq!(n, 20);
            } else {
                panic!("Expected number expression for field value");
            }
        } else {
            panic!("Expected StructNewPattern expression, got: {:?}", expr);
        }
    }

    #[test]
    fn test_struct_new_pattern_vs_regular_new() {
        // Test that both syntaxes work and produce different AST nodes
        let code1 = "new Point { .x = 10, .y = 20 }";
        let code2 = "new(struct) Point { .x = 10, .y = 20 }";

        let mut lexer1 = Lexer::new(code1);
        let tokens1 = lexer1.tokenize().unwrap();
        let mut parser1 = Parser::new(tokens1);
        let expr1 = parser1.parse().unwrap();

        let mut lexer2 = Lexer::new(code2);
        let tokens2 = lexer2.tokenize().unwrap();
        let mut parser2 = Parser::new(tokens2);
        let expr2 = parser2.parse().unwrap();

        // First should be StructNew
        if let Expr::StructNew { type_name, fields } = expr1 {
            assert_eq!(type_name, "Point");
            assert_eq!(fields.len(), 2);
        } else {
            panic!("Expected StructNew expression");
        }

        // Second should be StructNewPattern
        if let Expr::StructNewPattern { type_name, fields } = expr2 {
            assert_eq!(type_name, "Point");
            assert_eq!(fields.len(), 2);
        } else {
            panic!("Expected StructNewPattern expression");
        }
    }
}
