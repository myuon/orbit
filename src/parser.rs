use crate::ast::{
    BinaryOp, Decl, Expr, FunParam, Function, GlobalVariable, Positioned, PositionedDecl,
    PositionedExpr, PositionedFunction, PositionedGlobalVariable, PositionedStmt,
    PositionedStructDecl, Program, Span, Stmt, StructDecl, StructField, StructNewKind, Token,
    TokenType, Type,
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
                "Expected {:?}, found {:?} at position {}",
                expected,
                self.current_token().token_type,
                self.current_token().position
            )
        }
    }

    pub fn parse(&mut self) -> Result<PositionedExpr> {
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
    fn parse_decl(&mut self) -> Result<PositionedDecl> {
        let start_pos = self.current_token().position;
        let result = match &self.current_token().token_type {
            TokenType::Fun => {
                let function = self.parse_function()?;
                Decl::Function(function)
            }
            TokenType::Type => {
                let struct_decl = self.parse_struct_decl()?;
                Decl::Struct(struct_decl)
            }
            TokenType::Let => {
                let global_var = self.parse_global_variable()?;
                Decl::GlobalVariable(global_var)
            }
            _ => {
                bail!(
                    "Expected declaration (function, type, or let), found {:?}",
                    self.current_token().token_type
                )
            }
        };

        let end_pos = if self.position > 0 {
            self.tokens[self.position - 1].position
        } else {
            start_pos
        };

        Ok(Positioned::new(result, Span::new(start_pos, end_pos)))
    }

    /// Parse a global variable declaration
    fn parse_global_variable(&mut self) -> Result<PositionedGlobalVariable> {
        let start_pos = self.current_token().position;
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

        let end_pos = if self.position > 0 {
            self.tokens[self.position - 1].position
        } else {
            start_pos
        };

        Ok(Positioned::new(
            GlobalVariable { name, value },
            Span::new(start_pos, end_pos),
        ))
    }

    /// Parse a function declaration
    fn parse_function(&mut self) -> Result<PositionedFunction> {
        let start_pos = self.current_token().position;
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

        let end_pos = if self.position > 0 {
            self.tokens[self.position - 1].position
        } else {
            start_pos
        };

        Ok(Positioned::new(
            Function {
                name,
                type_params,
                params,
                body,
            },
            Span::new(start_pos, end_pos),
        ))
    }

    /// Parse a struct declaration
    fn parse_struct_decl(&mut self) -> Result<PositionedStructDecl> {
        let start_pos = self.current_token().position;
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

        let end_pos = if self.position > 0 {
            self.tokens[self.position - 1].position
        } else {
            start_pos
        };

        Ok(Positioned::new(
            StructDecl {
                name,
                type_params,
                fields,
                methods,
            },
            Span::new(start_pos, end_pos),
        ))
    }

    pub fn parse_stmt(&mut self) -> Result<PositionedStmt> {
        let start_pos = self.current_token().position;
        let stmt = match &self.current_token().token_type {
            TokenType::Let => self.parse_let_stmt()?,
            TokenType::Return => self.parse_return_stmt()?,
            TokenType::If => self.parse_if_stmt()?,
            TokenType::While => self.parse_while_stmt()?,
            TokenType::Identifier(_) => {
                // Look ahead to see if this is a vector push, otherwise use unified assignment parser
                if self.position + 1 < self.tokens.len() {
                    match &self.tokens[self.position + 1].token_type {
                        TokenType::Push => self.parse_vector_push_stmt()?,
                        _ => {
                            // Use unified assignment parser for all other cases
                            // This handles: var = value, obj.field = value, arr[i] = value, self.data[i] = value
                            self.parse_assignment_or_expression()?
                        }
                    }
                } else {
                    let expr = self.parse_expression()?;
                    self.consume(TokenType::Semicolon)?;
                    Stmt::Expression(expr)
                }
            }
            _ => {
                let expr = self.parse_expression()?;
                self.consume(TokenType::Semicolon)?;
                Stmt::Expression(expr)
            }
        };

        let end_pos = if self.position > 0 {
            self.tokens[self.position - 1].position
        } else {
            start_pos
        };

        Ok(Positioned::new(stmt, Span::new(start_pos, end_pos)))
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
        let condition = self.parse_expression()?;
        self.consume(TokenType::Do)?;

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
            self.consume(TokenType::Do)?; // consume 'do'

            // Check for 'else if'
            if matches!(self.current_token().token_type, TokenType::If) {
                // Parse as a single if statement in the else branch
                let start_pos = self.current_token().position;
                let if_stmt = self.parse_if_stmt()?;
                let end_pos = if self.position > 0 {
                    self.tokens[self.position - 1].position
                } else {
                    start_pos
                };
                Some(vec![Positioned::new(
                    if_stmt,
                    Span::new(start_pos, end_pos),
                )])
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
        let condition = self.parse_expression()?;
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


    fn parse_vector_push_stmt(&mut self) -> Result<Stmt> {
        let vector = self.parse_identifier("in push statement")?;
        self.consume(TokenType::Push)?; // consume '<-'
        let value = self.parse_expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::VectorPush { vector, value })
    }



    fn parse_expression(&mut self) -> Result<PositionedExpr> {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> Result<PositionedExpr> {
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
    ) -> Result<PositionedExpr>
    where
        F: FnMut(&mut Self) -> Result<PositionedExpr>,
    {
        let start_pos = self.current_token().position;
        let mut left = operand_parser(self)?;

        loop {
            let mut found = false;
            for (token_type, binary_op) in operators {
                if std::mem::discriminant(&self.current_token().token_type)
                    == std::mem::discriminant(token_type)
                {
                    self.advance();
                    let right = operand_parser(self)?;
                    let end_pos = if self.position > 0 {
                        self.tokens[self.position - 1].position
                    } else {
                        start_pos
                    };
                    left = Positioned::new(
                        Expr::Binary {
                            left: Box::new(left),
                            op: *binary_op,
                            right: Box::new(right),
                        },
                        Span::new(start_pos, end_pos),
                    );
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

    fn parse_term(&mut self) -> Result<PositionedExpr> {
        self.parse_binary_left_assoc(
            |parser| parser.parse_factor(),
            &[
                (TokenType::Plus, BinaryOp::Add),
                (TokenType::Minus, BinaryOp::Subtract),
            ],
        )
    }

    fn parse_factor(&mut self) -> Result<PositionedExpr> {
        self.parse_binary_left_assoc(
            |parser| parser.parse_primary(),
            &[
                (TokenType::Star, BinaryOp::Multiply),
                (TokenType::Slash, BinaryOp::Divide),
            ],
        )
    }

    fn parse_primary(&mut self) -> Result<PositionedExpr> {
        let start_pos = self.current_token().position;

        match &self.current_token().token_type {
            TokenType::Int(value) => {
                let num = *value;
                self.advance();
                let end_pos = if self.position > 0 {
                    self.tokens[self.position - 1].position
                } else {
                    start_pos
                };
                return Ok(Positioned::new(
                    Expr::Int(num),
                    Span::new(start_pos, end_pos),
                ));
            }
            TokenType::Boolean(value) => {
                let bool_val = *value;
                self.advance();
                let end_pos = if self.position > 0 {
                    self.tokens[self.position - 1].position
                } else {
                    start_pos
                };
                return Ok(Positioned::new(
                    Expr::Boolean(bool_val),
                    Span::new(start_pos, end_pos),
                ));
            }
            TokenType::String(value) => {
                let string_val = value.clone();
                self.advance();
                let end_pos = if self.position > 0 {
                    self.tokens[self.position - 1].position
                } else {
                    start_pos
                };
                return Ok(Positioned::new(
                    Expr::String(string_val),
                    Span::new(start_pos, end_pos),
                ));
            }
            TokenType::Byte(value) => {
                let byte_val = *value;
                self.advance();
                let end_pos = if self.position > 0 {
                    self.tokens[self.position - 1].position
                } else {
                    start_pos
                };
                return Ok(Positioned::new(
                    Expr::Byte(byte_val),
                    Span::new(start_pos, end_pos),
                ));
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
                    let identifier_pos = Positioned::new(
                        Expr::Identifier(identifier),
                        Span::new(start_pos, start_pos),
                    );
                    let mut expr = Expr::Call {
                        callee: Box::new(identifier_pos),
                        args,
                    };

                    // Check for container indexing
                    if matches!(self.current_token().token_type, TokenType::LeftBracket) {
                        self.advance(); // consume '['
                        let index = self.parse_expression()?;
                        self.consume(TokenType::RightBracket)?;
                        let positioned_expr =
                            Positioned::new(expr, Span::new(start_pos, start_pos));
                        expr = Expr::Index {
                            container: Box::new(positioned_expr),
                            index: Box::new(index),
                            container_type: None, // Will be filled in by type checker
                        };
                    }

                    let end_pos = if self.position > 0 {
                        self.tokens[self.position - 1].position
                    } else {
                        start_pos
                    };
                    Ok(Positioned::new(expr, Span::new(start_pos, end_pos)))
                } else if matches!(self.current_token().token_type, TokenType::LeftBracket) {
                    // Container indexing - we'll determine the type at type checking
                    self.advance(); // consume '['
                    let index = self.parse_expression()?;
                    self.consume(TokenType::RightBracket)?;

                    let end_pos = if self.position > 0 {
                        self.tokens[self.position - 1].position
                    } else {
                        start_pos
                    };
                    let identifier_pos = Positioned::new(
                        Expr::Identifier(identifier),
                        Span::new(start_pos, start_pos),
                    );
                    Ok(Positioned::new(
                        Expr::Index {
                            container: Box::new(identifier_pos),
                            index: Box::new(index),
                            container_type: None, // Will be filled in by type checker
                        },
                        Span::new(start_pos, end_pos),
                    ))
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
                            let positioned_expr =
                                Positioned::new(expr, Span::new(start_pos, start_pos));
                            expr = Expr::MethodCall {
                                object: Some(Box::new(positioned_expr)),
                                type_name: None, // Will be filled by type checker
                                method: field_name,
                                args,
                            };
                        } else {
                            // Regular field access
                            let positioned_expr =
                                Positioned::new(expr, Span::new(start_pos, start_pos));
                            expr = Expr::FieldAccess {
                                object: Box::new(positioned_expr),
                                field: field_name,
                            };
                        }
                    }

                    // Check for indexing after field access
                    if matches!(self.current_token().token_type, TokenType::LeftBracket) {
                        self.advance(); // consume '['
                        let index = self.parse_expression()?;
                        self.consume(TokenType::RightBracket)?;
                        let positioned_expr = Positioned::new(expr, Span::new(start_pos, start_pos));
                        expr = Expr::Index {
                            container: Box::new(positioned_expr),
                            index: Box::new(index),
                            container_type: None, // Will be filled in by type checker
                        };
                    }

                    let end_pos = if self.position > 0 {
                        self.tokens[self.position - 1].position
                    } else {
                        start_pos
                    };
                    Ok(Positioned::new(expr, Span::new(start_pos, end_pos)))
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

                        let type_name = Type::from_string(&self.parse_type_name()?);
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
                        let end_pos = if self.position > 0 {
                            self.tokens[self.position - 1].position
                        } else {
                            start_pos
                        };
                        return Ok(Positioned::new(
                            Expr::StructNew {
                                type_name,
                                fields,
                                kind: StructNewKind::Pattern,
                            },
                            Span::new(start_pos, end_pos),
                        ));
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
                    let end_pos = if self.position > 0 {
                        self.tokens[self.position - 1].position
                    } else {
                        start_pos
                    };
                    Ok(Positioned::new(
                        Expr::VectorNew {
                            element_type,
                            initial_values,
                        },
                        Span::new(start_pos, end_pos),
                    ))
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
                    let end_pos = if self.position > 0 {
                        self.tokens[self.position - 1].position
                    } else {
                        start_pos
                    };
                    Ok(Positioned::new(
                        Expr::MapNew {
                            key_type,
                            value_type,
                            initial_pairs,
                        },
                        Span::new(start_pos, end_pos),
                    ))
                } else {
                    // Handle struct instantiation: new TypeName { .field = value, ... }
                    // Support generic types: new Container(int) { .field = value, ... }
                    let type_name = Type::from_string(&self.parse_type_name()?);

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
                    let end_pos = if self.position > 0 {
                        self.tokens[self.position - 1].position
                    } else {
                        start_pos
                    };
                    Ok(Positioned::new(
                        Expr::StructNew {
                            type_name,
                            fields,
                            kind: StructNewKind::Regular,
                        },
                        Span::new(start_pos, end_pos),
                    ))
                }
            }
            TokenType::LeftParen => {
                self.advance();

                // Check if this is a type expression for associated method call
                if matches!(self.current_token().token_type, TokenType::Type) {
                    self.advance(); // consume 'type'
                    let type_name = Type::from_string(&self.parse_type_name()?);
                    self.consume(TokenType::RightParen)?;

                    // Now check for .method(args) after the closing paren
                    if matches!(self.current_token().token_type, TokenType::Dot) {
                        self.advance(); // consume '.'
                        let method = match &self.current_token().token_type {
                            TokenType::Identifier(name) => {
                                let n = name.clone();
                                self.advance();
                                n
                            }
                            _ => bail!("Expected method name after '.'"),
                        };

                        // Parse method arguments
                        self.consume(TokenType::LeftParen)?;
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

                        let end_pos = if self.position > 0 {
                            self.tokens[self.position - 1].position
                        } else {
                            start_pos
                        };
                        Ok(Positioned::new(
                            Expr::MethodCall {
                                object: None, // No object for associated method calls
                                type_name: Some(type_name.to_string()),
                                method,
                                args,
                            },
                            Span::new(start_pos, end_pos),
                        ))
                    } else {
                        // Just a type expression in parentheses
                        let end_pos = if self.position > 0 {
                            self.tokens[self.position - 1].position
                        } else {
                            start_pos
                        };
                        Ok(Positioned::new(
                            Expr::TypeExpr { type_name },
                            Span::new(start_pos, end_pos),
                        ))
                    }
                } else {
                    // Regular parenthesized expression
                    let expr = self.parse_expression()?;
                    self.consume(TokenType::RightParen)?;
                    Ok(expr)
                }
            }
            TokenType::Type => {
                self.advance(); // consume 'type'
                let type_name = Type::from_string(&self.parse_type_name()?);
                let end_pos = if self.position > 0 {
                    self.tokens[self.position - 1].position
                } else {
                    start_pos
                };
                Ok(Positioned::new(
                    Expr::TypeExpr { type_name },
                    Span::new(start_pos, end_pos),
                ))
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

                let end_pos = if self.position > 0 {
                    self.tokens[self.position - 1].position
                } else {
                    start_pos
                };
                Ok(Positioned::new(
                    Expr::Alloc {
                        element_type,
                        size: Box::new(size),
                    },
                    Span::new(start_pos, end_pos),
                ))
            }
            _ => bail!("Unexpected token: {:?} at position {}", self.current_token().token_type, self.current_token().position),
        }
    }

    /// Check if an expression is a valid left-hand value for assignment
    fn is_valid_lvalue(expr: &Expr) -> bool {
        match expr {
            Expr::Identifier(_) => true,
            Expr::FieldAccess { .. } => true,
            Expr::Index { .. } => true,
            _ => false,
        }
    }


    fn parse_assignment_or_expression(&mut self) -> Result<Stmt> {
        // Parse a potentially complex left-hand value and check if it's followed by assignment
        let lvalue = self.parse_expression()?;
        
        // Check if this is an assignment
        if matches!(self.current_token().token_type, TokenType::Assign) {
            // Validate that the left-hand side is a valid lvalue
            if !Self::is_valid_lvalue(&lvalue.value) {
                bail!(
                    "Invalid left-hand side in assignment at position {}. Only variables, field access (obj.field), and indexing (arr[i]) are allowed.",
                    lvalue.span.start.unwrap_or(0)
                );
            }
            
            self.advance(); // consume '='
            let value = self.parse_expression()?;
            self.consume(TokenType::Semicolon)?;
            
            Ok(Stmt::Assign { lvalue, value })
        } else {
            // Not an assignment, treat as expression statement
            self.consume(TokenType::Semicolon)?;
            Ok(Stmt::Expression(lvalue))
        }
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
        match &result.declarations[0].value {
            Decl::Struct(struct_decl) => {
                assert_eq!(struct_decl.value.name, "Container");
                assert_eq!(struct_decl.value.type_params.len(), 1);
                assert_eq!(struct_decl.value.type_params[0], "T");
                assert_eq!(struct_decl.value.fields.len(), 1);
                assert_eq!(struct_decl.value.fields[0].name, "value");
                assert_eq!(struct_decl.value.fields[0].type_name, "T");
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
        match &result.declarations[0].value {
            Decl::Function(function) => {
                assert_eq!(function.value.name, "identity");
                assert_eq!(function.value.type_params.len(), 1);
                assert_eq!(function.value.type_params[0], "T");
                assert_eq!(function.value.params.len(), 1);
                assert_eq!(function.value.params[0].name, "value");
                assert_eq!(function.value.params[0].type_name, Some("T".to_string()));
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
        match &result.declarations[0].value {
            Decl::Struct(struct_decl) => {
                assert_eq!(struct_decl.value.name, "Point");
                assert_eq!(struct_decl.value.fields.len(), 1);
                assert_eq!(struct_decl.value.fields[0].name, "container");
                assert_eq!(struct_decl.value.fields[0].type_name, "Container(int)");
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

        if let Decl::Struct(struct_decl) = &program.declarations[0].value {
            assert_eq!(struct_decl.value.name, "Point");
            assert_eq!(struct_decl.value.fields.len(), 2);
            assert_eq!(struct_decl.value.methods.len(), 1);

            let method = &struct_decl.value.methods[0];
            assert_eq!(method.value.name, "sum");
            assert_eq!(method.value.params.len(), 1);
            assert_eq!(method.value.params[0].name, "self");
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
            type_name,
            method,
            args,
        } = expr.value
        {
            if let Some(obj) = object {
                if let Expr::Identifier(obj_name) = &obj.value {
                    assert_eq!(obj_name, "p");
                } else {
                    panic!("Expected identifier for object");
                }
            } else {
                panic!("Expected object for instance method call");
            }
            assert_eq!(method, "sum");
            assert_eq!(args.len(), 0);
            assert_eq!(type_name, None); // Should be None before type checking
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

        if let Decl::Struct(struct_decl) = &program.declarations[0].value {
            assert_eq!(struct_decl.value.name, "Point");
            assert_eq!(struct_decl.value.fields.len(), 1);
            assert_eq!(struct_decl.value.fields[0].name, "data");
            assert_eq!(struct_decl.value.fields[0].type_name, "[*]int");
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

        if let Expr::StructNew {
            type_name,
            fields,
            kind: StructNewKind::Pattern,
        } = expr.value
        {
            assert_eq!(type_name, Type::from_string("Point"));
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].0, "x");
            assert_eq!(fields[1].0, "y");
            if let Expr::Int(n) = fields[0].1.value {
                assert_eq!(n, 10);
            } else {
                panic!("Expected number expression for field value");
            }
            if let Expr::Int(n) = fields[1].1.value {
                assert_eq!(n, 20);
            } else {
                panic!("Expected number expression for field value");
            }
        } else {
            panic!(
                "Expected StructNew expression with kind=Pattern, got: {:?}",
                expr
            );
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
        if let Expr::StructNew {
            type_name,
            fields,
            kind: StructNewKind::Regular,
        } = expr1.value
        {
            assert_eq!(type_name, Type::from_string("Point"));
            assert_eq!(fields.len(), 2);
        } else {
            panic!("Expected StructNew expression");
        }

        // Second should be StructNew with kind=Pattern
        if let Expr::StructNew {
            type_name,
            fields,
            kind: StructNewKind::Pattern,
        } = expr2.value
        {
            assert_eq!(type_name, Type::from_string("Point"));
            assert_eq!(fields.len(), 2);
        } else {
            panic!("Expected StructNew expression with kind=Pattern");
        }
    }

    #[test]
    fn test_field_access_parsing() {
        let code = "self.data";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse().unwrap();

        if let Expr::FieldAccess { object, field } = expr.value {
            if let Expr::Identifier(obj_name) = &object.value {
                assert_eq!(obj_name, "self");
            } else {
                panic!("Expected identifier for object");
            }
            assert_eq!(field, "data");
        } else {
            panic!("Expected FieldAccess expression, got: {:?}", expr);
        }
    }

    #[test]
    fn test_index_access_parsing() {
        let code = "arr[i]";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse().unwrap();

        if let Expr::Index { container, index, .. } = expr.value {
            if let Expr::Identifier(container_name) = &container.value {
                assert_eq!(container_name, "arr");
            } else {
                panic!("Expected identifier for container");
            }
            if let Expr::Identifier(index_name) = &index.value {
                assert_eq!(index_name, "i");
            } else {
                panic!("Expected identifier for index");
            }
        } else {
            panic!("Expected Index expression, got: {:?}", expr);
        }
    }

    #[test]
    fn test_field_access_with_index_parsing() {
        let code = "self.data[i]";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse().unwrap();

        if let Expr::Index { container, index, .. } = expr.value {
            // The container should be field access (self.data)
            if let Expr::FieldAccess { object, field } = &container.value {
                if let Expr::Identifier(obj_name) = &object.value {
                    assert_eq!(obj_name, "self");
                } else {
                    panic!("Expected identifier for object in field access");
                }
                assert_eq!(field, "data");
            } else {
                panic!("Expected FieldAccess for container in index expression");
            }
            
            // The index should be 'i'
            if let Expr::Identifier(index_name) = &index.value {
                assert_eq!(index_name, "i");
            } else {
                panic!("Expected identifier for index");
            }
        } else {
            panic!("Expected Index expression, got: {:?}", expr);
        }
    }

    #[test]
    fn test_nested_field_access_with_index_parsing() {
        let code = "obj.container.data[index]";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse().unwrap();

        if let Expr::Index { container, index, .. } = expr.value {
            // The container should be nested field access (obj.container.data)
            if let Expr::FieldAccess { object, field } = &container.value {
                assert_eq!(field, "data");
                
                // The object should be another field access (obj.container)
                if let Expr::FieldAccess { object: inner_object, field: inner_field } = &object.value {
                    if let Expr::Identifier(obj_name) = &inner_object.value {
                        assert_eq!(obj_name, "obj");
                    } else {
                        panic!("Expected identifier for innermost object");
                    }
                    assert_eq!(inner_field, "container");
                } else {
                    panic!("Expected nested FieldAccess for object");
                }
            } else {
                panic!("Expected FieldAccess for container in index expression");
            }
            
            // The index should be 'index'
            if let Expr::Identifier(index_name) = &index.value {
                assert_eq!(index_name, "index");
            } else {
                panic!("Expected identifier for index");
            }
        } else {
            panic!("Expected Index expression, got: {:?}", expr);
        }
    }

    #[test]
    fn test_field_access_with_numeric_index_parsing() {
        let code = "self.data[0]";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse().unwrap();

        if let Expr::Index { container, index, .. } = expr.value {
            // The container should be field access (self.data)
            if let Expr::FieldAccess { object, field } = &container.value {
                if let Expr::Identifier(obj_name) = &object.value {
                    assert_eq!(obj_name, "self");
                } else {
                    panic!("Expected identifier for object in field access");
                }
                assert_eq!(field, "data");
            } else {
                panic!("Expected FieldAccess for container in index expression");
            }
            
            // The index should be numeric 0
            if let Expr::Int(index_num) = &index.value {
                assert_eq!(*index_num, 0);
            } else {
                panic!("Expected integer for index");
            }
        } else {
            panic!("Expected Index expression, got: {:?}", expr);
        }
    }

    #[test]
    fn test_field_access_with_expression_index_parsing() {
        let code = "self.data[i + 1]";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse().unwrap();

        if let Expr::Index { container, index, .. } = expr.value {
            // The container should be field access (self.data)
            if let Expr::FieldAccess { object, field } = &container.value {
                if let Expr::Identifier(obj_name) = &object.value {
                    assert_eq!(obj_name, "self");
                } else {
                    panic!("Expected identifier for object in field access");
                }
                assert_eq!(field, "data");
            } else {
                panic!("Expected FieldAccess for container in index expression");
            }
            
            // The index should be a binary expression (i + 1)
            if let Expr::Binary { left, op, right } = &index.value {
                if let Expr::Identifier(left_name) = &left.value {
                    assert_eq!(left_name, "i");
                } else {
                    panic!("Expected identifier for left operand");
                }
                assert_eq!(*op, BinaryOp::Add);
                if let Expr::Int(right_num) = &right.value {
                    assert_eq!(*right_num, 1);
                } else {
                    panic!("Expected integer for right operand");
                }
            } else {
                panic!("Expected Binary expression for index");
            }
        } else {
            panic!("Expected Index expression, got: {:?}", expr);
        }
    }

    #[test]
    fn test_simple_field_assignment_parsing() {
        let code = "obj.field = 42;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let stmt = parser.parse_stmt().unwrap();

        // Simple field assignments are now parsed as Assign since they use the new parser
        if let Stmt::Assign { lvalue, value } = stmt.value {
            if let Expr::FieldAccess { object, field } = &lvalue.value {
                if let Expr::Identifier(obj_name) = &object.value {
                    assert_eq!(obj_name, "obj");
                } else {
                    panic!("Expected identifier for object");
                }
                assert_eq!(field, "field");
            } else {
                panic!("Expected FieldAccess for lvalue");
            }
            if let Expr::Int(val) = value.value {
                assert_eq!(val, 42);
            } else {
                panic!("Expected integer for value");
            }
        } else {
            panic!("Expected Assign statement, got: {:?}", stmt);
        }
    }

    #[test]
    fn test_simple_index_assignment_parsing() {
        let code = "arr[0] = 42;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let stmt = parser.parse_stmt().unwrap();

        // Simple index assignments are now parsed as Assign since they use the new parser
        if let Stmt::Assign { lvalue, value } = stmt.value {
            if let Expr::Index { container, index, .. } = &lvalue.value {
                if let Expr::Identifier(container_name) = &container.value {
                    assert_eq!(container_name, "arr");
                } else {
                    panic!("Expected identifier for container");
                }
                if let Expr::Int(idx) = index.value {
                    assert_eq!(idx, 0);
                } else {
                    panic!("Expected integer for index");
                }
            } else {
                panic!("Expected Index for lvalue");
            }
            if let Expr::Int(val) = value.value {
                assert_eq!(val, 42);
            } else {
                panic!("Expected integer for value");
            }
        } else {
            panic!("Expected Assign statement, got: {:?}", stmt);
        }
    }

    #[test]
    fn test_complex_field_index_assignment_parsing() {
        // This test checks if self.data[i] = value can be parsed as Assign
        let code = "self.data[i] = 42;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let stmt = parser.parse_stmt().unwrap();

        if let Stmt::Assign { lvalue, value } = stmt.value {
            // The lvalue should be index access (self.data[i])
            if let Expr::Index { container, index, .. } = &lvalue.value {
                // The container should be field access (self.data)
                if let Expr::FieldAccess { object, field } = &container.value {
                    if let Expr::Identifier(obj_name) = &object.value {
                        assert_eq!(obj_name, "self");
                    } else {
                        panic!("Expected identifier for object in field access");
                    }
                    assert_eq!(field, "data");
                } else {
                    panic!("Expected FieldAccess for container in index expression");
                }
                
                // The index should be 'i'
                if let Expr::Identifier(index_name) = &index.value {
                    assert_eq!(index_name, "i");
                } else {
                    panic!("Expected identifier for index");
                }
            } else {
                panic!("Expected Index expression for lvalue");
            }
            
            // The value should be 42
            if let Expr::Int(val) = value.value {
                assert_eq!(val, 42);
            } else {
                panic!("Expected integer for value");
            }
        } else {
            panic!("Expected Assign statement, got: {:?}", stmt);
        }
    }

    #[test]
    fn test_complex_assignment_with_expression_parsing() {
        // This test checks if self.data[i] = self.data[i] + 1 can be parsed as Assign
        let code = "self.data[i] = self.data[i] + 1;";
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let stmt = parser.parse_stmt().unwrap();

        if let Stmt::Assign { lvalue, value } = stmt.value {
            // The lvalue should be index access (self.data[i])
            if let Expr::Index { container, index, .. } = &lvalue.value {
                // The container should be field access (self.data)
                if let Expr::FieldAccess { object, field } = &container.value {
                    if let Expr::Identifier(obj_name) = &object.value {
                        assert_eq!(obj_name, "self");
                    } else {
                        panic!("Expected identifier for object in field access");
                    }
                    assert_eq!(field, "data");
                } else {
                    panic!("Expected FieldAccess for container in index expression");
                }
                
                // The index should be 'i'
                if let Expr::Identifier(index_name) = &index.value {
                    assert_eq!(index_name, "i");
                } else {
                    panic!("Expected identifier for index");
                }
            } else {
                panic!("Expected Index expression for lvalue");
            }
            
            // The value should be a binary expression: self.data[i] + 1
            if let Expr::Binary { left, op, right } = &value.value {
                assert_eq!(*op, BinaryOp::Add);
                
                // Left side should be self.data[i]
                if let Expr::Index { container, index, .. } = &left.value {
                    if let Expr::FieldAccess { object, field } = &container.value {
                        if let Expr::Identifier(obj_name) = &object.value {
                            assert_eq!(obj_name, "self");
                        }
                        assert_eq!(field, "data");
                    }
                    if let Expr::Identifier(index_name) = &index.value {
                        assert_eq!(index_name, "i");
                    }
                }
                
                // Right side should be 1
                if let Expr::Int(val) = &right.value {
                    assert_eq!(*val, 1);
                } else {
                    panic!("Expected integer 1 for right operand");
                }
            } else {
                panic!("Expected Binary expression for value");
            }
        } else {
            panic!("Expected Assign statement, got: {:?}", stmt);
        }
    }
}
