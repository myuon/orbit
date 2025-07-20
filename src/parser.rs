use crate::ast::{
    BinaryOp, Decl, Expr, FunParam, Function, GlobalVariable, Positioned, PositionedDecl,
    PositionedExpr, PositionedFunction, PositionedGlobalVariable, PositionedStmt,
    PositionedStructDecl, Program, Span, Stmt, StructDecl, StructField, StructNewKind, Token,
    TokenType, Type,
};
use crate::bail_with_position;
use anyhow::{bail, Result};
use std::collections::HashSet;

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    eof_token: Token,
    /// Current type parameters in scope (for recognizing type parameter references)
    type_params_in_scope: HashSet<String>,
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
            type_params_in_scope: HashSet::new(),
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

    /// Create a span from a token position
    fn token_span(&self, token: &Token) -> Span {
        Span::single(token.position)
    }

    fn consume(&mut self, expected: TokenType) -> Result<()> {
        if std::mem::discriminant(&self.current_token().token_type)
            == std::mem::discriminant(&expected)
        {
            self.advance();
            Ok(())
        } else {
            bail_with_position!(
                self.token_span(self.current_token()),
                "Expected {:?}, found {:?}",
                expected,
                self.current_token().token_type
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
            _ => bail_with_position!(
                self.token_span(self.current_token()),
                "Expected variable name after 'let'"
            ),
        };

        // Check if there's an initialization
        let value = if matches!(self.current_token().token_type, TokenType::Assign) {
            self.consume(TokenType::Assign)?;
            Some(self.parse_expression()?)
        } else {
            None
        };

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
                        type_params.push(Type::TypeParameter(param_name.clone()));
                        // Add this type parameter to scope immediately so subsequent params can use it
                        self.type_params_in_scope.insert(param_name);
                    } else {
                        let param_type = self.parse_type()?;
                        params.push(FunParam {
                            name: param_name,
                            param_type: Some(param_type),
                        });
                    }
                } else {
                    params.push(FunParam {
                        name: param_name,
                        param_type: None,
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
            let _return_type = self.parse_type()?; // Parse but ignore for now
        }

        // Add type parameters to scope before parsing function body
        for type_param in &type_params {
            if let Type::TypeParameter(param_name) = type_param {
                self.type_params_in_scope.insert(param_name.clone());
            }
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

        // Remove type parameters from scope after parsing function body
        for type_param in &type_params {
            if let Type::TypeParameter(param_name) = type_param {
                self.type_params_in_scope.remove(param_name);
            }
        }

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

                params.push(Type::TypeParameter(param_name));

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

        // Add type parameters to scope before parsing fields and methods
        for type_param in &type_params {
            if let Type::TypeParameter(param_name) = type_param {
                self.type_params_in_scope.insert(param_name.clone());
            }
        }

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

                let field_type = self.parse_type()?;

                fields.push(StructField {
                    name: field_name,
                    field_type,
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

        // Remove type parameters from scope after parsing fields and methods
        for type_param in &type_params {
            if let Type::TypeParameter(param_name) = type_param {
                self.type_params_in_scope.remove(param_name);
            }
        }

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
            _ => bail_with_position!(
                self.token_span(self.current_token()),
                "Expected variable name after 'let'"
            ),
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

    /// Parse a base type (primitive types or struct names without generics)
    fn parse_base_type(&mut self) -> Result<Type> {
        match &self.current_token().token_type {
            TokenType::Identifier(name) => {
                let type_name = name.clone();
                self.advance();

                match type_name.as_str() {
                    "bool" | "boolean" => Ok(Type::Boolean),
                    "int" | "number" => Ok(Type::Int),
                    "string" => Ok(Type::String),
                    "byte" => Ok(Type::Byte),
                    _ => {
                        // Check if this is a type parameter in scope
                        if self.type_params_in_scope.contains(&type_name) {
                            Ok(Type::TypeParameter(type_name))
                        } else {
                            Ok(Type::Struct {
                                name: type_name,
                                args: vec![],
                            })
                        }
                    }
                }
            }
            _ => bail!("Expected type name"),
        }
    }

    /// Parse a type with optional generic arguments: BaseType(arg1, arg2, ...)
    fn parse_type(&mut self) -> Result<Type> {
        if matches!(self.current_token().token_type, TokenType::LeftBracket) {
            self.advance(); // consume '['
            self.consume(TokenType::Star)?; // consume '*'
            self.consume(TokenType::RightBracket)?; // consume ']'
            let inner_type = self.parse_type()?;
            Ok(Type::Pointer(Box::new(inner_type)))
        } else {
            let base_type = self.parse_base_type()?;

            // Check for generic instantiation: Type(arg1, arg2, ...)
            if matches!(self.current_token().token_type, TokenType::LeftParen) {
                self.advance(); // consume '('
                let mut args = Vec::new();

                while !matches!(self.current_token().token_type, TokenType::RightParen) {
                    if matches!(self.current_token().token_type, TokenType::Eof) {
                        bail!("Unexpected end of file in generic type arguments");
                    }
                    let arg_type = self.parse_type()?;
                    args.push(arg_type);

                    if matches!(self.current_token().token_type, TokenType::Comma) {
                        self.advance(); // consume ','
                    } else if !matches!(self.current_token().token_type, TokenType::RightParen) {
                        bail!("Expected ',' or ')' in generic type arguments");
                    }
                }

                self.consume(TokenType::RightParen)?;

                // Convert base_type to struct with arguments
                match base_type {
                    Type::Struct { name, args: _ } => Ok(Type::Struct { name, args }),
                    _ => bail!("Only struct types can have generic arguments"),
                }
            } else {
                Ok(base_type)
            }
        }
    }

    fn parse_vector_push_stmt(&mut self) -> Result<Stmt> {
        let vector = self.parse_identifier("in push statement")?;
        self.consume(TokenType::Push)?; // consume '<-'
        let value = self.parse_expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::VectorPush {
            vector,
            value,
            vector_type: None,
        })
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
            |parser| parser.parse_cast(),
            &[
                (TokenType::Star, BinaryOp::Multiply),
                (TokenType::Slash, BinaryOp::Divide),
                (TokenType::Modulo, BinaryOp::Modulo),
            ],
        )
    }

    fn parse_cast(&mut self) -> Result<PositionedExpr> {
        let mut expr = self.parse_primary()?;

        while matches!(self.current_token().token_type, TokenType::As) {
            self.advance(); // consume 'as'
            let target_type = self.parse_type()?;
            let start_pos = expr.span.start.unwrap_or(0);
            let end_pos = expr.span.end.unwrap_or(start_pos);

            expr = Positioned::new(
                Expr::Cast {
                    expr: Box::new(expr),
                    target_type,
                },
                Span::new(start_pos, end_pos),
            );
        }

        Ok(expr)
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
                            container_value_type: None, // Will be filled in by type checker
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
                            container_value_type: None, // Will be filled in by type checker
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
                        let positioned_expr =
                            Positioned::new(expr, Span::new(start_pos, start_pos));
                        expr = Expr::Index {
                            container: Box::new(positioned_expr),
                            index: Box::new(index),
                            container_type: None, // Will be filled in by type checker
                            container_value_type: None, // Will be filled in by type checker
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

                        let type_name = self.parse_type()?;
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
                } else {
                    // Handle struct instantiation: new TypeName { .field = value, ... }
                    // Support generic types: new Container(int) { .field = value, ... }
                    let type_name = self.parse_type()?;

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
                    let type_name = self.parse_type()?;
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
                let type_name = self.parse_type()?;
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
                let element_type = self.parse_type()?;

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
            TokenType::Sizeof => {
                self.advance(); // consume 'sizeof'
                self.consume(TokenType::LeftParen)?; // consume '('
                self.consume(TokenType::Type)?; // consume 'type'

                // Parse the type
                let type_name = self.parse_type()?;

                self.consume(TokenType::RightParen)?; // consume ')'

                let end_pos = if self.position > 0 {
                    self.tokens[self.position - 1].position
                } else {
                    start_pos
                };
                Ok(Positioned::new(
                    Expr::Sizeof { type_name },
                    Span::new(start_pos, end_pos),
                ))
            }
            _ => bail_with_position!(
                self.token_span(self.current_token()),
                "Unexpected token: {:?}",
                self.current_token().token_type
            ),
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
                bail_with_position!(
                    lvalue.span.clone(),
                    "Invalid left-hand side in assignment. Only variables, field access (obj.field), and indexing (arr[i]) are allowed."
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

    struct DeclarationTestCase {
        name: &'static str,
        input: &'static str,
        validate: fn(&Program),
    }

    #[test]
    fn test_declaration_parsing() {
        let test_cases = vec![
            DeclarationTestCase {
                name: "generic struct",
                input: "type Container(T: type) = struct { value: T, };",
                validate: |program| {
                    assert_eq!(program.declarations.len(), 1);
                    match &program.declarations[0].value {
                        Decl::Struct(struct_decl) => {
                            assert_eq!(struct_decl.value.name, "Container");
                            assert_eq!(struct_decl.value.type_params.len(), 1);
                            assert_eq!(struct_decl.value.type_params[0], Type::TypeParameter("T".to_string()));
                            assert_eq!(struct_decl.value.fields.len(), 1);
                            assert_eq!(struct_decl.value.fields[0].name, "value");
                            assert_eq!(struct_decl.value.fields[0].field_type, Type::TypeParameter("T".to_string()));
                        }
                        _ => panic!("Expected struct declaration"),
                    }
                },
            },
            DeclarationTestCase {
                name: "generic function",
                input: "fun identity(T: type, value: T): T do return value; end",
                validate: |program| {
                    assert_eq!(program.declarations.len(), 1);
                    match &program.declarations[0].value {
                        Decl::Function(function) => {
                            assert_eq!(function.value.name, "identity");
                            assert_eq!(function.value.type_params.len(), 1);
                            assert_eq!(function.value.type_params[0], Type::TypeParameter("T".to_string()));
                            assert_eq!(function.value.params.len(), 1);
                            assert_eq!(function.value.params[0].name, "value");
                            assert_eq!(function.value.params[0].param_type, Some(Type::TypeParameter("T".to_string())));
                        }
                        _ => panic!("Expected function declaration"),
                    }
                },
            },
            DeclarationTestCase {
                name: "generic type instantiation",
                input: "type Point = struct { container: Container(int), };",
                validate: |program| {
                    assert_eq!(program.declarations.len(), 1);
                    match &program.declarations[0].value {
                        Decl::Struct(struct_decl) => {
                            assert_eq!(struct_decl.value.name, "Point");
                            assert_eq!(struct_decl.value.fields.len(), 1);
                            assert_eq!(struct_decl.value.fields[0].name, "container");
                            assert_eq!(struct_decl.value.fields[0].field_type, Type::Struct { name: "Container".to_string(), args: vec![Type::Int] });
                        }
                        _ => panic!("Expected struct declaration"),
                    }
                },
            },
            DeclarationTestCase {
                name: "struct with methods",
                input: "type Point = struct { x: int, y: int, fun sum(self: Point) do return self.x + self.y; end };",
                validate: |program| {
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
                },
            },
            DeclarationTestCase {
                name: "pointer type",
                input: "type Point = struct { data: [*]int };",
                validate: |program| {
                    assert_eq!(program.declarations.len(), 1);
                    if let Decl::Struct(struct_decl) = &program.declarations[0].value {
                        assert_eq!(struct_decl.value.name, "Point");
                        assert_eq!(struct_decl.value.fields.len(), 1);
                        assert_eq!(struct_decl.value.fields[0].name, "data");
                        assert_eq!(struct_decl.value.fields[0].field_type, Type::Pointer(Box::new(Type::Int)));
                    } else {
                        panic!("Expected struct declaration");
                    }
                },
            },
        ];

        for test_case in test_cases {
            let mut lexer = Lexer::new(test_case.input);
            let tokens = lexer
                .tokenize()
                .unwrap_or_else(|e| panic!("Tokenize failed for {}: {}", test_case.name, e));
            let mut parser = Parser::new(tokens);
            let result = parser
                .parse_program()
                .unwrap_or_else(|e| panic!("Parse failed for {}: {}", test_case.name, e));
            (test_case.validate)(&result);
        }
    }

    struct ExpressionTestCase {
        name: &'static str,
        input: &'static str,
        validate: fn(&PositionedExpr),
    }

    #[test]
    fn test_expression_parsing() {
        let test_cases = vec![
            ExpressionTestCase {
                name: "method call",
                input: "p.sum()",
                validate: |expr| {
                    if let Expr::MethodCall {
                        object,
                        type_name,
                        method,
                        args,
                    } = &expr.value
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
                        assert_eq!(*type_name, None);
                    } else {
                        panic!("Expected method call, got: {:?}", expr);
                    }
                },
            },
            ExpressionTestCase {
                name: "field access",
                input: "self.data",
                validate: |expr| {
                    if let Expr::FieldAccess { object, field } = &expr.value {
                        if let Expr::Identifier(obj_name) = &object.value {
                            assert_eq!(obj_name, "self");
                        } else {
                            panic!("Expected identifier for object");
                        }
                        assert_eq!(field, "data");
                    } else {
                        panic!("Expected FieldAccess expression, got: {:?}", expr);
                    }
                },
            },
            ExpressionTestCase {
                name: "index access",
                input: "arr[i]",
                validate: |expr| {
                    if let Expr::Index {
                        container, index, ..
                    } = &expr.value
                    {
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
                },
            },
            ExpressionTestCase {
                name: "field access with index",
                input: "self.data[i]",
                validate: |expr| {
                    if let Expr::Index {
                        container, index, ..
                    } = &expr.value
                    {
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
                        if let Expr::Identifier(index_name) = &index.value {
                            assert_eq!(index_name, "i");
                        } else {
                            panic!("Expected identifier for index");
                        }
                    } else {
                        panic!("Expected Index expression, got: {:?}", expr);
                    }
                },
            },
            ExpressionTestCase {
                name: "nested field access with index",
                input: "obj.container.data[index]",
                validate: |expr| {
                    if let Expr::Index {
                        container, index, ..
                    } = &expr.value
                    {
                        if let Expr::FieldAccess { object, field } = &container.value {
                            assert_eq!(field, "data");
                            if let Expr::FieldAccess {
                                object: inner_object,
                                field: inner_field,
                            } = &object.value
                            {
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
                        if let Expr::Identifier(index_name) = &index.value {
                            assert_eq!(index_name, "index");
                        } else {
                            panic!("Expected identifier for index");
                        }
                    } else {
                        panic!("Expected Index expression, got: {:?}", expr);
                    }
                },
            },
            ExpressionTestCase {
                name: "field access with numeric index",
                input: "self.data[0]",
                validate: |expr| {
                    if let Expr::Index {
                        container, index, ..
                    } = &expr.value
                    {
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
                        if let Expr::Int(index_num) = &index.value {
                            assert_eq!(*index_num, 0);
                        } else {
                            panic!("Expected integer for index");
                        }
                    } else {
                        panic!("Expected Index expression, got: {:?}", expr);
                    }
                },
            },
            ExpressionTestCase {
                name: "field access with expression index",
                input: "self.data[i + 1]",
                validate: |expr| {
                    if let Expr::Index {
                        container, index, ..
                    } = &expr.value
                    {
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
                },
            },
        ];

        for test_case in test_cases {
            let mut lexer = Lexer::new(test_case.input);
            let tokens = lexer
                .tokenize()
                .unwrap_or_else(|e| panic!("Tokenize failed for {}: {}", test_case.name, e));
            let mut parser = Parser::new(tokens);
            let expr = parser
                .parse()
                .unwrap_or_else(|e| panic!("Parse failed for {}: {}", test_case.name, e));
            (test_case.validate)(&expr);
        }
    }

    struct StructNewTestCase {
        name: &'static str,
        input: &'static str,
        expected_kind: StructNewKind,
        expected_type: &'static str,
        expected_fields: usize,
    }

    #[test]
    fn test_struct_new_parsing() {
        let test_cases = vec![
            StructNewTestCase {
                name: "pattern syntax",
                input: "new(struct) Point { .x = 10, .y = 20 }",
                expected_kind: StructNewKind::Pattern,
                expected_type: "Point",
                expected_fields: 2,
            },
            StructNewTestCase {
                name: "regular syntax",
                input: "new Point { .x = 10, .y = 20 }",
                expected_kind: StructNewKind::Regular,
                expected_type: "Point",
                expected_fields: 2,
            },
        ];

        for test_case in test_cases {
            let mut lexer = Lexer::new(test_case.input);
            let tokens = lexer
                .tokenize()
                .unwrap_or_else(|e| panic!("Tokenize failed for {}: {}", test_case.name, e));
            let mut parser = Parser::new(tokens);
            let expr = parser
                .parse()
                .unwrap_or_else(|e| panic!("Parse failed for {}: {}", test_case.name, e));

            if let Expr::StructNew {
                type_name,
                fields,
                kind,
            } = expr.value
            {
                let expected_type = Type::Struct {
                    name: test_case.expected_type.to_string(),
                    args: vec![],
                };
                assert_eq!(type_name, expected_type);
                assert_eq!(fields.len(), test_case.expected_fields);
                assert_eq!(kind, test_case.expected_kind);
                if test_case.expected_fields > 0 {
                    assert_eq!(fields[0].0, "x");
                    if let Expr::Int(n) = fields[0].1.value {
                        assert_eq!(n, 10);
                    } else {
                        panic!("Expected number expression for field value");
                    }
                }
                if test_case.expected_fields > 1 {
                    assert_eq!(fields[1].0, "y");
                    if let Expr::Int(n) = fields[1].1.value {
                        assert_eq!(n, 20);
                    } else {
                        panic!("Expected number expression for field value");
                    }
                }
            } else {
                panic!("Expected StructNew expression, got: {:?}", expr);
            }
        }
    }

    struct AssignmentTestCase {
        name: &'static str,
        input: &'static str,
        validate_lvalue: fn(&Expr),
        validate_rvalue: fn(&Expr),
    }

    #[test]
    fn test_assignment_parsing() {
        let test_cases = vec![
            AssignmentTestCase {
                name: "simple field assignment",
                input: "obj.field = 42;",
                validate_lvalue: |lvalue| {
                    if let Expr::FieldAccess { object, field } = lvalue {
                        if let Expr::Identifier(obj_name) = &object.value {
                            assert_eq!(obj_name, "obj");
                        } else {
                            panic!("Expected identifier for object");
                        }
                        assert_eq!(field, "field");
                    } else {
                        panic!("Expected FieldAccess for lvalue");
                    }
                },
                validate_rvalue: |rvalue| {
                    if let Expr::Int(val) = rvalue {
                        assert_eq!(*val, 42);
                    } else {
                        panic!("Expected integer for value");
                    }
                },
            },
            AssignmentTestCase {
                name: "simple index assignment",
                input: "arr[0] = 42;",
                validate_lvalue: |lvalue| {
                    if let Expr::Index {
                        container, index, ..
                    } = lvalue
                    {
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
                },
                validate_rvalue: |rvalue| {
                    if let Expr::Int(val) = rvalue {
                        assert_eq!(*val, 42);
                    } else {
                        panic!("Expected integer for value");
                    }
                },
            },
            AssignmentTestCase {
                name: "complex field index assignment",
                input: "self.data[i] = 42;",
                validate_lvalue: |lvalue| {
                    if let Expr::Index {
                        container, index, ..
                    } = lvalue
                    {
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
                        if let Expr::Identifier(index_name) = &index.value {
                            assert_eq!(index_name, "i");
                        } else {
                            panic!("Expected identifier for index");
                        }
                    } else {
                        panic!("Expected Index expression for lvalue");
                    }
                },
                validate_rvalue: |rvalue| {
                    if let Expr::Int(val) = rvalue {
                        assert_eq!(*val, 42);
                    } else {
                        panic!("Expected integer for value");
                    }
                },
            },
            AssignmentTestCase {
                name: "complex assignment with expression",
                input: "self.data[i] = self.data[i] + 1;",
                validate_lvalue: |lvalue| {
                    if let Expr::Index {
                        container, index, ..
                    } = lvalue
                    {
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
                        if let Expr::Identifier(index_name) = &index.value {
                            assert_eq!(index_name, "i");
                        } else {
                            panic!("Expected identifier for index");
                        }
                    } else {
                        panic!("Expected Index expression for lvalue");
                    }
                },
                validate_rvalue: |rvalue| {
                    if let Expr::Binary { left, op, right } = rvalue {
                        assert_eq!(*op, BinaryOp::Add);
                        if let Expr::Index {
                            container, index, ..
                        } = &left.value
                        {
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
                        if let Expr::Int(val) = &right.value {
                            assert_eq!(*val, 1);
                        } else {
                            panic!("Expected integer 1 for right operand");
                        }
                    } else {
                        panic!("Expected Binary expression for value");
                    }
                },
            },
        ];

        for test_case in test_cases {
            let mut lexer = Lexer::new(test_case.input);
            let tokens = lexer
                .tokenize()
                .unwrap_or_else(|e| panic!("Tokenize failed for {}: {}", test_case.name, e));
            let mut parser = Parser::new(tokens);
            let stmt = parser
                .parse_stmt()
                .unwrap_or_else(|e| panic!("Parse failed for {}: {}", test_case.name, e));

            if let Stmt::Assign { lvalue, value } = stmt.value {
                (test_case.validate_lvalue)(&lvalue.value);
                (test_case.validate_rvalue)(&value.value);
            } else {
                panic!("Expected Assign statement, got: {:?}", stmt);
            }
        }
    }
}
