pub mod ast;
pub mod lexer;
pub mod parser;
pub mod runtime;

use anyhow::Result;
use ast::TokenType;
use lexer::Lexer;
use parser::Parser;
use runtime::{Runtime, Value};

/// Execute Orbit source code and return the result
pub fn execute_code(code: &str) -> Result<Option<Value>> {
    let mut runtime = Runtime::new();
    
    // Try to parse the entire code as a sequence of statements first
    let mut lexer = Lexer::new(code);
    let tokens = lexer.tokenize()?;
    let mut parser = Parser::new(tokens);
    
    // Parse and execute statements until we reach EOF
    let mut last_value = None;
    while !matches!(parser.current_token().token_type, TokenType::Eof) {
        let stmt = parser.parse_stmt()?;
        let result = runtime.execute_stmt(&stmt)?;
        if let Some(value) = result {
            last_value = Some(value);
        }
    }
    
    Ok(last_value)
}

/// Execute Orbit source code from a file and return the result
pub fn execute_file(filename: &str) -> Result<Option<Value>> {
    let content = std::fs::read_to_string(filename)
        .map_err(|e| anyhow::anyhow!("Error reading file {}: {}", filename, e))?;
    execute_code(&content)
}

/// Execute a single Orbit expression and return the result
pub fn execute_expression(input: &str, runtime: &mut Runtime) -> Result<Value> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize()?;
    
    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;
    
    runtime.evaluate(&ast)
}

/// Execute a single Orbit statement and return the result
pub fn execute_statement(input: &str, runtime: &mut Runtime) -> Result<Option<Value>> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize()?;
    
    let mut parser = Parser::new(tokens);
    let stmt = parser.parse_stmt()?;
    
    runtime.execute_stmt(&stmt)
}