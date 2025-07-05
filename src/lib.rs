pub mod ast;
pub mod compiler;
pub mod lexer;
pub mod parser;
pub mod runtime;

// Re-export commonly used items
pub use compiler::{execute_code, execute_file, Compiler};
pub use runtime::{Runtime, Value};

use anyhow::Result;
use lexer::Lexer;
use parser::Parser;

/// Execute a single Orbit expression and return the result
/// This is a utility function for testing and REPL scenarios
pub fn execute_expression(input: &str, runtime: &mut Runtime) -> Result<Value> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize()?;
    
    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;
    
    runtime.evaluate(&ast)
}

/// Execute a single Orbit statement and return the result
/// This is a utility function for testing and REPL scenarios
pub fn execute_statement(input: &str, runtime: &mut Runtime) -> Result<Option<Value>> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize()?;
    
    let mut parser = Parser::new(tokens);
    let stmt = parser.parse_stmt()?;
    
    runtime.execute_stmt(&stmt)
}