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

/// Helper function for common tokenization and parser creation
fn create_parser(input: &str) -> Result<Parser> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize()?;
    Ok(Parser::new(tokens))
}

/// Execute a single Orbit expression and return the result
/// This is a utility function for testing and REPL scenarios
pub fn execute_expression(input: &str, runtime: &mut Runtime) -> Result<Value> {
    let mut parser = create_parser(input)?;
    let ast = parser.parse()?;
    runtime.evaluate(&ast)
}

/// Execute a single Orbit statement and return the result
/// This is a utility function for testing and REPL scenarios
pub fn execute_statement(input: &str, runtime: &mut Runtime) -> Result<Option<Value>> {
    let mut parser = create_parser(input)?;
    let stmt = parser.parse_stmt()?;
    runtime.execute_stmt(&stmt)
}

/// Execute multiple statements and collect all output values
/// This is a utility function for testing scenarios where you have multiple expressions/statements
pub fn execute_statements(input: &str) -> Result<Vec<Value>> {
    let mut runtime = Runtime::new();
    let mut outputs = Vec::new();
    let mut parser = create_parser(input)?;

    while !matches!(
        parser.current_token().token_type,
        crate::ast::TokenType::Eof
    ) {
        let stmt = parser.parse_stmt()?;
        if let Some(value) = runtime.execute_stmt(&stmt)? {
            outputs.push(value);
        }
    }

    Ok(outputs)
}
