use crate::ast::{Stmt, TokenType};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::runtime::{Runtime, Value};
use anyhow::Result;

/// The main compiler structure that orchestrates the compilation pipeline
pub struct Compiler {
    runtime: Runtime,
}

impl Compiler {
    /// Create a new compiler instance
    pub fn new() -> Self {
        Compiler {
            runtime: Runtime::new(),
        }
    }

    /// Compile and execute Orbit source code, returning the last value
    pub fn execute(&mut self, code: &str) -> Result<Option<Value>> {
        let tokens = self.tokenize(code)?;
        let statements = self.parse(tokens)?;
        self.evaluate(statements)
    }

    /// Compile and execute Orbit source code, collecting all output values
    pub fn execute_with_output(&mut self, code: &str) -> Result<Vec<Value>> {
        let tokens = self.tokenize(code)?;
        let statements = self.parse(tokens)?;
        self.evaluate_all(statements)
    }

    /// Tokenize the source code
    fn tokenize(&self, code: &str) -> Result<Vec<crate::ast::Token>> {
        let mut lexer = Lexer::new(code);
        lexer.tokenize()
    }

    /// Parse tokens into statements
    fn parse(&self, tokens: Vec<crate::ast::Token>) -> Result<Vec<Stmt>> {
        let mut parser = Parser::new(tokens);
        let mut statements = Vec::new();

        while !matches!(parser.current_token().token_type, TokenType::Eof) {
            statements.push(parser.parse_stmt()?);
        }

        Ok(statements)
    }

    /// Evaluate statements and return the last value
    fn evaluate(&mut self, statements: Vec<Stmt>) -> Result<Option<Value>> {
        let mut last_value = None;

        for stmt in statements {
            if let Some(value) = self.runtime.execute_stmt(&stmt)? {
                last_value = Some(value);
            }
        }

        Ok(last_value)
    }

    /// Evaluate statements and collect all output values
    fn evaluate_all(&mut self, statements: Vec<Stmt>) -> Result<Vec<Value>> {
        let mut outputs = Vec::new();

        for stmt in statements {
            if let Some(value) = self.runtime.execute_stmt(&stmt)? {
                outputs.push(value);
            }
        }

        Ok(outputs)
    }

    /// Get a mutable reference to the runtime for direct manipulation
    pub fn runtime_mut(&mut self) -> &mut Runtime {
        &mut self.runtime
    }
}

/// Execute Orbit source code and return the result
pub fn execute_code(code: &str) -> Result<Option<Value>> {
    let mut compiler = Compiler::new();
    compiler.execute(code)
}

/// Execute Orbit source code and collect all output values
pub fn execute_code_with_output(code: &str) -> Result<Vec<Value>> {
    let mut compiler = Compiler::new();
    compiler.execute_with_output(code)
}

/// Execute Orbit source code from a file and return the result
pub fn execute_file(filename: &str) -> Result<Option<Value>> {
    let content = std::fs::read_to_string(filename)
        .map_err(|e| anyhow::anyhow!("Error reading file {}: {}", filename, e))?;
    execute_code(&content)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compiler_basic_execution() {
        let mut compiler = Compiler::new();
        let result = compiler.execute("42").unwrap();
        assert_eq!(result, Some(Value::Number(42.0)));
    }

    #[test]
    fn test_compiler_with_output() {
        let mut compiler = Compiler::new();
        let results = compiler.execute_with_output("1\n2\n3").unwrap();
        assert_eq!(results, vec![
            Value::Number(1.0),
            Value::Number(2.0),
            Value::Number(3.0),
        ]);
    }

    #[test]
    fn test_compiler_with_statements() {
        let mut compiler = Compiler::new();
        let code = "
            let x = 10;
            let y = 20;
            x + y
        ";
        let result = compiler.execute(code).unwrap();
        assert_eq!(result, Some(Value::Number(30.0)));
    }
}