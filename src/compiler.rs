use crate::ast::Program;
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
        let program = self.parse(tokens)?;
        self.runtime.execute_program(&program)
    }

    /// Tokenize the source code
    fn tokenize(&self, code: &str) -> Result<Vec<crate::ast::Token>> {
        let mut lexer = Lexer::new(code);
        lexer.tokenize()
    }

    /// Parse tokens into a program
    fn parse(&self, tokens: Vec<crate::ast::Token>) -> Result<Program> {
        let mut parser = Parser::new(tokens);
        parser.parse_program()
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
        let code = "
            fun main() do
                return 42
            end
        ";
        let result = compiler.execute(code).unwrap();
        assert_eq!(result, Some(Value::Number(42.0)));
    }

    #[test]
    fn test_compiler_with_program() {
        let mut compiler = Compiler::new();
        let code = "
            fun main() do
                return 42
            end
        ";
        let result = compiler.execute(code).unwrap();
        assert_eq!(result, Some(Value::Number(42.0)));
    }

    #[test]
    fn test_compiler_with_statements() {
        let mut compiler = Compiler::new();
        let code = "
            fun main() do
                let x = 10;
                let y = 20;
                return x + y
            end
        ";
        let result = compiler.execute(code).unwrap();
        assert_eq!(result, Some(Value::Number(30.0)));
    }
}
