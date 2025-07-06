use crate::ast::Program;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::runtime::{Runtime, Value};
use crate::typecheck::TypeChecker;
use anyhow::Result;

/// Compiler configuration options
#[derive(Debug, Clone)]
pub struct CompilerOptions {
    pub enable_type_checking: bool,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        CompilerOptions {
            enable_type_checking: true,
        }
    }
}

/// The main compiler structure that orchestrates the compilation pipeline
pub struct Compiler {
    runtime: Runtime,
    options: CompilerOptions,
}

impl Compiler {
    /// Create a new compiler instance with default options
    pub fn new() -> Self {
        Compiler {
            runtime: Runtime::new(),
            options: CompilerOptions::default(),
        }
    }

    /// Create a new compiler instance with specific options
    pub fn new_with_options(options: CompilerOptions) -> Self {
        Compiler {
            runtime: Runtime::new(),
            options,
        }
    }

    /// Compile and execute Orbit source code, returning the last value
    pub fn execute(&mut self, code: &str) -> Result<Option<Value>> {
        let tokens = self.tokenize(code)?;
        let program = self.parse(tokens)?;
        
        if self.options.enable_type_checking {
            self.typecheck(&program)?;
        }
        
        self.runtime.execute_program(&program)
    }

    /// Compile and execute Orbit source code without type checking
    pub fn execute_unchecked(&mut self, code: &str) -> Result<Option<Value>> {
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

    /// Type check the program
    fn typecheck(&self, program: &Program) -> Result<()> {
        let mut type_checker = TypeChecker::new();
        type_checker.check_program(program)
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
