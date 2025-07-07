use crate::ast::Program;
use crate::desugar::Desugarer;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::runtime::{Runtime, Value};
use crate::typecheck::TypeChecker;
use crate::vm::VMCompiler;
use anyhow::Result;

/// Compiler configuration options
#[derive(Debug, Clone)]
pub struct CompilerOptions {
    /// Enable IR dumping
    pub dump_ir: bool,
    /// Output file for IR dump
    pub dump_ir_output: Option<String>,
    /// Enable stack printing during execution
    pub print_stacks: bool,
    /// Enable stack printing for specific function calls
    pub print_stacks_on_call: Option<String>,
    /// Enable profiling
    pub enable_profiling: bool,
    /// Output file for profiling results
    pub profile_output: Option<String>,
    /// Enable automatic loading of standard library
    pub enable_load_std: bool,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        CompilerOptions {
            dump_ir: false,
            dump_ir_output: None,
            print_stacks: false,
            print_stacks_on_call: None,
            enable_profiling: false,
            profile_output: None,
            enable_load_std: true,
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
        Self::new_with_options(CompilerOptions::default())
    }

    /// Create a new compiler instance with specific options
    pub fn new_with_options(options: CompilerOptions) -> Self {
        let runtime = if options.print_stacks || options.print_stacks_on_call.is_some() {
            Runtime::new_with_call_tracing(
                options.print_stacks,
                options.print_stacks_on_call.clone(),
            )
        } else {
            Runtime::new()
        };

        Compiler { runtime, options }
    }

    /// Compile and execute Orbit source code, returning the last value
    pub fn execute(&mut self, code: &str) -> Result<Option<Value>> {
        let processed_code = self.preprocess_code(code)?;
        let tokens = self.tokenize(&processed_code)?;
        let program = self.parse(tokens)?;
        self.execute_program(program)
    }

    /// Execute Orbit source code from a file
    pub fn execute_file(&mut self, filename: &str) -> Result<Option<Value>> {
        let content = std::fs::read_to_string(filename)
            .map_err(|e| anyhow::anyhow!("Error reading file {}: {}", filename, e))?;
        self.execute(&content)
    }

    /// Preprocess the code by prepending standard library if enabled
    fn preprocess_code(&self, code: &str) -> Result<String> {
        if !self.options.enable_load_std {
            return Ok(code.to_string());
        }

        let std_lib_path = "lib/std.io";
        match std::fs::read_to_string(std_lib_path) {
            Ok(std_content) => {
                Ok(format!("{}\n{}", std_content, code))
            }
            Err(_) => {
                // If std.io doesn't exist, just return the original code
                Ok(code.to_string())
            }
        }
    }

    /// Execute a parsed program with all configured options
    fn execute_program(&mut self, program: Program) -> Result<Option<Value>> {
        // 1. Type inference phase: analyze types and set object_type information
        let mut type_checker = TypeChecker::new();
        let mut program_with_type_info = program;

        // First register struct types and functions
        type_checker.check_program(&program_with_type_info)?;
        // Then perform type inference to set object_type fields
        type_checker.infer_types(&mut program_with_type_info)?;

        // 2. Desugar phase: transform method calls to function calls using type info
        let mut desugarer = Desugarer::new();
        let desugared_program = desugarer.desugar_program(program_with_type_info)?;

        // 3. Final type checking on desugared program
        let mut final_type_checker = TypeChecker::new();
        final_type_checker.check_program(&desugared_program)?;

        // 4. Handle IR dumping if requested
        if self.options.dump_ir {
            let mut vm_compiler = VMCompiler::new();
            let _instructions = vm_compiler.compile_program(&desugared_program);
            
            if let Some(dump_ir_output) = &self.options.dump_ir_output {
                vm_compiler
                    .dump_ir_to_file(dump_ir_output)
                    .map_err(|e| anyhow::anyhow!("Failed to write IR dump: {}", e))?;
            } else {
                // Default behavior: dump to stdout
                println!("{}", vm_compiler.dump_ir());
            }
        }

        // 5. Enable profiling if requested
        if self.options.enable_profiling {
            self.runtime.enable_profiling();
        }

        // 6. Execute the program
        let result = if self.options.print_stacks || self.options.print_stacks_on_call.is_some() {
            self.runtime
                .execute_program_with_options(&desugared_program, self.options.print_stacks)
        } else {
            self.runtime.execute_program(&desugared_program)
        };

        // 7. Handle profiling output
        if self.options.enable_profiling {
            if let Some(output_file) = &self.options.profile_output {
                self.runtime
                    .dump_profile_to_file(output_file)
                    .map_err(|e| anyhow::anyhow!("Failed to write profile: {}", e))?;
            } else {
                println!("{}", self.runtime.get_profile());
            }
        }

        result
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

/// Execute Orbit source code and return both the result and captured stdout
pub fn execute_code_with_output(code: &str) -> Result<(Option<Value>, String)> {
    let mut compiler = Compiler::new();
    
    // Enable output capture
    compiler.runtime_mut().enable_output_capture();
    
    // Execute the code
    let result = compiler.execute(code)?;
    
    // Get the captured output
    let captured_output = compiler.runtime_mut().take_captured_output().unwrap_or_default();
    
    Ok((result, captured_output))
}
