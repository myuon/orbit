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
    /// Enable IR dumping to a file
    pub ir_dump_file: Option<String>,
    /// Enable stack printing during execution
    pub print_stacks: bool,
    /// Enable stack printing for specific function calls
    pub print_stacks_on_call: Option<String>,
    /// Enable profiling
    pub enable_profiling: bool,
    /// Output file for profiling results
    pub profile_output: Option<String>,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        CompilerOptions {
            ir_dump_file: None,
            print_stacks: false,
            print_stacks_on_call: None,
            enable_profiling: false,
            profile_output: None,
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
            Runtime::new_with_call_tracing(options.print_stacks, options.print_stacks_on_call.clone())
        } else {
            Runtime::new()
        };
        
        Compiler {
            runtime,
            options,
        }
    }

    /// Compile and execute Orbit source code, returning the last value
    pub fn execute(&mut self, code: &str) -> Result<Option<Value>> {
        let tokens = self.tokenize(code)?;
        let program = self.parse(tokens)?;
        self.execute_program(program)
    }

    /// Execute Orbit source code from a file
    pub fn execute_file(&mut self, filename: &str) -> Result<Option<Value>> {
        let content = std::fs::read_to_string(filename)
            .map_err(|e| anyhow::anyhow!("Error reading file {}: {}", filename, e))?;
        self.execute(&content)
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
        if let Some(ir_dump_file) = &self.options.ir_dump_file {
            let mut vm_compiler = VMCompiler::new();
            let _instructions = vm_compiler.compile_program(&desugared_program);
            vm_compiler
                .dump_ir_to_file(ir_dump_file)
                .map_err(|e| anyhow::anyhow!("Failed to write IR dump: {}", e))?;
        }

        // 5. Enable profiling if requested
        if self.options.enable_profiling {
            self.runtime.enable_profiling();
        }

        // 6. Execute the program
        let result = if self.options.print_stacks || self.options.print_stacks_on_call.is_some() {
            self.runtime.execute_program_with_options(&desugared_program, self.options.print_stacks)
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

/// Execute Orbit source code from a file and return the result
pub fn execute_file(filename: &str) -> Result<Option<Value>> {
    let mut compiler = Compiler::new();
    compiler.execute_file(filename)
}

/// Execute a file with IR dumping capability
pub fn execute_file_with_ir_dump(filename: &str, ir_dump_file: &str) -> Result<Option<Value>> {
    let options = CompilerOptions {
        ir_dump_file: Some(ir_dump_file.to_string()),
        ..Default::default()
    };
    let mut compiler = Compiler::new_with_options(options);
    compiler.execute_file(filename)
}

/// Execute a file with IR dumping and stack printing options
pub fn execute_file_with_ir_dump_and_options(
    filename: &str,
    ir_dump_file: &str,
    print_stacks: bool,
) -> Result<Option<Value>> {
    let options = CompilerOptions {
        ir_dump_file: Some(ir_dump_file.to_string()),
        print_stacks,
        ..Default::default()
    };
    let mut compiler = Compiler::new_with_options(options);
    compiler.execute_file(filename)
}

/// Execute a file with optional stack printing
pub fn execute_file_with_options(filename: &str, print_stacks: bool) -> Result<Option<Value>> {
    let options = CompilerOptions {
        print_stacks,
        ..Default::default()
    };
    let mut compiler = Compiler::new_with_options(options);
    compiler.execute_file(filename)
}

/// Execute a file with profiling enabled
pub fn execute_file_with_profiling(
    filename: &str,
    profile_output: Option<&str>,
) -> Result<Option<Value>> {
    let options = CompilerOptions {
        enable_profiling: true,
        profile_output: profile_output.map(|s| s.to_string()),
        ..Default::default()
    };
    let mut compiler = Compiler::new_with_options(options);
    compiler.execute_file(filename)
}

/// Execute a file with optional stack printing and call-specific tracing
pub fn execute_file_with_options_on_call(
    filename: &str,
    print_stacks: bool,
    print_stacks_on_call: Option<&str>,
) -> Result<Option<Value>> {
    let options = CompilerOptions {
        print_stacks,
        print_stacks_on_call: print_stacks_on_call.map(|s| s.to_string()),
        ..Default::default()
    };
    let mut compiler = Compiler::new_with_options(options);
    compiler.execute_file(filename)
}

/// Execute a file with IR dumping and call-specific tracing options
pub fn execute_file_with_ir_dump_and_options_on_call(
    filename: &str,
    ir_dump_file: &str,
    print_stacks: bool,
    print_stacks_on_call: Option<&str>,
) -> Result<Option<Value>> {
    let options = CompilerOptions {
        ir_dump_file: Some(ir_dump_file.to_string()),
        print_stacks,
        print_stacks_on_call: print_stacks_on_call.map(|s| s.to_string()),
        ..Default::default()
    };
    let mut compiler = Compiler::new_with_options(options);
    compiler.execute_file(filename)
}
