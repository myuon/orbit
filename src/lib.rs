pub mod ast;
pub mod compiler;
pub mod lexer;
pub mod parser;
pub mod profiler;
pub mod runtime;
pub mod typecheck;
pub mod vm;

// Re-export commonly used items
pub use compiler::{execute_code, execute_file, Compiler};
pub use runtime::{Runtime, Value};
pub use vm::VMCompiler;

use anyhow::Result;
use lexer::Lexer;
use parser::Parser;

/// Helper function for common tokenization and parser creation
fn create_parser(input: &str) -> Result<Parser> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize()?;
    Ok(Parser::new(tokens))
}

/// Execute a file with IR dumping capability
pub fn execute_file_with_ir_dump(filename: &str, ir_dump_file: &str) -> Result<Option<Value>> {
    use std::fs;

    let contents = fs::read_to_string(filename)?;
    let mut parser = create_parser(&contents)?;
    let mut program = parser.parse_program()?;

    // Perform type checking and inference
    let mut type_checker = crate::typecheck::TypeChecker::new();
    type_checker.infer_types(&mut program)?;
    type_checker.check_program(&program)?;

    // Compile to IR and dump
    let mut compiler = VMCompiler::new();
    let _instructions = compiler.compile_program(&program);

    // Dump IR to file
    compiler
        .dump_ir_to_file(ir_dump_file)
        .map_err(|e| anyhow::anyhow!("Failed to write IR dump: {}", e))?;

    // Execute the program
    let mut runtime = Runtime::new();
    runtime.execute_program(&program)
}

/// Execute a file with IR dumping and stack printing options
pub fn execute_file_with_ir_dump_and_options(
    filename: &str,
    ir_dump_file: &str,
    print_stacks: bool,
) -> Result<Option<Value>> {
    use std::fs;

    let contents = fs::read_to_string(filename)?;
    let mut parser = create_parser(&contents)?;
    let mut program = parser.parse_program()?;

    // Perform type checking and inference
    let mut type_checker = crate::typecheck::TypeChecker::new();
    type_checker.infer_types(&mut program)?;
    type_checker.check_program(&program)?;

    // Compile to IR and dump
    let mut compiler = VMCompiler::new();
    let _instructions = compiler.compile_program(&program);

    // Dump IR to file
    compiler
        .dump_ir_to_file(ir_dump_file)
        .map_err(|e| anyhow::anyhow!("Failed to write IR dump: {}", e))?;

    // Execute the program with stack printing option
    let mut runtime = Runtime::new();
    runtime.execute_program_with_options(&program, print_stacks)
}

/// Execute a file with optional stack printing
pub fn execute_file_with_options(filename: &str, print_stacks: bool) -> Result<Option<Value>> {
    use std::fs;

    let contents = fs::read_to_string(filename)?;
    let mut parser = create_parser(&contents)?;
    let mut program = parser.parse_program()?;

    // Perform type checking and inference
    let mut type_checker = crate::typecheck::TypeChecker::new();
    type_checker.infer_types(&mut program)?;
    type_checker.check_program(&program)?;

    // Execute the program with stack printing option
    let mut runtime = Runtime::new();
    runtime.execute_program_with_options(&program, print_stacks)
}

/// Execute a file with profiling enabled
pub fn execute_file_with_profiling(
    filename: &str,
    profile_output: Option<&str>,
) -> Result<Option<Value>> {
    use std::fs;

    let contents = fs::read_to_string(filename)?;
    let mut parser = create_parser(&contents)?;
    let mut program = parser.parse_program()?;

    // Perform type checking and inference
    let mut type_checker = crate::typecheck::TypeChecker::new();
    type_checker.infer_types(&mut program)?;
    type_checker.check_program(&program)?;

    // Execute the program with profiling enabled
    let mut runtime = Runtime::new();
    runtime.enable_profiling();

    let result = runtime.execute_program(&program)?;

    // Output profiling results
    if let Some(output_file) = profile_output {
        runtime
            .dump_profile_to_file(output_file)
            .map_err(|e| anyhow::anyhow!("Failed to write profile: {}", e))?;
    } else {
        println!("{}", runtime.get_profile());
    }

    Ok(result)
}

/// Execute a file with optional stack printing and call-specific tracing
pub fn execute_file_with_options_on_call(
    filename: &str,
    print_stacks: bool,
    print_stacks_on_call: Option<&str>,
) -> Result<Option<Value>> {
    use std::fs;

    let contents = fs::read_to_string(filename)?;
    let mut parser = create_parser(&contents)?;
    let mut program = parser.parse_program()?;

    // Perform type checking and inference
    let mut type_checker = crate::typecheck::TypeChecker::new();
    type_checker.infer_types(&mut program)?;
    type_checker.check_program(&program)?;

    // Execute the program with the specified options
    let mut runtime =
        Runtime::new_with_call_tracing(print_stacks, print_stacks_on_call.map(|s| s.to_string()));
    runtime.execute_program(&program)
}

/// Execute a file with IR dumping and call-specific tracing options
pub fn execute_file_with_ir_dump_and_options_on_call(
    filename: &str,
    ir_dump_file: &str,
    print_stacks: bool,
    print_stacks_on_call: Option<&str>,
) -> Result<Option<Value>> {
    use std::fs;

    let contents = fs::read_to_string(filename)?;
    let mut parser = create_parser(&contents)?;
    let mut program = parser.parse_program()?;

    // Perform type checking and inference
    let mut type_checker = crate::typecheck::TypeChecker::new();
    type_checker.infer_types(&mut program)?;
    type_checker.check_program(&program)?;

    // Compile to IR and dump
    let mut compiler = VMCompiler::new();
    let _instructions = compiler.compile_program(&program);

    // Dump IR to file
    compiler
        .dump_ir_to_file(ir_dump_file)
        .map_err(|e| anyhow::anyhow!("Failed to write IR dump: {}", e))?;

    // Execute the program with the specified options
    let mut runtime =
        Runtime::new_with_call_tracing(print_stacks, print_stacks_on_call.map(|s| s.to_string()));
    runtime.execute_program(&program)
}
