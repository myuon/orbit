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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOp, Expr};

    #[test]
    fn test_ast_to_ir_compilation() {
        // Test that Runtime properly compiles AST to IR and executes on VM
        let test_cases = vec![
            // Simple arithmetic
            (
                Expr::Binary {
                    left: Box::new(Expr::Number(2.0)),
                    op: BinaryOp::Add,
                    right: Box::new(Expr::Number(3.0)),
                },
                Value::Number(5.0),
            ),
            // Complex expression
            (
                Expr::Binary {
                    left: Box::new(Expr::Binary {
                        left: Box::new(Expr::Number(2.0)),
                        op: BinaryOp::Multiply,
                        right: Box::new(Expr::Number(3.0)),
                    }),
                    op: BinaryOp::Add,
                    right: Box::new(Expr::Number(4.0)),
                },
                Value::Number(10.0),
            ),
            // Comparison
            (
                Expr::Binary {
                    left: Box::new(Expr::Number(5.0)),
                    op: BinaryOp::Equal,
                    right: Box::new(Expr::Number(5.0)),
                },
                Value::Boolean(true),
            ),
            // Boolean values
            (Expr::Boolean(false), Value::Boolean(false)),
        ];

        for (expr, expected) in test_cases {
            let mut runtime = Runtime::new();
            let result = runtime.evaluate(&expr).unwrap();
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_expression_execution_via_ir() {
        let mut runtime = Runtime::new();

        // Test simple expression compiled to IR and executed on VM
        let result = execute_expression("2 + 3 * 4", &mut runtime).unwrap();
        assert_eq!(result, Value::Number(14.0));

        // Test boolean comparison compiled to IR and executed on VM
        let result = execute_expression("5 == 5", &mut runtime).unwrap();
        assert_eq!(result, Value::Boolean(true));

        // Test complex arithmetic expression
        let result = execute_expression("(10 - 4) / 2", &mut runtime).unwrap();
        assert_eq!(result, Value::Number(3.0));
    }
}
