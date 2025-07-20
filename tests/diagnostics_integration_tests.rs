//! Integration tests for position-based error reporting
//! These tests verify that errors include accurate line/column information

use orbit::diagnostics::{Diagnostic, PositionCalculator};
use orbit::{Compiler, CompilerOptions};
use std::fs;

#[test]
fn test_parse_error_position() {
    // Disable std.ob to get cleaner position information
    let mut options = CompilerOptions::default();
    options.enable_load_std = false;
    let mut compiler = Compiler::new_with_options(options);

    // Test parsing error - missing variable name after let
    let result = compiler.execute_file("tests/testcase/errors/parse_error_missing_var_name.ob");

    assert!(result.is_err());
    let err = result.unwrap_err();
    let error_msg = format!("{:?}", err); // Get full error chain

    // The error message should now contain proper position information
    assert!(error_msg.contains("parse_error_missing_var_name.ob:2:"));
    assert!(error_msg.contains("Expected variable name after 'let'"));
}

#[test]
fn test_typecheck_error_position() {
    // Disable std.ob to get cleaner position information
    let mut options = CompilerOptions::default();
    options.enable_load_std = false;
    let mut compiler = Compiler::new_with_options(options);

    // Test type error - undefined variable
    let result = compiler.execute_file("tests/testcase/errors/typecheck_undefined_variable.ob");

    assert!(result.is_err());
    let err = result.unwrap_err();
    let error_msg = format!("{:?}", err);
    
    println!("Actual error message: {}", error_msg);

    // Should contain position information for undefined variable
    assert!(error_msg.contains("Undefined variable: unknown_var"));
    assert!(error_msg.contains("typecheck_undefined_variable.ob:2:"));
}

#[test]
fn test_if_condition_error_position() {
    // Disable std.ob to get cleaner position information
    let mut options = CompilerOptions::default();
    options.enable_load_std = false;
    let mut compiler = Compiler::new_with_options(options);

    // Test type error - if condition must be boolean
    let result = compiler.execute_file("tests/testcase/errors/typecheck_if_condition_error.ob");

    assert!(result.is_err());
    let err = result.unwrap_err();
    let error_msg = format!("{:?}", err);

    // Should contain position information for the condition
    assert!(error_msg.contains("If condition must be boolean"));
    assert!(error_msg.contains("typecheck_if_condition_error.ob:2:"));
}

#[test]
fn test_while_condition_error_position() {
    // Disable std.ob to get cleaner position information
    let mut options = CompilerOptions::default();
    options.enable_load_std = false;
    let mut compiler = Compiler::new_with_options(options);

    // Test type error - while condition must be boolean
    let result = compiler.execute_file("tests/testcase/errors/typecheck_while_condition_error.ob");

    assert!(result.is_err());
    let err = result.unwrap_err();
    let error_msg = format!("{:?}", err);

    // Should contain position information for the condition
    assert!(error_msg.contains("While condition must be boolean"));
    assert!(error_msg.contains("typecheck_while_condition_error.ob:2:"));
}

#[test]
fn test_std_lib_position_calculation() {
    // Test that positions are correctly calculated when std.ob is loaded
    let std_content = "type TestType = struct {};\n".to_string();
    let user_code = "fun main() do\n  let x = unknown_var;\nend";
    let full_source = format!("{}\n{}", std_content, user_code);

    let calc = PositionCalculator::new(full_source, Some(std_content.clone()));

    // Test position in std.ob - "TestType" starts at position 5 (0-indexed)
    let std_loc = calc.position_to_location(5, "test.ob"); // Position of "TestType"
    assert_eq!(std_loc.file, "std.ob");
    assert_eq!(std_loc.line, 1);
    assert_eq!(std_loc.column, 6); // 1-indexed column

    // Test position in user code
    let user_start = std_content.len() + 1; // +1 for newline
                                            // "unknown_var" is at position 8 in the user_code string ("  let x = unknown_var")
    let unknown_var_offset = user_code.find("unknown_var").unwrap();
    let user_loc = calc.position_to_location(user_start + unknown_var_offset, "test.ob");
    assert_eq!(user_loc.file, "test.ob");
    assert_eq!(user_loc.line, 2);
    // Column calculation: line starts with "  let x = " (10 chars) + 1 for 1-indexed = 11
    assert_eq!(user_loc.column, 11);
}

#[test]
fn test_diagnostic_formatting() {
    let source = "fun main() do\n  let x = unknown_var;\nend";

    let calc = PositionCalculator::new(source.to_string(), None);
    let unknown_var_pos = source.find("unknown_var").unwrap();
    let location = calc.position_to_location(unknown_var_pos, "test.ob");

    let location_clone = location.clone();
    let diagnostic = Diagnostic::error_at("Undefined variable 'unknown_var'".to_string(), location)
        .with_help("Check if the variable is declared in scope".to_string());

    let formatted = diagnostic.format(Some(source));

    // Should contain file, line, column
    assert!(formatted.contains(&format!(
        "test.ob:{}:{}:",
        location_clone.line, location_clone.column
    )));
    assert!(formatted.contains("error: Undefined variable 'unknown_var'"));
    assert!(formatted.contains("let x = unknown_var;"));
    assert!(formatted.contains("^"));
    assert!(formatted.contains("help: Check if the variable is declared in scope"));
}

#[test]
fn test_multiline_position_calculation() {
    let source = fs::read_to_string("tests/testcase/errors/multiline_position_test.ob")
        .expect("Failed to read test file");
    let calc = PositionCalculator::new(source.clone(), None);

    // Position of "z" on line 3
    let z_pos = source.find('z').unwrap();
    let loc = calc.position_to_location(z_pos, "test.ob");
    assert_eq!(loc.line, 3);
    assert_eq!(loc.column, 13);
}

#[test]
fn test_error_position_with_disabled_std_lib() {
    // Disable std lib loading
    let mut options = CompilerOptions::default();
    options.enable_load_std = false;
    let mut compiler = Compiler::new_with_options(options);

    // Test that positions work correctly without std.ob
    let result = compiler.execute_file("tests/testcase/errors/typecheck_undefined_variable.ob");

    assert!(result.is_err());
    let err = result.unwrap_err();
    let error_msg = format!("{:?}", err);

    // Should still contain position information
    assert!(error_msg.contains("Undefined variable: unknown_var"));
    assert!(error_msg.contains("typecheck_undefined_variable.ob:2:"));
}

#[test]
fn test_error_position_with_enabled_std_lib() {
    // Test with std.ob enabled (default)
    let mut compiler = Compiler::new();

    let result = compiler.execute_file("tests/testcase/errors/typecheck_undefined_variable.ob");

    assert!(result.is_err());
    let err = result.unwrap_err();
    let error_msg = format!("{:?}", err);

    // Should contain position information and show proper file:line:column
    assert!(error_msg.contains("typecheck_undefined_variable.ob:2:"));
    assert!(error_msg.contains("Undefined variable: unknown_var"));
}
