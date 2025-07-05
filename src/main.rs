mod ast;
mod lexer;
mod parser;
mod runtime;

use std::env;
use std::fs;
use std::io::{self, Write};

use anyhow::Result;
use lexer::Lexer;
use parser::Parser;
use runtime::Runtime;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() > 1 {
        // File execution mode
        let filename = &args[1];
        match fs::read_to_string(filename) {
            Ok(content) => {
                if let Err(e) = execute_code(&content) {
                    eprintln!("Error: {}", e);
                }
            }
            Err(e) => {
                eprintln!("Error reading file {}: {}", filename, e);
            }
        }
    } else {
        // REPL mode
        println!("Orbit Calculator REPL");
        println!("Enter arithmetic expressions (e.g., 2 + 3 * 4)");
        println!("Type 'exit' to quit");
        
        let mut input = String::new();
        let runtime = Runtime::new();
        
        loop {
            print!("> ");
            io::stdout().flush().unwrap();
            
            input.clear();
            if io::stdin().read_line(&mut input).is_err() {
                break;
            }
            
            let input = input.trim();
            if input.is_empty() {
                continue;
            }
            
            if input == "exit" {
                break;
            }
            
            match execute_expression(input, &runtime) {
                Ok(value) => println!("{}", value),
                Err(e) => println!("Error: {}", e),
            }
        }
    }
}

fn execute_code(code: &str) -> Result<()> {
    let runtime = Runtime::new();
    let result = execute_expression(code, &runtime)?;
    println!("{}", result);
    Ok(())
}

fn execute_expression(input: &str, runtime: &Runtime) -> Result<runtime::Value> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize()?;
    
    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;
    
    runtime.evaluate(&ast)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arithmetic_expressions() {
        let test_cases = vec![
            ("2 + 3", runtime::Value::Number(5.0)),
            ("4 * 5", runtime::Value::Number(20.0)),
            ("2 + 3 * 4", runtime::Value::Number(14.0)),
            ("(2 + 3) * 4", runtime::Value::Number(20.0)),
            ("2 * 3 + 4 / 2", runtime::Value::Number(8.0)),
            ("((2 + 3) * 4) / 2", runtime::Value::Number(10.0)),
            ("3.14 * 2", runtime::Value::Number(6.28)),
        ];

        let runtime = Runtime::new();
        for (input, expected) in test_cases {
            let result = execute_expression(input, &runtime).unwrap();
            assert_eq!(result, expected, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_boolean_literals() {
        let test_cases = vec![
            ("true", runtime::Value::Boolean(true)),
            ("false", runtime::Value::Boolean(false)),
        ];

        let runtime = Runtime::new();
        for (input, expected) in test_cases {
            let result = execute_expression(input, &runtime).unwrap();
            assert_eq!(result, expected, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_string_expressions() {
        let test_cases = vec![
            ("\"Hello, World!\"", runtime::Value::String("Hello, World!".to_string())),
            ("\"Hello, \" + \"World!\"", runtime::Value::String("Hello, World!".to_string())),
            ("\"Hello\\nWorld\"", runtime::Value::String("Hello\nWorld".to_string())),
            ("\"\"", runtime::Value::String("".to_string())),
            ("\"He said \\\"Hello\\\"\"", runtime::Value::String("He said \"Hello\"".to_string())),
        ];

        let runtime = Runtime::new();
        for (input, expected) in test_cases {
            let result = execute_expression(input, &runtime).unwrap();
            assert_eq!(result, expected, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_error_cases() {
        let error_test_cases = vec![
            ("5 / 0", "Division by zero"),
            ("\"hello\" * 2", "Type mismatch"),
            ("5 + \"hello\"", "Type mismatch"),
        ];

        let runtime = Runtime::new();
        for (input, expected_error) in error_test_cases {
            let result = execute_expression(input, &runtime);
            assert!(result.is_err(), "Expected error for input: {}", input);
            let error_msg = result.unwrap_err().to_string();
            assert!(error_msg.contains(expected_error), 
                "Expected error containing '{}', got '{}' for input: {}", 
                expected_error, error_msg, input);
        }
    }
}
