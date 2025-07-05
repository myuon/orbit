mod ast;
mod lexer;
mod parser;
mod runtime;

use std::env;
use std::fs;
use std::io::{self, Write};

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

fn execute_code(code: &str) -> Result<(), String> {
    let runtime = Runtime::new();
    let result = execute_expression(code, &runtime)?;
    println!("{}", result);
    Ok(())
}

fn execute_expression(input: &str, runtime: &Runtime) -> Result<runtime::Value, String> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    
    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;
    
    runtime.evaluate(&ast)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_execute_simple_expression() {
        let runtime = Runtime::new();
        let result = execute_expression("2 + 3", &runtime).unwrap();
        assert_eq!(result, runtime::Value::Number(5.0));
    }

    #[test]
    fn test_execute_multiplication() {
        let runtime = Runtime::new();
        let result = execute_expression("4 * 5", &runtime).unwrap();
        assert_eq!(result, runtime::Value::Number(20.0));
    }

    #[test]
    fn test_execute_precedence() {
        let runtime = Runtime::new();
        let result = execute_expression("2 + 3 * 4", &runtime).unwrap();
        assert_eq!(result, runtime::Value::Number(14.0));
    }

    #[test]
    fn test_execute_parentheses() {
        let runtime = Runtime::new();
        let result = execute_expression("(2 + 3) * 4", &runtime).unwrap();
        assert_eq!(result, runtime::Value::Number(20.0));
    }

    #[test]
    fn test_execute_complex_expression() {
        let runtime = Runtime::new();
        let result = execute_expression("2 * 3 + 4 / 2", &runtime).unwrap();
        assert_eq!(result, runtime::Value::Number(8.0));
    }

    #[test]
    fn test_execute_nested_parentheses() {
        let runtime = Runtime::new();
        let result = execute_expression("((2 + 3) * 4) / 2", &runtime).unwrap();
        assert_eq!(result, runtime::Value::Number(10.0));
    }

    #[test]
    fn test_execute_float_numbers() {
        let runtime = Runtime::new();
        let result = execute_expression("3.14 * 2", &runtime).unwrap();
        assert_eq!(result, runtime::Value::Number(6.28));
    }

    #[test]
    fn test_execute_division_by_zero() {
        let runtime = Runtime::new();
        let result = execute_expression("5 / 0", &runtime);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "Division by zero");
    }

    #[test]
    fn test_execute_boolean_true() {
        let runtime = Runtime::new();
        let result = execute_expression("true", &runtime).unwrap();
        assert_eq!(result, runtime::Value::Boolean(true));
    }

    #[test]
    fn test_execute_boolean_false() {
        let runtime = Runtime::new();
        let result = execute_expression("false", &runtime).unwrap();
        assert_eq!(result, runtime::Value::Boolean(false));
    }

    #[test]
    fn test_execute_string_literal() {
        let runtime = Runtime::new();
        let result = execute_expression("\"Hello, World!\"", &runtime).unwrap();
        assert_eq!(result, runtime::Value::String("Hello, World!".to_string()));
    }

    #[test]
    fn test_execute_string_concatenation() {
        let runtime = Runtime::new();
        let result = execute_expression("\"Hello, \" + \"World!\"", &runtime).unwrap();
        assert_eq!(result, runtime::Value::String("Hello, World!".to_string()));
    }

    #[test]
    fn test_execute_string_with_escapes() {
        let runtime = Runtime::new();
        let result = execute_expression("\"Hello\\nWorld\"", &runtime).unwrap();
        assert_eq!(result, runtime::Value::String("Hello\nWorld".to_string()));
    }

    #[test]
    fn test_execute_empty_string() {
        let runtime = Runtime::new();
        let result = execute_expression("\"\"", &runtime).unwrap();
        assert_eq!(result, runtime::Value::String("".to_string()));
    }

    #[test]
    fn test_execute_string_quotes() {
        let runtime = Runtime::new();
        let result = execute_expression("\"He said \\\"Hello\\\"\"", &runtime).unwrap();
        assert_eq!(result, runtime::Value::String("He said \"Hello\"".to_string()));
    }

    #[test]
    fn test_execute_type_mismatch_error() {
        let runtime = Runtime::new();
        let result = execute_expression("\"hello\" * 2", &runtime);
        assert!(result.is_err());
        let error_msg = result.unwrap_err();
        assert!(error_msg.contains("Type mismatch"));
    }

    #[test]
    fn test_execute_number_string_mismatch() {
        let runtime = Runtime::new();
        let result = execute_expression("5 + \"hello\"", &runtime);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Type mismatch"));
    }
}
