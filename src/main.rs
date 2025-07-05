use std::env;
use std::io::{self, Write};

use orbit::{execute_file, execute_statement, runtime::Runtime};

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() > 1 {
        // File execution mode
        let filename = &args[1];
        match execute_file(filename) {
            Ok(Some(value)) => println!("{}", value),
            Ok(None) => {},
            Err(e) => eprintln!("Error: {}", e),
        }
    } else {
        // REPL mode
        println!("Orbit Calculator REPL");
        println!("Enter arithmetic expressions (e.g., 2 + 3 * 4)");
        println!("Type 'exit' to quit");
        
        let mut input = String::new();
        let mut runtime = Runtime::new();
        
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
            
            match execute_statement(input, &mut runtime) {
                Ok(Some(value)) => println!("{}", value),
                Ok(None) => {},
                Err(e) => eprintln!("Error: {}", e),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use orbit::{execute_expression, runtime::Value};

    #[test]
    fn test_arithmetic_expressions() {
        let test_cases = vec![
            ("2 + 3", Value::Number(5.0)),
            ("4 * 5", Value::Number(20.0)),
            ("2 + 3 * 4", Value::Number(14.0)),
            ("(2 + 3) * 4", Value::Number(20.0)),
            ("3.14 * 2", Value::Number(6.28)),
        ];

        let mut runtime = Runtime::new();
        for (input, expected) in test_cases {
            let result = execute_expression(input, &mut runtime).unwrap();
            assert_eq!(result, expected, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_boolean_literals() {
        let test_cases = vec![
            ("true", Value::Boolean(true)),
            ("false", Value::Boolean(false)),
        ];

        let mut runtime = Runtime::new();
        for (input, expected) in test_cases {
            let result = execute_expression(input, &mut runtime).unwrap();
            assert_eq!(result, expected, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_string_expressions() {
        let test_cases = vec![
            ("\"hello\"", Value::String("hello".to_string())),
            ("\"world\"", Value::String("world".to_string())),
            ("\"hello\" + \" world\"", Value::String("hello world".to_string())),
            ("\"number: \" + \"42\"", Value::String("number: 42".to_string())),
            ("\"escape test: \\\"quoted\\\"\"", Value::String("escape test: \"quoted\"".to_string())),
            ("\"He said \\\"Hello\\\"\"", Value::String("He said \"Hello\"".to_string())),
        ];

        let mut runtime = Runtime::new();
        for (input, expected) in test_cases {
            let result = execute_expression(input, &mut runtime).unwrap();
            assert_eq!(result, expected, "Failed for input: {}", input);
        }
    }

    #[test]
    fn test_let_statements() {
        let mut runtime = Runtime::new();
        
        // Test let statement
        execute_statement("let x = 10;", &mut runtime).unwrap();
        let result = execute_expression("x", &mut runtime).unwrap();
        assert_eq!(result, Value::Number(10.0));
        
        // Test let with expression
        execute_statement("let y = 2 + 3;", &mut runtime).unwrap();
        let result = execute_expression("y", &mut runtime).unwrap();
        assert_eq!(result, Value::Number(5.0));
    }

    #[test]
    fn test_variable_usage() {
        let mut runtime = Runtime::new();
        
        execute_statement("let a = 5;", &mut runtime).unwrap();
        execute_statement("let b = 10;", &mut runtime).unwrap();
        
        let result = execute_expression("a + b", &mut runtime).unwrap();
        assert_eq!(result, Value::Number(15.0));
        
        let result = execute_expression("a * b", &mut runtime).unwrap();
        assert_eq!(result, Value::Number(50.0));
    }

    #[test]
    fn test_string_variables() {
        let mut runtime = Runtime::new();
        
        execute_statement("let greeting = \"Hello\";", &mut runtime).unwrap();
        execute_statement("let name = \"World\";", &mut runtime).unwrap();
        
        let result = execute_expression("greeting + \", \" + name + \"!\"", &mut runtime).unwrap();
        assert_eq!(result, Value::String("Hello, World!".to_string()));
    }

    #[test]
    fn test_error_cases() {
        let mut runtime = Runtime::new();
        
        // Test division by zero
        let result = execute_expression("5 / 0", &mut runtime);
        assert!(result.is_err());
        
        // Test undefined variable
        let result = execute_expression("undefined_var", &mut runtime);
        assert!(result.is_err());
    }
}