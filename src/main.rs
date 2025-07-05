use std::env;
use std::process;

use orbit::execute_file;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <file.ob>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    match execute_file(filename) {
        Ok(Some(value)) => println!("{}", value),
        Ok(None) => {}
        Err(e) => {
            eprintln!("Error: {}", e);
            process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use orbit::execute_code;
    use std::fs;
    use std::path::Path;

    #[test]
    fn test_file_execution() {
        // Test that we can execute .ob files correctly
        let test_cases = vec![("tests/testcase/program/simple_function.ob", "5")];

        for (file_path, expected) in test_cases {
            if Path::new(file_path).exists() {
                let content = fs::read_to_string(file_path).unwrap();
                let result = execute_code(&content).unwrap();
                match result {
                    Some(value) => assert_eq!(
                        value.to_string(),
                        expected,
                        "Failed for file: {}",
                        file_path
                    ),
                    None => assert_eq!("", expected, "Failed for file: {}", file_path),
                }
            }
        }
    }
}
