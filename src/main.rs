use std::env;
use std::process;


#[derive(Debug)]
struct Config {
    filename: String,
    dump_ir: Option<String>,
    print_stacks: bool,
}

fn parse_args() -> Result<Config, String> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return Err(format!("Usage: {} [OPTIONS] <file.ob>", args[0]));
    }

    let mut filename = None;
    let mut dump_ir = None;
    let mut print_stacks = false;
    let mut i = 1;

    while i < args.len() {
        let arg = &args[i];

        if arg.starts_with("--dump-ir=") {
            let ir_file = arg.strip_prefix("--dump-ir=").unwrap();
            if ir_file.is_empty() {
                return Err("--dump-ir option requires a filename".to_string());
            }
            dump_ir = Some(ir_file.to_string());
        } else if arg == "--print-stacks" {
            print_stacks = true;
        } else if arg == "--help" || arg == "-h" {
            return Err("help".to_string());
        } else if arg.starts_with("--") {
            return Err(format!("Unknown option: {}", arg));
        } else if filename.is_none() {
            filename = Some(arg.clone());
        } else {
            return Err("Too many arguments".to_string());
        }

        i += 1;
    }

    match filename {
        Some(f) => Ok(Config {
            filename: f,
            dump_ir,
            print_stacks,
        }),
        None => Err("No input file specified".to_string()),
    }
}

fn main() {
    let config = match parse_args() {
        Ok(config) => config,
        Err(e) => {
            if e == "help" {
                print_help(&env::args().next().unwrap());
                process::exit(0);
            } else {
                eprintln!("Error: {}", e);
                print_help(&env::args().next().unwrap());
                process::exit(1);
            }
        }
    };

    match execute_file_with_options(
        &config.filename,
        config.dump_ir.as_deref(),
        config.print_stacks,
    ) {
        Ok(Some(value)) => println!("{}", value),
        Ok(None) => {}
        Err(e) => {
            eprintln!("Error: {}", e);
            process::exit(1);
        }
    }
}

fn print_help(program_name: &str) {
    println!("Orbit Programming Language Compiler");
    println!();
    println!("USAGE:");
    println!("    {} [OPTIONS] <file.ob>", program_name);
    println!();
    println!("OPTIONS:");
    println!("    --dump-ir=<file>    Dump compiled IR to specified file");
    println!("    --print-stacks      Print stack traces");
    println!("    -h, --help          Print help information");
    println!();
    println!("ARGS:");
    println!("    <file.ob>           Orbit source file to execute");
}

/// Execute a file with optional IR dumping
fn execute_file_with_options(
    filename: &str,
    dump_ir_file: Option<&str>,
    print_stacks: bool,
) -> Result<Option<orbit::Value>, Box<dyn std::error::Error>> {
    if let Some(ir_file) = dump_ir_file {
        orbit::execute_file_with_ir_dump(filename, ir_file).map_err(|e| e.into())
    } else {
        orbit::execute_file_with_options(filename, print_stacks).map_err(|e| e.into())
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
