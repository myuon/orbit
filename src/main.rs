use std::env;
use std::process;
use std::time::Duration;

#[derive(Debug)]
struct Config {
    filename: String,
    dump_ir: Option<String>,
    print_stacks: bool,
    print_stacks_on_call: Option<String>,
    profile: bool,
    profile_output: Option<String>,
    timeout: Option<u64>, // timeout in seconds
}

fn parse_args() -> Result<Config, String> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return Err(format!("Usage: {} [OPTIONS] <file.ob>", args[0]));
    }

    let mut filename = None;
    let mut dump_ir = None;
    let mut print_stacks = false;
    let mut print_stacks_on_call = None;
    let mut profile = false;
    let mut profile_output = None;
    let mut timeout = None;
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
        } else if arg.starts_with("--print-stacks-on-call=") {
            let function_name = arg.strip_prefix("--print-stacks-on-call=").unwrap();
            if function_name.is_empty() {
                return Err("--print-stacks-on-call option requires a function name".to_string());
            }
            print_stacks_on_call = Some(function_name.to_string());
        } else if arg == "--profile" {
            profile = true;
        } else if arg.starts_with("--profile-output=") {
            let profile_file = arg.strip_prefix("--profile-output=").unwrap();
            if profile_file.is_empty() {
                return Err("--profile-output option requires a filename".to_string());
            }
            profile = true;
            profile_output = Some(profile_file.to_string());
        } else if arg.starts_with("--timeout=") {
            let timeout_str = arg.strip_prefix("--timeout=").unwrap();
            if timeout_str.is_empty() {
                return Err("--timeout option requires a value".to_string());
            }
            // Parse timeout value (support formats like "10s", "5m", "30")
            let timeout_secs = parse_timeout(timeout_str)?;
            timeout = Some(timeout_secs);
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
            print_stacks_on_call,
            profile,
            profile_output,
            timeout,
        }),
        None => Err("No input file specified".to_string()),
    }
}

/// Parse timeout string like "10s", "5m", "30" (defaults to seconds)
fn parse_timeout(timeout_str: &str) -> Result<u64, String> {
    if timeout_str.is_empty() {
        return Err("Empty timeout value".to_string());
    }
    
    if timeout_str.ends_with('s') {
        let num_str = &timeout_str[..timeout_str.len() - 1];
        num_str.parse::<u64>()
            .map_err(|_| format!("Invalid timeout value: {}", timeout_str))
    } else if timeout_str.ends_with('m') {
        let num_str = &timeout_str[..timeout_str.len() - 1];
        num_str.parse::<u64>()
            .map(|m| m * 60)
            .map_err(|_| format!("Invalid timeout value: {}", timeout_str))
    } else {
        // Default to seconds if no unit specified
        timeout_str.parse::<u64>()
            .map_err(|_| format!("Invalid timeout value: {}", timeout_str))
    }
}

#[tokio::main]
async fn main() {
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

    let result = if let Some(timeout_secs) = config.timeout {
        // Execute with timeout
        let timeout_duration = Duration::from_secs(timeout_secs);
        let execution_future = execute_with_config(&config);
        
        match tokio::time::timeout(timeout_duration, execution_future).await {
            Ok(result) => result,
            Err(_) => {
                eprintln!("Error: Execution timed out after {} seconds", timeout_secs);
                process::exit(1);
            }
        }
    } else {
        // Execute without timeout
        execute_with_config(&config).await
    };

    match result {
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
    println!("    --dump-ir=<file>         Dump compiled IR to specified file");
    println!("    --print-stacks           Print stack traces during execution");
    println!("    --print-stacks-on-call=<func>  Print stack traces only when calling specific function");
    println!("    --profile                Enable profiling and print results");
    println!("    --profile-output=<file>  Enable profiling and save results to file");
    println!("    --timeout=<time>         Set execution timeout (e.g., 10s, 5m, 30)");
    println!("    -h, --help               Print help information");
    println!();
    println!("ARGS:");
    println!("    <file.ob>           Orbit source file to execute");
}

/// Execute with configuration in an async context
async fn execute_with_config(config: &Config) -> Result<Option<orbit::Value>, String> {
    // Execute in a blocking task since orbit execution is synchronous
    let filename = config.filename.clone();
    let dump_ir_file = config.dump_ir.clone();
    let print_stacks = config.print_stacks;
    let print_stacks_on_call = config.print_stacks_on_call.clone();
    let profile = config.profile;
    let profile_output = config.profile_output.clone();
    
    tokio::task::spawn_blocking(move || {
        if profile {
            // Use profiling execution
            orbit::execute_file_with_profiling(&filename, profile_output.as_deref())
                .map_err(|e| e.to_string())
        } else {
            // Use regular execution
            execute_file_with_options(
                &filename,
                dump_ir_file.as_deref(),
                print_stacks,
                print_stacks_on_call.as_deref(),
            ).map_err(|e| e.to_string())
        }
    }).await.unwrap()
}

/// Execute a file with optional IR dumping
fn execute_file_with_options(
    filename: &str,
    dump_ir_file: Option<&str>,
    print_stacks: bool,
    print_stacks_on_call: Option<&str>,
) -> Result<Option<orbit::Value>, Box<dyn std::error::Error>> {
    if let Some(ir_file) = dump_ir_file {
        // IRダンプとスタックトレースの両方を有効にする
        orbit::execute_file_with_ir_dump_and_options_on_call(filename, ir_file, print_stacks, print_stacks_on_call)
            .map_err(|e| e.into())
    } else {
        orbit::execute_file_with_options_on_call(filename, print_stacks, print_stacks_on_call).map_err(|e| e.into())
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
