use clap::Parser;
use std::process;
use std::time::Duration;

#[derive(Parser, Debug)]
#[command(name = "orbit")]
#[command(version, about = "Orbit Programming Language Compiler", long_about = None)]
struct Config {
    /// Orbit source file to execute
    filename: String,

    /// Dump compiled IR to specified file
    #[arg(long, value_name = "FILE")]
    dump_ir: Option<String>,

    /// Print stack traces during execution
    #[arg(long)]
    print_stacks: bool,

    /// Print stack traces only when calling specific function
    #[arg(long, value_name = "FUNCTION")]
    print_stacks_on_call: Option<String>,

    /// Enable profiling and print results
    #[arg(long)]
    profile: bool,

    /// Enable profiling and save results to file
    #[arg(long, value_name = "FILE")]
    profile_output: Option<String>,

    /// Set execution timeout (e.g., 10s, 5m, 30)
    #[arg(long, value_name = "TIME", value_parser = parse_timeout)]
    timeout: Option<u64>,
}

impl Config {
    /// Convert Config to CompilerOptions
    fn to_compiler_options(&self) -> orbit::CompilerOptions {
        orbit::CompilerOptions {
            ir_dump_file: self.dump_ir.clone(),
            print_stacks: self.print_stacks,
            print_stacks_on_call: self.print_stacks_on_call.clone(),
            enable_profiling: self.profile || self.profile_output.is_some(),
            profile_output: self.profile_output.clone(),
        }
    }
}

/// Parse timeout string like "10s", "5m", "30" (defaults to seconds)
fn parse_timeout(timeout_str: &str) -> Result<u64, String> {
    if timeout_str.is_empty() {
        return Err("Empty timeout value".to_string());
    }

    if timeout_str.ends_with('s') {
        let num_str = &timeout_str[..timeout_str.len() - 1];
        num_str
            .parse::<u64>()
            .map_err(|_| format!("Invalid timeout value: {}", timeout_str))
    } else if timeout_str.ends_with('m') {
        let num_str = &timeout_str[..timeout_str.len() - 1];
        num_str
            .parse::<u64>()
            .map(|m| m * 60)
            .map_err(|_| format!("Invalid timeout value: {}", timeout_str))
    } else {
        // Default to seconds if no unit specified
        timeout_str
            .parse::<u64>()
            .map_err(|_| format!("Invalid timeout value: {}", timeout_str))
    }
}

#[tokio::main]
async fn main() {
    let config = Config::parse();

    let result = execute_with_optional_timeout(
        async {
            // Execute in a blocking task since orbit execution is synchronous
            let filename = config.filename.clone();
            let options = config.to_compiler_options();

            tokio::task::spawn_blocking(move || {
                let mut compiler = orbit::Compiler::new_with_options(options);
                compiler.execute_file(&filename).map_err(|e| e.to_string())
            })
            .await
            .unwrap()
        },
        config.timeout,
    )
    .await;

    match result {
        Ok(Some(value)) => println!("{}", value),
        Ok(None) => {}
        Err(e) => {
            eprintln!("Error: {}", e);
            process::exit(1);
        }
    }
}

/// Execute a future with optional timeout
async fn execute_with_optional_timeout<T, F>(
    future: F,
    timeout_secs: Option<u64>,
) -> Result<T, String>
where
    F: std::future::Future<Output = Result<T, String>>,
{
    if let Some(timeout_secs) = timeout_secs {
        let timeout_duration = Duration::from_secs(timeout_secs);
        match tokio::time::timeout(timeout_duration, future).await {
            Ok(result) => result,
            Err(_) => {
                eprintln!("Error: Execution timed out after {} seconds", timeout_secs);
                process::exit(1);
            }
        }
    } else {
        future.await
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
