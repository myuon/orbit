use clap::Parser;
use orbit::utils::{execute_with_optional_timeout, parse_timeout};
use std::process;

#[derive(Parser, Debug)]
#[command(name = "orbit")]
#[command(version, about = "Orbit Programming Language Compiler", long_about = None)]
struct Config {
    /// Orbit source file to execute
    filename: String,

    /// Enable IR dumping
    #[arg(long)]
    dump_ir: bool,

    /// Dump compiled IR to specified file
    #[arg(long, value_name = "FILE")]
    dump_ir_output: Option<String>,

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
            dump_ir: self.dump_ir || self.dump_ir_output.is_some(),
            dump_ir_output: self.dump_ir_output.clone(),
            print_stacks: self.print_stacks,
            print_stacks_on_call: self.print_stacks_on_call.clone(),
            enable_profiling: self.profile || self.profile_output.is_some(),
            profile_output: self.profile_output.clone(),
        }
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
