use std::fs;
use std::process::Command;
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

// Reference implementations in Rust
fn heavy_arithmetic_ref(n: i64) -> i64 {
    let mut sum = 0i64;
    for i in 1..=n {
        sum = sum.wrapping_add(i.wrapping_mul(i));
    }
    sum
}

fn fibonacci_ref(n: i64) -> i64 {
    if n <= 1 {
        n
    } else {
        fibonacci_ref(n - 1).wrapping_add(fibonacci_ref(n - 2))
    }
}

fn ackermann_ref(m: i64, n: i64) -> i64 {
    if m == 0 {
        n + 1
    } else if n == 0 {
        ackermann_ref(m - 1, 1)
    } else {
        ackermann_ref(m - 1, ackermann_ref(m, n - 1))
    }
}

// Wrapper for ackermann with fixed m=3
fn ackermann_3_ref(n: i64) -> i64 {
    ackermann_ref(3, n)
}

fn vector_heavy_ops_ref(size: i64) -> i64 {
    let mut vec: Vec<i64> = Vec::new();

    // Fill vector with values
    for i in 0..size {
        vec.push(i);
    }

    // Modify all elements
    for i in 0..size as usize {
        vec[i] = vec[i].wrapping_mul(2);
    }

    // Sum all elements
    let mut sum = 0i64;
    for i in 0..size as usize {
        sum = sum.wrapping_add(vec[i]);
    }

    sum
}

fn vector_push_intensive_ref(count: i64) -> i64 {
    let mut vec1: Vec<i64> = Vec::new();
    let mut vec2: Vec<i64> = Vec::new();

    // Push many elements to both vectors
    for i in 0..count {
        vec1.push(i);
        vec2.push(i.wrapping_mul(2));
    }

    // Cross-reference and sum
    let mut sum = 0i64;
    for i in 0..count as usize {
        sum = sum.wrapping_add(vec1[i]).wrapping_add(vec2[i]);
    }

    sum
}

// Benchmark configuration
struct BenchmarkConfig {
    name: &'static str,
    orbit_file: &'static str,
    rust_func: fn(i64) -> i64,
    test_sizes: Vec<i64>,
}

struct BenchmarkResult {
    program: String,
    test_size: i64,
    orbit_time_ns: Option<u64>,
    rust_time_ns: u64,
    orbit_result: Option<i64>,
    rust_result: i64,
    is_error: bool,
    error_message: Option<String>,
}

fn run_rust_with_timeout<F>(func: F, timeout_secs: u64) -> Result<(i64, Duration), String>
where
    F: FnOnce() -> i64 + Send + 'static,
{
    let (tx, rx) = mpsc::channel();
    let start = Instant::now();

    thread::spawn(move || {
        let result = func();
        let _ = tx.send(result);
    });

    match rx.recv_timeout(Duration::from_secs(timeout_secs)) {
        Ok(result) => {
            let duration = start.elapsed();
            Ok((result, duration))
        }
        Err(_) => Err("Timeout: Rust execution exceeded timeout".to_string()),
    }
}

fn get_timestamp_and_commit() -> (String, String) {
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs();

    let commit_hash = Command::new("git")
        .args(&["rev-parse", "HEAD"])
        .output()
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_string())
        .unwrap_or_else(|_| "unknown".to_string());

    (format!("{}", timestamp), commit_hash)
}

fn print_benchmark_report(results: &[BenchmarkResult], timestamp: &str, commit_hash: &str) {
    // Header
    println!("# Orbit Benchmark Report\n");
    println!("**Timestamp**: {}", timestamp);
    println!("**Commit Hash**: {}", commit_hash);
    println!("**Total Tests**: {}", results.len());

    let errors = results.iter().filter(|r| r.is_error).count();
    println!("**Errors**: {}\n", errors);

    // Summary table
    println!("## Summary\n");
    println!("| Program | Test Size | Orbit Time (ms) | Rust Time (ms) | Speedup | Status | Orbit Result | Rust Result |");
    println!("|---------|-----------|-----------------|----------------|---------|--------|--------------|-------------|");

    for result in results {
        let orbit_time_ms = result.orbit_time_ns.map(|ns| ns as f64 / 1_000_000.0);
        let rust_time_ms = result.rust_time_ns as f64 / 1_000_000.0;

        let orbit_time_str = match orbit_time_ms {
            Some(time) => format!("{:.3}", time),
            None => "N/A".to_string(),
        };

        let speedup = match orbit_time_ms {
            Some(orbit_time) if orbit_time > 0.0 => {
                let ratio = rust_time_ms / orbit_time;
                if ratio > 1.0 {
                    format!("{:.2}x faster", ratio)
                } else {
                    format!("{:.2}x slower", 1.0 / ratio)
                }
            }
            _ => "N/A".to_string(),
        };

        let status = if result.is_error {
            "❌ ERROR"
        } else {
            "✅ OK"
        };

        let orbit_result_str = match result.orbit_result {
            Some(val) => val.to_string(),
            None => "N/A".to_string(),
        };

        println!(
            "| {} | {} | {} | {:.3} | {} | {} | {} | {} |",
            result.program,
            result.test_size,
            orbit_time_str,
            rust_time_ms,
            speedup,
            status,
            orbit_result_str,
            result.rust_result
        );
    }

    // Error details section
    let error_results: Vec<_> = results.iter().filter(|r| r.is_error).collect();
    if !error_results.is_empty() {
        println!("\n## Error Details\n");
        for result in error_results {
            println!("### {} (size: {})", result.program, result.test_size);
            if let Some(ref error_msg) = result.error_message {
                println!("**Error**: {}\n", error_msg);
            }
        }
    }

}

fn create_temp_orbit_file(template_path: &str, bench_size: i64) -> Result<String, String> {
    let template_content = std::fs::read_to_string(template_path)
        .map_err(|e| format!("Failed to read template file {}: {}", template_path, e))?;

    let content = template_content.replace("{{BENCH_SIZE}}", &bench_size.to_string());

    let temp_filename = format!(
        "./tmp/bench_{}_{}.ob",
        std::path::Path::new(template_path)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown"),
        bench_size
    );

    std::fs::create_dir_all("./tmp")
        .map_err(|e| format!("Failed to create tmp directory: {}", e))?;
    std::fs::write(&temp_filename, content)
        .map_err(|e| format!("Failed to write temp file {}: {}", temp_filename, e))?;

    Ok(temp_filename)
}

fn run_orbit_with_timeout(file_path: &str, timeout_secs: u64) -> Result<(i64, Duration), String> {
    let start = Instant::now();

    let mut child = Command::new("./target/release/orbit")
        .arg(file_path)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to spawn orbit process: {}", e))?;

    // Simple timeout implementation
    let timeout_duration = Duration::from_secs(timeout_secs);
    let mut elapsed = Duration::new(0, 0);

    while elapsed < timeout_duration {
        match child.try_wait() {
            Ok(Some(status)) => {
                let duration = start.elapsed();
                if !status.success() {
                    return Err("Orbit execution failed".to_string());
                }

                let output = Command::new("./target/release/orbit")
                    .arg(file_path)
                    .stdout(std::process::Stdio::piped())
                    .stderr(std::process::Stdio::piped())
                    .output()
                    .map_err(|e| format!("Failed to get output: {}", e))?;

                let result_str = String::from_utf8_lossy(&output.stdout).trim().to_string();
                let result = result_str
                    .parse::<i64>()
                    .map_err(|e| format!("Failed to parse result '{}': {}", result_str, e))?;

                return Ok((result, duration));
            }
            Ok(None) => {
                std::thread::sleep(Duration::from_millis(100));
                elapsed = start.elapsed();
            }
            Err(e) => return Err(format!("Error checking process status: {}", e)),
        }
    }

    // Timeout reached
    let _ = child.kill();
    Err("Timeout: execution exceeded timeout".to_string())
}

// Main benchmark function that runs Orbit vs Rust comparisons
fn main() {
    // Build release version first
    let build_output = Command::new("cargo")
        .args(&["build", "--release"])
        .output()
        .expect("Failed to build release version");

    assert!(
        build_output.status.success(),
        "Release build failed: {}",
        String::from_utf8_lossy(&build_output.stderr)
    );

    let benchmark_configs = vec![
        BenchmarkConfig {
            name: "heavy_arithmetic",
            orbit_file: "benches/programs/heavy_arithmetic.ob",
            rust_func: heavy_arithmetic_ref,
            test_sizes: vec![
                1000, 5000, 10000, 50000, 100000, 500000, 1000000, 2000000, 5000000, 10000000,
            ],
        },
        BenchmarkConfig {
            name: "fibonacci_recursive",
            orbit_file: "benches/programs/fibonacci_recursive.ob",
            rust_func: fibonacci_ref,
            test_sizes: vec![20, 25, 28, 30, 32, 35, 37, 39, 41],
        },
        BenchmarkConfig {
            name: "ackermann_function",
            orbit_file: "benches/programs/ackermann_function.ob",
            rust_func: ackermann_3_ref,
            test_sizes: vec![2, 4, 6, 8, 9, 10, 11, 12, 13, 14],
        },
        BenchmarkConfig {
            name: "vector_heavy_ops",
            orbit_file: "benches/programs/vector_heavy_ops.ob",
            rust_func: vector_heavy_ops_ref,
            test_sizes: vec![1000, 5000, 10000, 25000, 50000, 100000],
        },
        BenchmarkConfig {
            name: "vector_push_intensive",
            orbit_file: "benches/programs/vector_push_intensive.ob",
            rust_func: vector_push_intensive_ref,
            test_sizes: vec![
                1000, 5000, 10000, 25000, 50000, 100000, 250000, 500000, 750000, 1000000,
            ],
        },
    ];

    let (timestamp, commit_hash) = get_timestamp_and_commit();
    let mut all_results = Vec::new();

    for config in benchmark_configs {
        let file_path = config.orbit_file;

        // Check if file exists
        assert!(
            fs::metadata(&file_path).is_ok(),
            "Benchmark file {} not found",
            config.orbit_file
        );

        eprint!("[{}] ", config.name);

        // Set 10-second timeout for entire benchmark
        let benchmark_start = Instant::now();
        let benchmark_timeout = Duration::from_secs(10);

        for &test_size in &config.test_sizes {
            // Check if we've exceeded the benchmark timeout
            if benchmark_start.elapsed() >= benchmark_timeout {
                eprint!("T"); // Indicate timeout
                break;
            }

            // Calculate remaining time for this test case
            let remaining_time = benchmark_timeout.saturating_sub(benchmark_start.elapsed());
            let test_timeout_secs = std::cmp::min(remaining_time.as_secs(), 5); // Max 5 seconds per test

            if test_timeout_secs == 0 {
                eprint!("T"); // Indicate timeout
                break;
            }

            // Measure Rust reference implementation with remaining timeout
            let rust_func = config.rust_func;
            let rust_result_with_timeout =
                run_rust_with_timeout(move || rust_func(test_size), test_timeout_secs);

            let benchmark_result = match rust_result_with_timeout {
                Ok((rust_result, rust_duration)) => {
                    // Check if we still have time for Orbit execution
                    let remaining_time_after_rust =
                        benchmark_timeout.saturating_sub(benchmark_start.elapsed());
                    let orbit_timeout_secs = std::cmp::min(remaining_time_after_rust.as_secs(), 5);

                    if orbit_timeout_secs == 0 {
                        // No time left for Orbit execution
                        BenchmarkResult {
                            program: config.name.to_string(),
                            test_size,
                            orbit_time_ns: None,
                            rust_time_ns: rust_duration.as_nanos() as u64,
                            orbit_result: None,
                            rust_result,
                            is_error: true,
                            error_message: Some(
                                "Benchmark timeout: no time left for Orbit execution".to_string(),
                            ),
                        }
                    } else {
                        // Create temporary orbit file with the specific test size
                        let temp_file_result = create_temp_orbit_file(&file_path, test_size);
                        let orbit_result = match temp_file_result {
                            Ok(temp_file_path) => {
                                let result =
                                    run_orbit_with_timeout(&temp_file_path, orbit_timeout_secs);
                                // Clean up temp file
                                let _ = std::fs::remove_file(&temp_file_path);
                                result
                            }
                            Err(e) => Err(format!("Failed to create temp file: {}", e)),
                        };

                        match orbit_result {
                            Ok((orbit_result_val, orbit_duration)) => {
                                let is_error = orbit_result_val != rust_result;
                                let error_message = if is_error {
                                    Some(format!(
                                        "Result mismatch: Orbit={}, Rust={}",
                                        orbit_result_val, rust_result
                                    ))
                                } else {
                                    None
                                };

                                BenchmarkResult {
                                    program: config.name.to_string(),
                                    test_size,
                                    orbit_time_ns: Some(orbit_duration.as_nanos() as u64),
                                    rust_time_ns: rust_duration.as_nanos() as u64,
                                    orbit_result: Some(orbit_result_val),
                                    rust_result,
                                    is_error,
                                    error_message,
                                }
                            }
                            Err(error_msg) => BenchmarkResult {
                                program: config.name.to_string(),
                                test_size,
                                orbit_time_ns: None,
                                rust_time_ns: rust_duration.as_nanos() as u64,
                                orbit_result: None,
                                rust_result,
                                is_error: true,
                                error_message: Some(error_msg),
                            },
                        }
                    }
                }
                Err(rust_timeout_error) => {
                    // Rust execution timed out
                    BenchmarkResult {
                        program: config.name.to_string(),
                        test_size,
                        orbit_time_ns: None,
                        rust_time_ns: (test_timeout_secs * 1_000_000_000) as u64, // timeout duration in nanoseconds
                        orbit_result: None,
                        rust_result: 0, // Placeholder since we don't have the result
                        is_error: true,
                        error_message: Some(format!("Rust timeout: {}", rust_timeout_error)),
                    }
                }
            };

            eprint!("{}", if benchmark_result.is_error { "x" } else { "." });

            // Suppressed progress indication for cleaner output
            all_results.push(benchmark_result);
        }

        eprintln!("");
    }

    // Print benchmark report to stdout
    print_benchmark_report(&all_results, &timestamp, &commit_hash);

    // Exit with error code if there are any errors
    let errors = all_results.iter().filter(|r| r.is_error).count();
    if errors > 0 {
        std::process::exit(1);
    }
}
