use std::fs;
use std::process::Command;
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

#[test]
fn test_benchmark_performance_and_correctness() {
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

    // Use Rust reference implementations as expected values to compare with Orbit results
    let benchmark_files = vec![
        ("heavy_arithmetic.ob", heavy_arithmetic_ref(200000)),
        ("fibonacci_recursive.ob", fibonacci_ref(29)),
        // ("ackermann_function.ob", ackermann_ref(3, 4)),
        ("vector_heavy_ops.ob", vector_heavy_ops_ref(15000)),
        ("vector_push_intensive.ob", vector_push_intensive_ref(15000)),
    ];

    for (filename, expected_result) in benchmark_files {
        let file_path = format!("tests/bench/{}", filename);

        // Check if file exists
        assert!(
            fs::metadata(&file_path).is_ok(),
            "Benchmark file {} not found",
            filename
        );

        // Run benchmark 3 times and measure each execution
        let mut durations = Vec::new();
        let mut actual_result = 0i64;

        for run in 1..=3 {
            let start = Instant::now();
            let output = Command::new("./target/release/orbit")
                .arg(&file_path)
                .output()
                .expect(&format!("Failed to execute {}", filename));
            let duration = start.elapsed();
            durations.push(duration);

            // Check that execution was successful
            assert!(
                output.status.success(),
                "Benchmark {} (run {}) failed: {}",
                filename,
                run,
                String::from_utf8_lossy(&output.stderr)
            );

            // Check execution time is under 5 seconds for this run
            assert!(
                duration < Duration::from_secs(5),
                "Benchmark {} (run {}) took {:.2}s, exceeding 5 second limit",
                filename,
                run,
                duration.as_secs_f64()
            );

            // Parse result (should be same for all runs)
            let result_str = String::from_utf8_lossy(&output.stdout).trim().to_string();
            let run_result: i64 = result_str.parse().expect(&format!(
                "Failed to parse result '{}' for {} (run {})",
                result_str, filename, run
            ));

            if run == 1 {
                actual_result = run_result;
            } else {
                assert_eq!(
                    actual_result, run_result,
                    "Result inconsistency in {} between runs",
                    filename
                );
            }

            println!("  Run {}: {:.2}s", run, duration.as_secs_f64());
        }

        // Verify all runs were under 5 seconds
        let max_duration = durations.iter().max().unwrap();
        assert!(
            *max_duration < Duration::from_secs(5),
            "Benchmark {} exceeded 5 seconds in at least one run (max: {:.2}s)",
            filename,
            max_duration.as_secs_f64()
        );

        // Compare with Rust reference implementation
        assert_eq!(
            actual_result, expected_result,
            "❌ {} MISMATCH: Orbit result {} does not match Rust reference implementation {}",
            filename, actual_result, expected_result
        );
        
        println!("✅ {} MATCH: {}", filename, actual_result);

        let avg_duration = durations.iter().sum::<Duration>().as_secs_f64() / 3.0;
        println!(
            "  ⏱️  {} avg: {:.2}s, max: {:.2}s (all under 5s)",
            filename,
            avg_duration,
            max_duration.as_secs_f64()
        );
    }

    println!("All benchmarks passed performance and correctness tests!");
}
