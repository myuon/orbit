use std::fs;
use std::process::Command;

#[test]
fn test_benchmark_files_exist() {
    // Simple test to verify benchmark files exist
    let benchmark_files = vec![
        "benches/programs/heavy_arithmetic.ob",
        "benches/programs/fibonacci_recursive.ob",
        "benches/programs/ackermann_function.ob",
        "benches/programs/vector_heavy_ops.ob",
        "benches/programs/vector_push_intensive.ob",
    ];

    for file_path in benchmark_files {
        assert!(
            fs::metadata(file_path).is_ok(),
            "Benchmark file {} not found",
            file_path
        );
    }
}

#[test]
fn test_orbit_build() {
    // Test that orbit can be built
    let build_output = Command::new("cargo")
        .args(&["build", "--release"])
        .output()
        .expect("Failed to build release version");

    assert!(
        build_output.status.success(),
        "Release build failed: {}",
        String::from_utf8_lossy(&build_output.stderr)
    );
}
