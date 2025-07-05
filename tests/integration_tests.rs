use std::fs;
use std::path::Path;
use std::process::Command;

#[test]
fn test_orbit_files() {
    // Get the project root directory
    let project_root = env!("CARGO_MANIFEST_DIR");
    let testcase_dir = Path::new(project_root).join("tests").join("testcase");
    
    // Find all .ob files in the testcase directory
    let mut test_files = Vec::new();
    if let Ok(entries) = fs::read_dir(&testcase_dir) {
        for entry in entries {
            if let Ok(entry) = entry {
                let path = entry.path();
                if path.extension().and_then(|s| s.to_str()) == Some("ob") {
                    test_files.push(path);
                }
            }
        }
    }
    
    assert!(!test_files.is_empty(), "No .ob test files found in tests/testcase directory");
    
    // Build the project to ensure the binary exists
    let build_output = Command::new("cargo")
        .args(&["build", "--bin", "orbit"])
        .current_dir(project_root)
        .output()
        .expect("Failed to execute cargo build");
    
    assert!(build_output.status.success(), 
            "Failed to build orbit binary: {}", 
            String::from_utf8_lossy(&build_output.stderr));
    
    // Run each test file
    for test_file in test_files {
        run_single_test(&test_file, project_root);
    }
}

fn run_single_test(test_file: &Path, project_root: &str) {
    let test_name = test_file.file_stem().unwrap().to_str().unwrap();
    println!("Running test: {}", test_name);
    
    // Get the expected output file
    let expected_file = test_file.with_extension("stdout");
    
    // Check if expected output file exists
    if !expected_file.exists() {
        panic!("Expected output file not found: {}", expected_file.display());
    }
    
    // Read expected output
    let expected_output = fs::read_to_string(&expected_file)
        .unwrap_or_else(|e| panic!("Failed to read expected output file {}: {}", expected_file.display(), e))
        .trim()
        .to_string();
    
    // Run the orbit binary with the test file
    let binary_path = Path::new(project_root).join("target").join("debug").join("orbit");
    let output = Command::new(&binary_path)
        .arg(test_file)
        .output()
        .unwrap_or_else(|e| panic!("Failed to execute orbit binary: {}", e));
    
    // Check if execution was successful
    if !output.status.success() {
        panic!(
            "Test {} failed with exit code {:?}.\nStderr: {}",
            test_name,
            output.status.code(),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    
    // Compare actual output with expected output
    let actual_output = String::from_utf8_lossy(&output.stdout).trim().to_string();
    
    assert_eq!(
        actual_output, 
        expected_output,
        "Test {} failed.\nExpected: '{}'\nActual: '{}'",
        test_name,
        expected_output,
        actual_output
    );
    
    println!("✓ Test {} passed", test_name);
}