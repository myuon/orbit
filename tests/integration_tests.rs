use std::fs;
use std::path::Path;

use orbit::execute_code_with_output;

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
    
    // Run each test file
    for test_file in test_files {
        run_single_test(&test_file);
    }
}

fn run_single_test(test_file: &Path) {
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
    
    // Read the test file content
    let test_content = fs::read_to_string(test_file)
        .unwrap_or_else(|e| panic!("Failed to read test file {}: {}", test_file.display(), e));
    
    // Execute the orbit code directly using the library
    let result = execute_code_with_output(&test_content);
    
    // Check if execution was successful
    let actual_output = match result {
        Ok(values) => values.iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join("\n"),
        Err(e) => panic!(
            "Test {} failed with error: {}",
            test_name,
            e
        ),
    };
    
    // Compare actual output with expected output
    assert_eq!(
        actual_output.trim(), 
        expected_output,
        "Test {} failed.\nExpected: '{}'\nActual: '{}'",
        test_name,
        expected_output,
        actual_output.trim()
    );
    
    println!("✓ Test {} passed", test_name);
}