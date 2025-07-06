use std::fs;
use std::path::Path;

use orbit::execute_code;

#[test]
fn test_program_files() {
    let testcase_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("testcase");

    // Test success cases
    let mut success_test_files = Vec::new();
    if let Ok(entries) = fs::read_dir(&testcase_dir) {
        for entry in entries {
            if let Ok(entry) = entry {
                let path = entry.path();
                if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("ob") {
                    success_test_files.push(path);
                }
            }
        }
    }

    assert!(
        !success_test_files.is_empty(),
        "No .ob test files found in tests/testcase directory"
    );

    for test_file in success_test_files {
        run_single_program_test(&test_file);
    }

    // Test error cases
    let errors_dir = testcase_dir.join("errors");
    if errors_dir.exists() {
        let mut error_test_files = Vec::new();
        if let Ok(entries) = fs::read_dir(&errors_dir) {
            for entry in entries {
                if let Ok(entry) = entry {
                    let path = entry.path();
                    if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("ob") {
                        error_test_files.push(path);
                    }
                }
            }
        }

        for test_file in error_test_files {
            run_single_error_test(&test_file);
        }
    }
}

fn run_single_program_test(test_file: &Path) {
    let test_name = test_file.file_stem().unwrap().to_str().unwrap();
    println!("Running program test: {}", test_name);

    let expected_file = test_file.with_extension("stdout");

    if !expected_file.exists() {
        panic!(
            "Expected output file not found: {}",
            expected_file.display()
        );
    }

    let expected_output = fs::read_to_string(&expected_file)
        .unwrap_or_else(|e| {
            panic!(
                "Failed to read expected output file {}: {}",
                expected_file.display(),
                e
            )
        })
        .trim()
        .to_string();

    let test_content = fs::read_to_string(test_file)
        .unwrap_or_else(|e| panic!("Failed to read test file {}: {}", test_file.display(), e));

    // Execute the orbit code directly using the library
    let result = execute_code(&test_content);

    // Check if execution was successful
    let actual_output = match result {
        Ok(Some(value)) => value.to_string(),
        Ok(None) => String::new(),
        Err(e) => panic!("Test {} failed with error: {}", test_name, e),
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

    println!("✓ Program test {} passed", test_name);
}

fn run_single_error_test(test_file: &Path) {
    let test_name = test_file.file_stem().unwrap().to_str().unwrap();
    println!("Running error test: {}", test_name);

    let expected_error_file = test_file.with_extension("stderr");

    if !expected_error_file.exists() {
        panic!(
            "Expected error file not found: {}",
            expected_error_file.display()
        );
    }

    let expected_error_fragment = fs::read_to_string(&expected_error_file)
        .unwrap_or_else(|e| {
            panic!(
                "Failed to read expected error file {}: {}",
                expected_error_file.display(),
                e
            )
        })
        .trim()
        .to_string();

    let test_content = fs::read_to_string(test_file)
        .unwrap_or_else(|e| panic!("Failed to read test file {}: {}", test_file.display(), e));

    // Execute the orbit code directly using the library
    let result = execute_code(&test_content);

    // Check if execution failed as expected
    match result {
        Ok(value) => {
            panic!(
                "Error test {} unexpectedly succeeded with result: {:?}",
                test_name, value
            );
        }
        Err(error) => {
            let error_message = error.to_string();
            if !error_message.contains(&expected_error_fragment) {
                panic!(
                    "Error test {} failed.\nExpected error to contain: '{}'\nActual error: '{}'",
                    test_name, expected_error_fragment, error_message
                );
            }
        }
    }

    println!("✓ Error test {} passed", test_name);
}
