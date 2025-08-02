use std::fs;
use std::path::Path;

use orbit::{execute_code, execute_code_with_output};

#[derive(Debug)]
enum TestResult {
    Passed,
    Failed(String),
}

struct TestSummary {
    name: String,
    result: TestResult,
}

#[test]
fn test_program_files() {
    let testcase_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("testcase");

    let mut all_results = Vec::new();

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
        let result = run_single_program_test_safe(&test_file);
        all_results.push(result);
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
            let result = run_single_error_test_safe(&test_file);
            all_results.push(result);
        }
    }

    // Print summary
    let passed_count = all_results
        .iter()
        .filter(|r| matches!(r.result, TestResult::Passed))
        .count();
    let failed_count = all_results.len() - passed_count;

    println!("\n=== Test Summary ===");
    println!(
        "Total: {}, Passed: {}, Failed: {}",
        all_results.len(),
        passed_count,
        failed_count
    );

    if failed_count > 0 {
        println!("\nFailed tests:");
        for summary in &all_results {
            if let TestResult::Failed(error) = &summary.result {
                println!("  ✗ {}: {}", summary.name, error);
            }
        }

        panic!("Some tests failed. See details above.");
    } else {
        println!("All tests passed!");
    }
}

fn run_single_program_test_safe(test_file: &Path) -> TestSummary {
    let test_name = test_file.file_stem().unwrap().to_str().unwrap().to_string();
    println!("Running program test: {}", test_name);

    let result = std::panic::catch_unwind(|| {
        run_single_program_test(test_file);
    });

    match result {
        Ok(_) => TestSummary {
            name: test_name.clone(),
            result: TestResult::Passed,
        },
        Err(panic_info) => {
            let error_message = if let Some(s) = panic_info.downcast_ref::<&str>() {
                s.to_string()
            } else if let Some(s) = panic_info.downcast_ref::<String>() {
                s.clone()
            } else {
                "Unknown panic".to_string()
            };
            TestSummary {
                name: test_name.clone(),
                result: TestResult::Failed(error_message),
            }
        }
    }
}

fn run_single_program_test(test_file: &Path) {
    let test_name = test_file.file_stem().unwrap().to_str().unwrap();
    println!("Running program test: {}", test_name);

    let expected_result_file = test_file.with_extension("result");
    let expected_stdout_file = test_file.with_extension("stdout");

    // Check if we have a .result file (new format) or fallback to .stdout (old format)
    let (expected_result, has_result_file) = if expected_result_file.exists() {
        let content = fs::read_to_string(&expected_result_file)
            .unwrap_or_else(|e| {
                panic!(
                    "Failed to read expected result file {}: {}",
                    expected_result_file.display(),
                    e
                )
            })
            .trim()
            .to_string();
        (content, true)
    } else if expected_stdout_file.exists() {
        // Fallback to old format for backward compatibility
        let content = fs::read_to_string(&expected_stdout_file)
            .unwrap_or_else(|e| {
                panic!(
                    "Failed to read expected stdout file {}: {}",
                    expected_stdout_file.display(),
                    e
                )
            })
            .trim()
            .to_string();
        (content, false)
    } else {
        panic!(
            "No expected output file found for test {}: neither {} nor {} exists",
            test_name,
            expected_result_file.display(),
            expected_stdout_file.display()
        );
    };

    let expected_stdout = if has_result_file && expected_stdout_file.exists() {
        Some(
            fs::read_to_string(&expected_stdout_file)
                .unwrap_or_else(|e| {
                    panic!(
                        "Failed to read expected stdout file {}: {}",
                        expected_stdout_file.display(),
                        e
                    )
                })
                .trim()
                .to_string(),
        )
    } else {
        None
    };

    let test_content = fs::read_to_string(test_file)
        .unwrap_or_else(|e| panic!("Failed to read test file {}: {}", test_file.display(), e));

    // Execute the orbit code with output capture
    let (result, captured_stdout) = execute_code_with_output(&test_content)
        .unwrap_or_else(|e| panic!("Test {} failed with error: {}", test_name, e));

    // Check the return value
    let actual_result = match result {
        Some(value) => value.to_string(),
        None => String::new(),
    };

    assert_eq!(
        actual_result.trim(),
        expected_result,
        "Test {} failed (return value).\nExpected result: '{}'\nActual result: '{}'",
        test_name,
        expected_result,
        actual_result.trim()
    );

    // Check stdout if we have expected stdout
    if let Some(expected_stdout) = expected_stdout {
        assert_eq!(
            captured_stdout.trim(),
            expected_stdout,
            "Test {} failed (stdout).\nExpected stdout: '{}'\nActual stdout: '{}'",
            test_name,
            expected_stdout,
            captured_stdout.trim()
        );
    }

    println!("✓ Program test {} passed", test_name);
}

fn run_single_error_test_safe(test_file: &Path) -> TestSummary {
    let test_name = test_file.file_stem().unwrap().to_str().unwrap().to_string();
    println!("Running error test: {}", test_name);

    let result = std::panic::catch_unwind(|| {
        run_single_error_test(test_file);
    });

    match result {
        Ok(_) => TestSummary {
            name: format!("{} (error)", test_name),
            result: TestResult::Passed,
        },
        Err(panic_info) => {
            let error_message = if let Some(s) = panic_info.downcast_ref::<&str>() {
                s.to_string()
            } else if let Some(s) = panic_info.downcast_ref::<String>() {
                s.clone()
            } else {
                "Unknown panic".to_string()
            };
            TestSummary {
                name: format!("{} (error)", test_name),
                result: TestResult::Failed(error_message),
            }
        }
    }
}

fn run_single_error_test(test_file: &Path) {
    let test_name = test_file.file_stem().unwrap().to_str().unwrap();
    println!("Running error test: {}", test_name);

    // Collect all .stderr files (.stderr, .stderr.2, .stderr.3, etc.)
    let mut expected_error_files = Vec::new();

    // Check for primary .stderr file
    let primary_stderr = test_file.with_extension("stderr");
    if primary_stderr.exists() {
        expected_error_files.push(primary_stderr);
    }

    // Check for additional .stderr.N files
    let mut counter = 2;
    loop {
        let stderr_file = test_file.with_extension(&format!("stderr.{}", counter));
        if stderr_file.exists() {
            expected_error_files.push(stderr_file);
            counter += 1;
        } else {
            break;
        }
    }

    if expected_error_files.is_empty() {
        panic!(
            "No expected error file found for test: {} (looking for .stderr)",
            test_file.display()
        );
    }

    // Read all expected error fragments
    let mut expected_error_fragments = Vec::new();
    for error_file in &expected_error_files {
        let fragment = fs::read_to_string(error_file)
            .unwrap_or_else(|e| {
                panic!(
                    "Failed to read expected error file {}: {}",
                    error_file.display(),
                    e
                )
            })
            .trim()
            .to_string();
        expected_error_fragments.push(fragment);
    }

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
            // Check the full error chain for debugging
            let full_error = format!("{:?}", error);

            // All expected fragments must be present (AND condition)
            for (i, expected_fragment) in expected_error_fragments.iter().enumerate() {
                if !error_message.contains(expected_fragment)
                    && !full_error.contains(expected_fragment)
                {
                    panic!(
                        "Error test {} failed.\nExpected error to contain fragment #{}: '{}'\nActual error: '{}'\nFull error: '{}'",
                        test_name, i + 1, expected_fragment, error_message, full_error
                    );
                }
            }

            println!(
                "✓ Error test {} passed ({} conditions checked)",
                test_name,
                expected_error_fragments.len()
            );
        }
    }
}
