use std::fs;
use std::path::Path;

use orbit::execute_statements;

#[test]
fn test_statement_files() {
    let testcase_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("testcase")
        .join("statement");

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

    assert!(
        !test_files.is_empty(),
        "No .ob test files found in tests/testcase/statement directory"
    );

    for test_file in test_files {
        run_single_statement_test(&test_file);
    }
}

fn run_single_statement_test(test_file: &Path) {
    let test_name = test_file.file_stem().unwrap().to_str().unwrap();
    println!("Running statement test: {}", test_name);

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

    // Execute the statements and collect all output values
    let result = execute_statements(&test_content);

    let actual_output = match result {
        Ok(values) => values
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join("\n"),
        Err(e) => panic!("Test {} failed with error: {}", test_name, e),
    };

    assert_eq!(
        actual_output.trim(),
        expected_output,
        "Test {} failed.\nExpected: '{}'\nActual: '{}'",
        test_name,
        expected_output,
        actual_output.trim()
    );

    println!("✓ Statement test {} passed", test_name);
}
