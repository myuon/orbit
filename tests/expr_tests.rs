use std::fs;
use std::path::Path;

use orbit::execute_expression;
use orbit::Runtime;

#[test]
fn test_expr_files() {
    let testcase_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("testcase")
        .join("expr");

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
        "No .ob test files found in tests/testcase/expr directory"
    );

    for test_file in test_files {
        run_single_expr_test(&test_file);
    }
}

fn run_single_expr_test(test_file: &Path) {
    let test_name = test_file.file_stem().unwrap().to_str().unwrap();
    println!("Running expr test: {}", test_name);

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

    // For expressions, we execute each line separately
    let mut runtime = Runtime::new();
    let mut actual_outputs = Vec::new();

    for line in test_content.lines() {
        let line = line.trim();
        if !line.is_empty() {
            match execute_expression(line, &mut runtime) {
                Ok(value) => actual_outputs.push(value.to_string()),
                Err(e) => panic!(
                    "Test {} failed with error on line '{}': {}",
                    test_name, line, e
                ),
            }
        }
    }

    let actual_output = actual_outputs.join("\n");

    assert_eq!(
        actual_output.trim(),
        expected_output,
        "Test {} failed.\nExpected: '{}'\nActual: '{}'",
        test_name,
        expected_output,
        actual_output.trim()
    );

    println!("✓ Expr test {} passed", test_name);
}
