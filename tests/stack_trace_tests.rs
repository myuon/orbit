use std::fs;
use std::path::Path;

use orbit::Compiler;

#[test]
fn test_stack_trace_files() {
    let testcase_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("stack_traces");

    if !testcase_dir.exists() {
        // No stack trace tests to run
        return;
    }

    let mut test_files = Vec::new();
    if let Ok(entries) = fs::read_dir(&testcase_dir) {
        for entry in entries {
            if let Ok(entry) = entry {
                let path = entry.path();
                if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("ob") {
                    test_files.push(path);
                }
            }
        }
    }

    if test_files.is_empty() {
        // No stack trace test files found
        return;
    }

    for test_file in test_files {
        run_single_stack_trace_test(&test_file);
    }
}

fn run_single_stack_trace_test(test_file: &Path) {
    let test_name = test_file.file_stem().unwrap().to_str().unwrap();
    println!("Running stack trace test: {}", test_name);

    let expected_stacks_file = test_file.with_extension("stacks");

    let test_content = fs::read_to_string(test_file)
        .unwrap_or_else(|e| panic!("Failed to read test file {}: {}", test_file.display(), e));

    if !expected_stacks_file.exists() {
        panic!(
            "Expected stack trace file not found: {}",
            expected_stacks_file.display()
        );
    }

    let expected_stacks = fs::read_to_string(&expected_stacks_file).unwrap_or_else(|e| {
        panic!(
            "Failed to read expected stack trace file {}: {}",
            expected_stacks_file.display(),
            e
        )
    });

    // Execute code step by step and capture stack states
    let captured_output = execute_step_by_step(&test_content)
        .unwrap_or_else(|e| panic!("Stack trace test {} failed with error: {}", test_name, e));

    // Normalize the captured output by removing trailing whitespace and standardizing line endings
    let actual_stacks = normalize_stack_output(&captured_output);
    let expected_stacks = normalize_stack_output(&expected_stacks);

    assert_eq!(
        actual_stacks, expected_stacks,
        "Stack trace test {} failed.\nExpected stacks:\n{}\nActual stacks:\n{}",
        test_name, expected_stacks, actual_stacks
    );

    println!("✓ Stack trace test {} passed", test_name);
}

/// Execute code step by step and capture stack trace information
fn execute_step_by_step(code: &str) -> Result<String, Box<dyn std::error::Error>> {
    use orbit::vm::{ControlFlow, VM};

    // Use the Compiler to handle the full compilation pipeline
    let mut compiler = Compiler::new();

    // Compile the code to VM instructions using the new method
    let instructions = compiler.compile_to_instructions(code)?;

    // Prepare VM for step-by-step execution
    let mut vm = VM::new();
    vm.reset();
    vm.load_program(instructions);

    let mut output = String::new();

    // Execute step by step and capture stack states
    loop {
        // Get current stack state before executing the instruction
        let stack = vm.get_stack();

        // Format and record only the stack state
        let stack_str = stack
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        output.push_str(&format!("[{}]\n", stack_str));

        // Execute one step
        let step_result = vm.step();

        match step_result {
            Ok(ControlFlow::Continue) => {
                // Continue execution
            }
            Ok(ControlFlow::Exit(_)) => {
                // Program finished
                break;
            }
            Err(err) => {
                return Err(Box::new(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("VM execution error: {}", err),
                )));
            }
        }
    }

    Ok(output)
}

/// Normalize stack output for comparison by trimming whitespace and standardizing format
fn normalize_stack_output(output: &str) -> String {
    output
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .collect::<Vec<&str>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_stack_output() {
        let input = "  Stack: [1, 2, 3]  \n  \n  Stack: [4, 5]  \n";
        let expected = "Stack: [1, 2, 3]\nStack: [4, 5]";
        assert_eq!(normalize_stack_output(input), expected);
    }
}
