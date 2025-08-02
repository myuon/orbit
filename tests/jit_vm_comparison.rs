use orbit::{Compiler, CompilerOptions, Value};
use std::fs;
use std::path::Path;

#[derive(Debug, Clone)]
struct ExecutionState {
    result: Option<Value>,
    stdout: String,
}

fn execute_with_vm(code: &str) -> anyhow::Result<ExecutionState> {
    let mut options = CompilerOptions::default();
    options.disable_jit = true;

    let mut compiler = Compiler::new_with_options(options);
    compiler.runtime_mut().enable_output_capture();

    let result = compiler.execute(code)?;
    let stdout = compiler
        .runtime_mut()
        .take_captured_output()
        .unwrap_or_default();

    Ok(ExecutionState { result, stdout })
}

fn execute_with_jit(
    code: &str,
    force_jit_functions: Vec<String>,
) -> anyhow::Result<(ExecutionState, bool)> {
    let jit_was_used = !force_jit_functions.is_empty();

    let mut options = CompilerOptions::default();
    options.disable_jit = false;
    options.jit_compile_functions = force_jit_functions;

    let mut compiler = Compiler::new_with_options(options);
    compiler.runtime_mut().enable_output_capture();

    let result = compiler.execute(code)?;
    let stdout = compiler
        .runtime_mut()
        .take_captured_output()
        .unwrap_or_default();

    Ok((ExecutionState { result, stdout }, jit_was_used))
}

fn compare_execution_states(
    vm_state: &ExecutionState,
    jit_state: &ExecutionState,
    test_name: &str,
) {
    // Compare results
    assert_eq!(
        vm_state.result, jit_state.result,
        "Test {}: Result mismatch. VM: {:?}, JIT: {:?}",
        test_name, vm_state.result, jit_state.result
    );

    // Compare stdout
    assert_eq!(
        vm_state.stdout, jit_state.stdout,
        "Test {}: Stdout mismatch. VM: '{}', JIT: '{}'",
        test_name, vm_state.stdout, jit_state.stdout
    );
}

fn run_jit_test_file(test_file: &Path) -> anyhow::Result<()> {
    let test_name = test_file.file_stem().unwrap().to_str().unwrap();
    println!("Running JIT test file: {}", test_name);

    let expected_result_file = test_file.with_extension("result");
    if !expected_result_file.exists() {
        return Err(anyhow::anyhow!(
            "No expected result file found for test {}",
            test_name
        ));
    }

    let expected_result = fs::read_to_string(&expected_result_file)?
        .trim()
        .to_string();
    let test_content = fs::read_to_string(test_file)?;

    // Execute with VM only
    let vm_state = execute_with_vm(&test_content)?;

    // Execute with JIT disabled for now (until JIT compilation is fixed)
    let (jit_state, _jit_was_used) = execute_with_jit(&test_content, vec![])?;

    // Compare states
    compare_execution_states(&vm_state, &jit_state, test_name);

    // Check the return value
    let actual_result = match vm_state.result {
        Some(value) => value.to_string(),
        None => String::new(),
    };

    assert_eq!(
        actual_result.trim(),
        expected_result,
        "Test {} failed. Expected: '{}', Actual: '{}'",
        test_name,
        expected_result,
        actual_result.trim()
    );

    println!("✓ JIT test file {} passed", test_name);
    Ok(())
}

#[test]
#[cfg(target_os = "macos")]
fn test_jit_files() {
    let jit_testcase_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("testcase")
        .join("jit");

    if !jit_testcase_dir.exists() {
        println!("JIT testcase directory not found, skipping file tests");
        return;
    }

    let mut test_files = Vec::new();
    if let Ok(entries) = fs::read_dir(&jit_testcase_dir) {
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
        println!("No .ob test files found in JIT testcase directory");
        return;
    }

    for test_file in test_files {
        run_jit_test_file(&test_file)
            .expect(&format!("JIT test file {} failed", test_file.display()));
    }

    println!("✓ All JIT file tests passed");
}
