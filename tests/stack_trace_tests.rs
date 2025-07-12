use std::fs;
use std::path::Path;

use orbit::runtime::{ControlFlow, VM};
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

    // Parse both expected and actual outputs
    let expected_entries = parse_stack_trace_output(&expected_stacks);
    let actual_entries = parse_stack_trace_output(&captured_output);

    for (i, (expected, actual)) in expected_entries
        .iter()
        .zip(actual_entries.iter())
        .enumerate()
    {
        if expected.pc != actual.pc {
            panic!(
                "Stack trace test {} failed at line {}.\nExpected PC: {}\nActual PC: {}",
                test_name,
                i + 1,
                expected.pc,
                actual.pc
            );
        }
        if expected.instruction != actual.instruction {
            panic!(
                "Stack trace test {} failed at line {}.\nExpected instruction: '{}'\nActual instruction: '{}'",
                test_name, i + 1, expected.instruction, actual.instruction
            );
        }
        if expected.stack != actual.stack {
            panic!(
                "Stack trace test {} failed at line {}.\nExpected stack: {:?}\nActual stack: {:?}",
                test_name,
                i + 1,
                expected.stack,
                actual.stack
            );
        }
    }

    println!("✓ Stack trace test {} passed", test_name);
}

/// Execute code step by step and capture stack trace information
fn execute_step_by_step(code: &str) -> Result<String, Box<dyn std::error::Error>> {
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
        // Get current state before executing the instruction
        let pc = vm.get_program_counter();
        let instruction = vm.get_current_instruction().cloned();

        // Execute one step
        let step_result = vm.step();

        // Get stack state after executing the instruction
        let stack = vm.get_stack();

        // Format and record the current state (PC, instruction, and stack after execution)
        if let Some(instr) = instruction {
            let stack_str = stack
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<_>>()
                .join(", ");

            output.push_str(&format!("{:04} {:20} [{}]\n", pc, instr, stack_str));
        }

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

/// Represents a single stack trace entry
#[derive(Debug, Clone, PartialEq)]
struct StackTraceEntry {
    pc: usize,
    instruction: String,
    stack: Vec<String>,
}

/// Parse stack trace output into structured entries
fn parse_stack_trace_output(output: &str) -> Vec<StackTraceEntry> {
    let mut entries = Vec::new();

    for line in output.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        // Parse format: "0000 instruction [stack_items]"
        if let Some(entry) = parse_stack_trace_line(line) {
            entries.push(entry);
        }
    }

    entries
}

/// Parse a single stack trace line
fn parse_stack_trace_line(line: &str) -> Option<StackTraceEntry> {
    // Find the PC (first 4 digits)
    if line.len() < 4 {
        return None;
    }

    let pc_str = &line[0..4];
    let pc = pc_str.parse::<usize>().ok()?;

    // Find the stack part (starts with '[' and ends with ']')
    let stack_start = line.find('[')?;
    let stack_end = line.rfind(']')?;

    if stack_start >= stack_end {
        return None;
    }

    // Extract instruction (between PC and stack)
    let instruction_part = &line[4..stack_start].trim();
    let instruction = instruction_part.to_string();

    // Extract and parse stack
    let stack_str = &line[stack_start + 1..stack_end];
    let stack = if stack_str.trim().is_empty() {
        Vec::new()
    } else {
        stack_str.split(',').map(|s| s.trim().to_string()).collect()
    };

    Some(StackTraceEntry {
        pc,
        instruction,
        stack,
    })
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

    #[test]
    fn test_parse_stack_trace_line() {
        let line = "0000 push 5               []";
        let entry = parse_stack_trace_line(line).unwrap();
        assert_eq!(entry.pc, 0);
        assert_eq!(entry.instruction, "push 5");
        assert_eq!(entry.stack, Vec::<String>::new());

        let line2 = "0025 get_global y         [-1, 0, -1, 5]";
        let entry2 = parse_stack_trace_line(line2).unwrap();
        assert_eq!(entry2.pc, 25);
        assert_eq!(entry2.instruction, "get_global y");
        assert_eq!(entry2.stack, vec!["-1", "0", "-1", "5"]);
    }

    #[test]
    fn test_parse_stack_trace_output() {
        let input = "0000 push 5               []\n0001 set_global x         [5]\n";
        let entries = parse_stack_trace_output(input);
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].pc, 0);
        assert_eq!(entries[0].instruction, "push 5");
        assert_eq!(entries[1].pc, 1);
        assert_eq!(entries[1].instruction, "set_global x");
        assert_eq!(entries[1].stack, vec!["5"]);
    }
}
